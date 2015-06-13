{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements of ranking system for all packages based on
-- | upvotes/stars supplied by users.
module Distribution.Server.Features.Ranking
  ( RankingFeature(..)
  , initRankingFeature
  ) where

import Distribution.Server.Features.Ranking.Types
  ( Votes(..)
  , VoteMap
  , initialVotes
  , addStar
  , removeStar
  , getUsersWhoStarred
  , getNumberOfStarsFor
  , enumerate
  )

import Distribution.Server.Framework as F
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import qualified Distribution.Server.Packages.Types as PT
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))

import qualified Distribution.Server.Features.Ranking.State as RState

import Distribution.Package
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex
import qualified Distribution.Server.Framework.Auth as Auth

import Data.Aeson
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import Data.List as L
import Data.Maybe (isJust)
import Data.Map as Map
import Data.Set as Set
import Distribution.Text as DT
import qualified Data.Text as T
import Control.Applicative (optional)

import Control.Monad.Reader
import Control.Arrow (first)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask, asks)


-- | Define the prototype for this feature
data RankingFeature = RankingFeature {
    rankingFeatureInterface :: HackageFeature
  , packageNumberOfStars    :: MonadIO m => PackageName -> m Int
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature RankingFeature where
  getFeatureInterface = rankingFeatureInterface

-- | Called from Features.hs to initialize this feature
initRankingFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> UserFeature
                      -> IO RankingFeature)

initRankingFeature ServerEnv{serverStateDir} = do
  -- Tracks the map of package names to UserIds in memory
  initialVoteCache <- newMemStateWHNF initialVotes
  -- Persist the map to the database
  dbVotesState <- votesStateComponent serverStateDir
  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = rankingFeature
                  initialVoteCache
                  dbVotesState
                  coref userf
    return feature

votesStateComponent :: FilePath -> IO (StateComponent AcidState Votes)
votesStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Votes") initialVotes
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Votes"
    , stateHandle  = st
    , getState     = query st RState.DbGetVotes
    , putState     = F.update st . RState.DbReplaceVotes
    , resetState   = votesStateComponent
    {-, backupState  = \_ -> dumpBackup-}
    {-, restoreState = restoreBackup-}
   }

{-data MyError = NotLoggedInError | OtherError-}
  {-deriving Show-}

{-func :: ServerPartE UserId-}
{-func = do-}
  {-users <- queryGetUserDb-}
  {-uid   <- myguardAuthenticated users-}
  {-return (UserId 0)-}

{-myguardAuthenticated :: ServerMonad m => Users.Users -> m (Either MyError UserId)-}
{-myguardAuthenticated users = do-}
  {-(uid,_) <- Auth.checkAuthenticated Auth.hackageRealm users-}
  {-authres <- Auth.checkAuthenticated Auth.hackageRealm users-}
  {-return $ case authres of-}
    {-Left autherr  -> Left NotLoggedInError-}
    {-Right (uid, _) ->  Right uid-}

-- | Default constructor for building a feature.
rankingFeature ::  MemState Votes              -- PackageName -> Set UserId
                -> StateComponent AcidState Votes
                -> CoreFeature                -- To get site package list
                -> UserFeature                -- To authenticate users
                -> RankingFeature

rankingFeature  votesCache
                votesState
                CoreFeature { coreResource =
                  CoreResource {
                    packageInPath
                  , guardValidPackageName
                  }
                , queryGetPackageIndex
                }
                UserFeature{..}
  = RankingFeature{..}
  where
    rankingFeatureInterface =
      (emptyHackageFeature "ranking") {
        featureResources      = [ getAllPackageVotesResource
                                , votingResource
                                ]
      , featurePostInit       = performInitProcedure
      , featureState          = [abstractAcidStateComponent votesState]
      , featureCaches         = [
          CacheComponent {
            cacheDesc         = "package votes",
            getCacheMemSize   = memSize <$> readMemState votesCache
          }
        ]
      }

    packageNumberOfStars :: MonadIO m => PackageName -> m Int
    packageNumberOfStars pkgname =  do
      dbVotesMap <- queryState votesState RState.DbGetVotes
      return $ getNumberOfStarsFor pkgname dbVotesMap

    -- | Called in phase 2 of Feature.hs, since it requires the package
    -- | list to be populated in the Core feature.
    performInitProcedure = do
      -- Populate the Votes map with package names
      pkgIndex <- queryGetPackageIndex
      let pkgs        = PackageIndex.allPackagesByName pkgIndex
          namesList   = [display . pkgName . PT.pkgInfoId $
            pkg | pkg <- L.map L.head pkgs]
           -- For now, assume no packages have been voted on at startup.
          namesTups   = L.zip namesList (repeat Set.empty)
          namesItems  = Map.fromList namesTups
          emptyVotes  = Votes namesItems
      writeMemState votesCache emptyVotes

      {-void $ updateState votesState $ RState.DbReplaceVotes initialVotes-}
      {-dbVotesMap <- queryState votesState $ RState.DbGetVotes-}
      {-case Map.null (extractMap dbVotesMap) of-}
        {-True -> do-}
          {-putStrLn "Empty database found. Populating.."-}
          {-void $ updateState votesState $ RState.DbReplaceVotes emptyVotes-}
        {-False -> do-}
          {-putStrLn "Database found."-}


      -- Populate the map of userIDs -> packages they've upvoted
      {-userlist <- Users.enumerateActiveUsers <$> queryGetUserDb-}
      {-let users = [ uid | (uid, _) <- userlist ]-}
          {-userTups = L.zip users (repeat [])-}
          {-userItems = Map.fromList userTups-}
      {-writeMemState userVotesCache userItems-}


    -- | Define the endpoints for this feature's resources

    -- Get the entire map from package names -> # of votes as a JSON object.
    getAllPackageVotesResource :: Resource
    getAllPackageVotesResource =
      (resourceAt "/package/stars") {
        resourceDesc  = [(GET, "Returns the entire database of package votes.")]
      , resourceGet   = [("json", getAllPackageVotes)]
    }

    -- Add a star to a single package (package name must be an exact match)
    -- or get the number of votes a package has.
    -- (Dev Note: path must contain ':package' exactly to use packageInPath)
    votingResource :: Resource
    votingResource  =
      (resourceAt "/package/star/:package") {
        resourceDesc  = [ (GET, "Returns the number of stars a package has.")
                        , (POST, "Adds a star to this package.")
                        ]
      , resourceGet   = [("json", getPackageVotes)]
      , resourcePost  = [("", upVotePackage)]
    }

    -- Get the entire map of userIDs -> packages they've upvoted as JSON
    {-getUserVoteMapResource :: Resource-}
    --{-getUserVoteMapResource = (resourceAt "/packages/uservotes") {-}
      {-resourceDesc = [(PUT, "Get the state of the user->vote map.")],-}
      {-resourceGet  = [("json", getUserVotes)]-}
    {-}-}

    -- | Implementations of the how the above resources are handled.

    -- Add a star to at :packageName (must match name exactly)
    upVotePackage :: DynamicPath -> ServerPartE Response
    upVotePackage dpath = do
      userID          <- guardAuthorised [AnyKnownUser]
      pkgname         <- packageInPath dpath
      guardValidPackageName pkgname

      {-packageVotesMap <- readMemState votesCache-}
      {-let newVoteMap = addStar pkgname userID packageVotesMap-}
      {-writeMemState votesCache newVoteMap-}

      updateState votesState $ RState.DbAddStar pkgname userID
      ok . toResponse $
        "Package \"" ++ unPackageName pkgname ++ "\" "
        ++ "upvoted successfully by " ++ (show userID)

    -- Retrive the entire map (from package names to # of votes)
    -- (Admin/debug function)
    getAllPackageVotes :: DynamicPath -> ServerPartE Response
    getAllPackageVotes _ = do
      guardAuthorised [InGroup adminGroup]
      {-memoryVotesMap <- readMemState votesCache-}
      dbVotesMap <- queryState votesState RState.DbGetVotes
      ok. toResponse $ toJSON $ enumerate dbVotesMap

    -- Get a single package's number of votes. If package name is not
    -- in the map, returns 0 (as it has never been upvoted)
    getPackageVotes :: DynamicPath -> ServerPartE Response
    getPackageVotes dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      {-memoryVotesMap <- readMemState votesCache-}
      dbVotesMap <- queryState votesState RState.DbGetVotes
      let numVotes = getNumberOfStarsFor pkgname dbVotesMap
          arr = objectL
                  [ ("packageName", string $ unPackageName pkgname)
                  , ("numVotes",    toJSON numVotes)
                  ]

      ok . toResponse $ toJSON arr

    -- Get the map of user names to packages they've upvoted.
    -- (Admin/debug function)
    {-getUserVotes :: DynamicPath -> ServerPartE Response-}
    {-getUserVotes _ = do-}
        {-guardAuthorised [InGroup adminGroup]-}
        {-userVotesMap    <- readMemState userVotesCache-}
        {-let  arr = Map.toList userVotesMap-}
        {-ok. toResponse $ toJSON arr-}

-- Use to construct a list of tuples that can be toJSON'd
objectL :: [(String, Value)] -> Value
objectL = Object . HashMap.fromList . L.map (first T.pack)

-- Use inside an objectL to transform strings into json values
string :: String -> Value
string = String . T.pack
