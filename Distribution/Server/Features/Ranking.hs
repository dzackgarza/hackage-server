{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, LambdaCase #-}

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
import qualified Distribution.Server.Features.Ranking.Types as RTypes

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
import Control.Monad.Error
import qualified Text.XHtml.Strict as X

import Control.Monad.Reader
import Control.Arrow (first)
import Control.Monad.State.Class (get, put, modify)
import Control.Monad.Reader.Class (ask, asks)

import Distribution.Server.Framework.Error (ServerPartE)

-- | Define the prototype for this feature
data RankingFeature = RankingFeature {
    rankingFeatureInterface :: HackageFeature
  , packageNumberOfStars    :: MonadIO m => PackageName -> m Int
  , didUserStar :: MonadIO m => PackageName -> UserId -> m Bool
  , masterRenderStars :: PackageName -> ServerPartE (String, X.Html)

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
        featureResources      = [ getPackageStarMapResource
                                , packageStarResource
                                , packageUnStarResource
                                , packageAllPackageStarsResource
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


    -- | Define resources for this feature's URIs

    -- Get the entire map from package names -> # of votes as a JSON object.
    getPackageStarMapResource :: Resource
    getPackageStarMapResource =
      (resourceAt "/package/stars") {
        resourceDesc  = [(GET, "Returns the entire database of package votes.")]
      , resourceGet   = [("json", getPackageStarMap)]
    }

    -- Add a star to a single package (package name must be an exact match)
    -- or get the number of votes a package has.
    -- (Dev Note: path must contain ':package' exactly to use packageInPath)
    packageStarResource :: Resource
    packageStarResource  =
      (resourceAt "/package/star/:package") {
        resourceDesc  = [ (GET, "Returns the number of stars a package has.")
                        , (POST, "Adds a star to this package.")
                        ]
      , resourceGet   = [("json", getPackageNumStars)]
      , resourcePost  = [("",     starPackage)]
    }

    -- Remove a user's star from a package.
    packageUnStarResource :: Resource
    packageUnStarResource =
      (resourceAt "/package/unstar/:package") {
        resourceDesc  = [(POST, "Adds a star to this package.")]
      , resourcePost  = [("",     unStarPackage)]
    }

    packageAllPackageStarsResource :: Resource
    packageAllPackageStarsResource =
      (resourceAt "/package/allstars/:package") {
        resourceDesc  = [(GET, "Returns all of the users who have starred a package.")]
      , resourceGet   = [("",     getAllPackageStars)]
    }
    -- Get the entire map of userIDs -> packages they've upvoted as JSON
    {-getUserVoteMapResource :: Resource-}
    --{-getUserVoteMapResource = (resourceAt "/packages/uservotes") {-}
      {-resourceDesc = [(PUT, "Get the state of the user->vote map.")],-}
      {-resourceGet  = [("json", getUserVotes)]-}
    {-}-}

    -- | Implementations of the how the above resources are handled.

    -- Add a star to at :packageName (must match name exactly)
    starPackage :: DynamicPath -> ServerPartE Response
    starPackage dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      {-packageVotesMap <- readMemState votesCache-}
      {-let newVoteMap = addStar pkgname userID packageVotesMap-}
      {-writeMemState votesCache newVoteMap-}

      tryAuthenticated >>= \case
        Nothing -> ok . toResponse $
              "Error: You must log in to star a package."
        Just uid -> do
          updateState votesState $ RState.DbAddStar pkgname uid
          seeOther previousPage (toResponse $ responseMsg)
          where
            previousPage = "/package/" ++ unPackageName pkgname
            responseMsg = "Package \"" ++ unPackageName pkgname ++ "\" "
              ++ "starred successfully by: " ++ (show uid)

    -- Removes a user's star from a package. If the user has not
    -- not voted for this package, does nothing.
    unStarPackage :: DynamicPath -> ServerPartE Response
    unStarPackage dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      tryAuthenticated >>= \case
        Nothing -> ok . toResponse $
              "Error: You must log in to unstar a package."
        Just uid -> do
          updateState votesState $ RState.DbRemoveStar pkgname (uid)
          seeOther previousPage (toResponse $ responseMsg)
          where
            previousPage = "/package/" ++ unPackageName pkgname
            responseMsg = "Package \"" ++ unPackageName pkgname ++ "\" "
              ++ "unstarred successfully by: " ++ (show uid)

    -- Retrive the entire map (from package names to # of votes)
    -- (Admin/debug function)
    getPackageStarMap :: DynamicPath -> ServerPartE Response
    getPackageStarMap _ = do
      guardAuthorised [InGroup adminGroup]
      {-memoryVotesMap <- readMemState votesCache-}
      dbVotesMap <- queryState votesState RState.DbGetVotes
      ok. toResponse $ toJSON $ enumerate dbVotesMap

    -- Get a single package's number of votes. If package name is not
    -- in the map, returns 0 (as it has never been upvoted)
    getPackageNumStars :: DynamicPath -> ServerPartE Response
    getPackageNumStars dpath = do
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

    getAllPackageStars :: DynamicPath -> ServerPartE Response
    getAllPackageStars dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      dbVotesMap <- queryState votesState RState.DbGetVotes

      let arr = Set.toList $ (extractMap dbVotesMap) Map.! (unPackageName pkgname)
      ok . toResponse $ toJSON arr

    -- Get the map of user names to packages they've upvoted.
    -- (Admin/debug function)
    {-getUserVotes :: DynamicPath -> ServerPartE Response-}
    {-getUserVotes _ = do-}
        {-guardAuthorised [InGroup adminGroup]-}
        {-userVotesMap    <- readMemState userVotesCache-}
        {-let  arr = Map.toList userVotesMap-}
        {-ok. toResponse $ toJSON arr-}

    -- | Helper Functions (Used outside of responses, e.g. by other features.)

    -- Returns true if a user has previously starred the
    -- package in question.
    didUserStar :: MonadIO m => PackageName -> UserId -> m Bool
    didUserStar pkgname uid = do
      dbVotesMap <- queryState votesState RState.DbGetVotes
      return $ RTypes.askUserStarred pkgname uid dbVotesMap

    -- Returns the number of stars a package has.
    packageNumberOfStars :: MonadIO m => PackageName -> m Int
    packageNumberOfStars pkgname =  do
      dbVotesMap <- queryState votesState RState.DbGetVotes
      return $ getNumberOfStarsFor pkgname dbVotesMap

    masterRenderStars :: PackageName -> ServerPartE (String, X.Html)
    masterRenderStars pkgname = do
      numStars <- packageNumberOfStars pkgname
      userid <- tryAuthenticated
      {-return $ ( "Stars:", X.toHtml $ show numStars )-}
      case userid of
        Just uid -> do
          foo <- didUserStar pkgname uid
          return $ renderStarsLoggedIn numStars pkgname foo
        Nothing ->
          return $ renderStarsAnon numStars pkgname



-- | Helper functions for constructing JSON responses.

-- Use to construct a list of tuples that can be toJSON'd
objectL :: [(String, Value)] -> Value
objectL = Object . HashMap.fromList . L.map (first T.pack)

-- Use inside an objectL to transform strings into json values
string :: String -> Value
string = String . T.pack

renderStarsAnon :: Int -> PackageName -> (String, X.Html)
renderStarsAnon numStars _ =
  ( "Stars:", X.toHtml $ show numStars )

renderStarsLoggedIn :: Int -> PackageName -> Bool -> (String, X.Html)
renderStarsLoggedIn numStars pkgname voted =
  ("Stars:",
    show numStars
    X.+++ if voted then
      X.form  X.! [ X.action $    "unstar/" ++ unPackageName pkgname
              , X.method      "POST" ]
      X.<<
      X.input X.! [ X.thetype     "submit"
              , X.value       "Remove vote for this package"
              , X.theclass    "text-button" ]
    else
      X.form  X.! [ X.action $    "star/" ++ unPackageName pkgname
              , X.method      "POST" ]
      X.<<
      X.input X.! [ X.thetype     "submit"
              , X.value       "Vote for this package"
              , X.theclass    "text-button" ]
  )
