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
  initialVoteCache      <- newMemStateWHNF initialVotes -- Tracks number of votes per package
   {-initialUserVotesCache <- newMemStateWHNF Map.empty        -- Tracks which packages a user has voted on-}
  votesState <- votesStateComponent serverStateDir
  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = rankingFeature
                  initialVoteCache -- initialUserVotesCache
                  votesState
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

-- | Default constructor for building a feature.
rankingFeature :: MemState Votes         -- Package -> # of Votes
                {--> MemState (Map UserId [String]) -- UserId -> [packageNames]-}
                ->StateComponent AcidState Votes
                -> CoreFeature               -- To get package list
                -> UserFeature               -- To authenticate users
                -> RankingFeature

rankingFeature votesCache
              {-userVotesCache-}
              votesState
              CoreFeature{ coreResource=CoreResource{ packageInPath
                                                      , guardValidPackageName
                                                      , lookupPackageName
                                                      }
                          , queryGetPackageIndex
                          , updateArchiveIndexEntry
                          }
              UserFeature{..}
  = RankingFeature{..}
  where
    rankingFeatureInterface = (emptyHackageFeature "ranking") {
      featureResources        = [getAllPackageVotesResource
                                , votingResource
                                {-, getUserVoteMapResource-}
                                ]
      , featurePostInit       = performInitProcedure
      , featureState          = [abstractAcidStateComponent votesState]
      , featureCaches         = [
          CacheComponent {
            cacheDesc       = "package vote counts",
            getCacheMemSize = memSize <$> readMemState votesCache
          }
        ]
      }

    -- | Called in phase 2 of Feature.hs, since it requires the package
    -- | list to be populated in the Core feature.
    performInitProcedure = do

      -- Populate the map of package names -> # of votes
      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex
          namesList = [display . pkgName . PT.pkgInfoId $ pkg | pkg <- L.map L.head pkgs]
          namesTups = L.zip namesList (repeat Set.empty) -- For now, assume no packages have been voted on at startup.
          namesItems = Map.fromList namesTups
      writeMemState votesCache (Votes namesItems)

      -- Populate the map of userIDs -> packages they've upvoted
      {-userlist <- Users.enumerateActiveUsers <$> queryGetUserDb-}
      {-let users = [ uid | (uid, _) <- userlist ]-}
          {-userTups = L.zip users (repeat [])-}
          {-userItems = Map.fromList userTups-}
      {-writeMemState userVotesCache userItems-}

    -- | Resources passed to featureResources in rankingFeatureInterface

    -- Get the entire map from package names -> # of votes as JSON
    getAllPackageVotesResource :: Resource
    getAllPackageVotesResource = (resourceAt "/packages/packagevotes") {
      resourceDesc = [(GET, "Get the current state of the map from packages to votes")],
      resourceGet  = [("json", getAllPackageVotes)]
    }

    -- Upvote a single package (package name must be an exact match)
    -- or get the number of votes a package has.
    -- (Note: path must contain ':package' to use packageInPath)
    votingResource :: Resource
    votingResource  = (resourceAt "/packages/upvote/:package") {
      resourceDesc  = [ (GET, "get a package's number of votes")
                      , (PUT, "upvote a package")
                      ],
      resourceGet   = [("json", getPackageVotes)],
      resourcePut   = [("", upVotePackage)]
    }

    -- Get the entire map of userIDs -> packages they've upvoted as JSON
    {-getUserVoteMapResource :: Resource-}
    --{-getUserVoteMapResource = (resourceAt "/packages/uservotes") {-}
      {-resourceDesc = [(PUT, "Get the state of the user->vote map.")],-}
      {-resourceGet  = [("json", getUserVotes)]-}
    {-}-}

    -- | Implementations of the how the above resources are handled.

    -- Increment the vote at /packages/vote/:packageName (must match name exactly)
    upVotePackage :: DynamicPath -> ServerPartE Response
    upVotePackage dpath = do
      userID          <- guardAuthorised [AnyKnownUser]
      pkgname         <- packageInPath dpath
      guardValidPackageName pkgname

      packageVotesMap <- readMemState votesCache
      let newVoteMap = addStar pkgname userID packageVotesMap
      writeMemState votesCache newVoteMap

      {-let pName = extractName pkgname-}
          {-newPVMap = adjust (1 +) pName packageVotesMap-}
      {-writeMemState votesCache newPVMap-}

      {-userVotesMap    <- readMemState userVotesCache-}
      {-let newUVMap = adjust (pName :) userID userVotesMap-}
      {-writeMemState userVotesCache newUVMap-}

      ok . toResponse $
        "Package \"" ++ unPackageName pkgname ++ "\" "
        ++ "upvoted successfully by " ++ (show userID)

    -- Retrive the entire map (from package names to # of votes)
    -- (Admin/debug function)
    getAllPackageVotes :: DynamicPath -> ServerPartE Response
    getAllPackageVotes _ = do
        guardAuthorised [InGroup adminGroup]
        memoryVotesMap <- readMemState votesCache
        ok. toResponse $ toJSON $ enumerate memoryVotesMap

    -- Get a single package's number of votes. If package name is not
    -- in the map, returns 0 (as it has never been upvoted)
    getPackageVotes :: DynamicPath -> ServerPartE Response
    getPackageVotes dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      memoryVotesMap <- readMemState votesCache
      let numVotes = getNumberOfStarsFor pkgname memoryVotesMap
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
