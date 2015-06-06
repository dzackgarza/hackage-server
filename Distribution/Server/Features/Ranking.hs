{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

-- | Implements of ranking system for all packages based on
-- | upvotes supplied by users.
module Distribution.Server.Features.Ranking (
  RankingFeature(..),
  initRankingFeature
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import Distribution.Server.Packages.Types
import Distribution.Server.Users.Types (UserId(..), UserName(UserName))

import Distribution.Package
import qualified Distribution.Server.Users.Users as Users
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Data.Text as Text
import Data.Aeson
import Data.Map as Map
import Data.Set as Set
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap

import Data.List as L
import Data.Maybe (isJust)
import Distribution.Text as DT
import qualified Data.Text as T
import Control.Applicative (optional)

import Control.Monad.Reader
import Control.Arrow (first)

type VoteCount = Integer
type Name = String
type UniquePackageNames = Set PackageName
type VoteMap = Map String Integer

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

initRankingFeature ServerEnv{..} = do
  initialVoteCache      <- newMemStateWHNF Map.empty        -- Tracks number of votes per package
  initialUserVotesCache <- newMemStateWHNF Map.empty        -- Tracks which packages a user has voted on
  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = rankingFeature
                  initialVoteCache initialUserVotesCache
                  coref userf
    return feature

-- | Default constructor for building a feature.
rankingFeature :: MemState (Map String Integer)    -- Package -> # of Votes
                -> MemState (Map UserId [String]) -- UserId -> [packageNames]
                -> CoreFeature          -- To get package list
                -> UserFeature          -- To authenticate users
                -> RankingFeature

rankingFeature packageVotesCache
              userVotesCache
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
      featureResources        = [getPackageVotesResource
                                , putUpvoteResource
                                , getUserVoteMapResource
                                ]
      , featurePostInit       = performInitProcedure
      , featureState          = []
      , featureCaches         = [
          CacheComponent {
            cacheDesc       = "package vote counts",
            getCacheMemSize = memSize <$> readMemState packageVotesCache
          }
        ]
      }

    -- | Called in phase 2 of Feature.hs, since it requires the package
    -- | list to be populated in the Core feature.
    performInitProcedure = do

      -- Populate the map of package names -> # of votes
      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex
          namesList = [display . pkgName . pkgInfoId $ pkg | pkg <- L.map L.head pkgs]
          namesTups = L.zip namesList [0, 0..]     -- Initialize their votes to zero (for now)
          namesItems = Map.fromList namesTups
      writeMemState packageVotesCache namesItems

      -- Populate the map of userIDs -> packages they've upvoted
      userlist <- Users.enumerateActiveUsers <$> queryGetUserDb
      let users = [ uid | (uid, _) <- userlist ]
          userTups = L.zip users (repeat [])
          userItems = Map.fromList userTups
      writeMemState userVotesCache userItems

    -- | Resources passed to featureResources in rankingFeatureInterface

    -- Get the entire map from package names -> # of votes as JSON
    getPackageVotesResource :: Resource
    getPackageVotesResource = (resourceAt "/packages/packagevotes") {
      resourceDesc = [(GET, "Get the current state of the map from packages to votes")],
      resourceGet  = [("json", getPackageVotes)]
    }

    -- Upvote a single package (package name must be an exact match)
    -- (Note: path must contain ':package' to use packageInPath)
    putUpvoteResource :: Resource
    putUpvoteResource = (resourceAt "/packages/vote/:package") {
      resourceDesc = [(PUT, "Add a vote to a package.")],
      resourceGet  = [("json", upVotePackage)]
    }

    -- Get the entire map of userIDs -> packages they've upvoted as JSON
    getUserVoteMapResource :: Resource
    getUserVoteMapResource = (resourceAt "/packages/uservotes") {
      resourceDesc = [(PUT, "Get the state of the user->vote map.")],
      resourceGet  = [("json", getUserVotes)]
    }

    -- | Implementations of the how the above resources are handled.

    -- Increment the vote at /packages/vote/:packageName (must match name exactly)
    upVotePackage :: DynamicPath -> ServerPartE Response
    upVotePackage dpath = do
      userID          <- guardAuthorised [AnyKnownUser]
      pkgname         <- packageInPath dpath
      guardValidPackageName pkgname

      packageVotesMap <- readMemState packageVotesCache
      let pName = extractName pkgname
          newPVMap = adjust (1 +) pName packageVotesMap
      writeMemState packageVotesCache newPVMap

      userVotesMap    <- readMemState userVotesCache
      let newUVMap = adjust (pName :) userID userVotesMap
      writeMemState userVotesCache newUVMap

      ok . toResponse $
        "Package \"" ++ pName ++ "\" "
        ++ "upvoted successfully by " ++ (show userID)

    -- Retrive the entire map (from package names to # of votes)
    -- (Admin/debug function)
    getPackageVotes :: DynamicPath -> ServerPartE Response
    getPackageVotes _ = do
        guardAuthorised [InGroup adminGroup]
        packageVotesMap <- readMemState packageVotesCache
        let  arr = Map.toList packageVotesMap
        ok. toResponse $ toJSON arr

    -- Get the map of user names to packages they've upvoted.
    -- (Admin/debug function)
    getUserVotes :: DynamicPath -> ServerPartE Response
    getUserVotes _ = do
        guardAuthorised [InGroup adminGroup]
        userVotesMap    <- readMemState userVotesCache
        let  arr = Map.toList userVotesMap
        ok. toResponse $ toJSON arr

extractName :: PackageName -> String
extractName (PackageName { unPackageName = n }) = n
