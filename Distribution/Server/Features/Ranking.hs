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

import Distribution.Package
import Distribution.Server.Packages.Types
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
  getFeatureInterface = rankingFeatureInterface -- rankingInterface -- #ToDo: Define rankingInterface.

-- | Called from Features.hs to initialize this feature
initRankingFeature :: ServerEnv
                   -> IO ( CoreFeature
                      -> IO RankingFeature)

initRankingFeature ServerEnv{..} = do
  initialVoteCache <- newMemStateWHNF Map.empty
  return $ \core@CoreFeature{..} -> do
    let feature = rankingFeature initialVoteCache core
    return feature

-- | Default constructor for building a feature.
rankingFeature :: MemState (Map String Integer)
                -> CoreFeature
                -> RankingFeature

rankingFeature votesCache CoreFeature{..}
  = RankingFeature{..}
  where
    rankingFeatureInterface = (emptyHackageFeature "ranking") {
      featureResources        = [getVoteResource
                                , modifyVoteResource
                                ]
      , featurePostInit       = performInitProcedure
      , featureState          = []
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
      -- Populate the map with package names
      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex
          list = [display . pkgName . pkgInfoId $ pkg | pkg <- L.map L.head pkgs]
          tups = L.zip list [0,0..]     -- Initialize their votes to zero (for now)
          items = Map.fromList tups
      writeMemState votesCache items


    -- | Resources passed to featureResources in rankingFeatureInterface
    -- | Get the entire map
    getVoteResource = (resourceAt "/packages/vote") {
      resourceDesc = [(GET, "Get the current state of the map from names to votes")],
      resourceGet  = [("json", getVotesMap)]
    }

    -- | Upvote a single package (:packageName must be an exact match)
    modifyVoteResource = (resourceAt "/packages/vote/:packageName") {
      resourceDesc = [(PUT, "Add a vote to a package.")],
      resourceGet  = [("json", upVotePackage)]
    }


    -- | Implementations of the how the above resources are handled.
    -- | Increment the vote at /packages/vote/:packageName
    upVotePackage dpath = case fromReqURI =<< L.lookup "packageName" dpath of
      Nothing -> mzero
      Just pName -> do
        theMap <- readMemState votesCache
        let newMap = adjust (1 +) pName theMap
        writeMemState votesCache newMap
        if pName `Map.member` theMap
          then ok . toResponse $ toJSON $ Map.toList newMap
          else ok . toResponse $ "Not found: " ++ pName

    -- | Retrive the entire map (from package names to # of votes)
    getVotesMap _ = do
        theMap <- readMemState votesCache
        let  arr = Map.toList theMap
        ok. toResponse $ toJSON arr


-- | Returns the number of votes a single package has.
queryGetNumberOfVotes :: (voteMap -> PackageName) -> voteMap -> Int
queryGetNumberOfVotes = error "queryGetNumberofVotes"

-- | Post an upvote to a package, increasing it's ranking by one.
-- | (Each user should only be able to add one upvote to a given package)
putUpVote :: (voteMap -> PackageName) -> voteMap
putUpVote = error "putUpVote"

-- | Revoke a previous upvote
-- | (Should only work if a user has previously +1'd a package)
putRemoveVote :: (voteMap -> PackageName) -> voteMap
putRemoveVote = error "putRemoveVote"

-- | Return a list of packages, sorted by votes (descending)
queryGetBestPackages :: voteMap -> [(PackageName, Int)]
queryGetBestPackages = error "queryGetBestPackages"

-- | Add a new package to the global map
addNewPackage :: (voteMap -> PackageName) -> voteMap
addNewPackage = error "addNewPackage"
