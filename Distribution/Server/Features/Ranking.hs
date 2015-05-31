{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

module Distribution.Server.Features.Ranking (
  RankingFeature(..),
  initRankingFeature,
  ) where

import Distribution.Server.Framework
import Distribution.Server.Framework.Templating

import Distribution.Server.Features.Core

import Distribution.Package
import Distribution.Server.Packages.Types

import Data.Map as Map
import Data.Set as Set


type VoteTally = Int
type UniquePackageNames = Set PackageName
type VoteMap = Map UniquePackageNames VoteTally

data RankingFeature = RankingFeature {
  rankingFeatureInterface :: HackageFeature

  -- getVoteResource :: Resource,
  -- May need to define a custom resource and branch it out, see TagsResource

  -- queryGetPackagesByVotes      :: MonadIO m => m [(PackageName, Int)],
  -- queryGetSinglePackageVotes   :: MonadIO m => PackageName -> m voteTally

}

instance IsHackageFeature RankingFeature where
  getFeatureInterface = rankingFeatureInterface -- rankingInterface -- #ToDo: Define rankingInterface.

-- initRankingFeature :: ServerEnv
-- --> IO ()
initRankingFeature :: ServerEnv
                   -> IO (IO RankingFeature)
initRankingFeature = error "initRankingFeature"

-- Returns the number of votes a single package has.
queryGetNumberOfVotes :: (voteMap -> PackageName) -> voteMap -> Int
queryGetNumberOfVotes = error "queryGetNumberofVotes"

-- Post an upvote to a package, increasing it's ranking by one.
-- (Each user should only be able to add one upvote to a given package)
putUpVote :: (voteMap -> PackageName) -> voteMap
putUpVote = error "putUpVote"

-- Revoke a previous upvote
-- (Should only work if a user has previously +1'd a package)
putRemoveVote :: (voteMap -> PackageName) -> voteMap
putRemoveVote = error "putRemoveVote"

-- Return a list of packages, sorted by votes (descending)
queryGetBestPackages :: voteMap -> [(PackageName, Int)]
queryGetBestPackages = error "queryGetBestPackages"

rankingFeature :: CoreFeature -> RankingFeature
rankingFeature c
  = RankingFeature{..}
  where
  rankingFeatureInterface = (emptyHackageFeature "ranking") {
      featureResources        = []
      , featureState          = []
      , featureCaches         = []
      , featurePostInit       = error "featurePostInit"
      }

-- searchFeatureInterface = (emptyHackageFeature "ranking") {
  --  featureResources    = []
  --, featureState        = []
  --, featureCaches       = [
  --      CacheComponent {
  --        cacheDesc       = "package ranking system",
  --        getCacheMemSize = memSize <$> readMemState searchEngineState
  --      }
  --    ]
  -- , featurePostInit = unknown -- #ToDo
--}
