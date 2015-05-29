{-# LANGUAGE RankNTypes, NamedFieldPuns #-}

module Distribution.Server.Features.Ranking (

  ) where

import Distribution.Server.Framework

import Distribution.Server.Package.Types

import Data.Map as Map
import Data.Set as Set


type VoteTally = Int
type UniquePackageNames = Set PackageName
type VoteMap = Map UniquePackageNames VoteTally

data RankingFeature = RankingFeature {
  voteFeatureInterface :: HackageFeature,

  getVoteResource :: Resource,
  -- May need to define a custom resource and branch it out, see TagsResource

   queryGetPackagesByVotes      :: MonadIO m => [(PackageName, Int)],
   queryGetSinglePackageVotes   :: MonadIO m => PackageName -> m voteTally

}

instance IsHackageFeature RankingFeature where
  getFeatureInterface = rankingInterface -- #ToDo: Define rankingInterface.

-- Returns the number of votes a single package has.
queryGetNumberOfVotes :: (voteMap -> PackageName) -> voteMap -> Int

-- Post an upvote to a package, increasing it's ranking by one.
-- (Each user should only be able to add one upvote to a given package)
putUpVote :: (voteMap -> PackageName) -> voteMap

-- Revoke a previous upvote
-- (Should only work if a user has previously +1'd a package)
putRemoveVote :: (voteMap -> PackageName) -> voteMap

-- Return a list of packages, sorted by votes (descending)
queryGetBestPackages :: voteMap -> [(PackageName, Int)]



searchFeatureInterface = (emptyHackageFeature "ranking") {
    featureResources    = []
  , featureState        = []
  , featureCaches       = [
        CacheComponent {
          cacheDesc       = "package ranking system",
          getCacheMemSize = memSize <$> readMemState searchEngineState
        }
      ]
  , featurePostInit = unknown -- #ToDo
}
