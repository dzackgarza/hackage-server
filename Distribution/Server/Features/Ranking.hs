{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards #-}

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

import qualified Data.HashMap.Strict as HashMap
import Data.Aeson
import qualified Data.Vector as Vector
import Data.Map as Map
import Data.Set as Set
import Data.Text as Text

import Data.List as L
import qualified Data.Text as T
import Control.Applicative (optional)
import Data.Maybe (isJust)
import Distribution.Text as DT

import Control.Monad.Reader

type VoteCount = Integer
type Name = String
type UniquePackageNames = Set PackageName
type VoteMap = Map String Integer

-- | Define the prototype for this feature
data RankingFeature = RankingFeature {
  {-votesNum :: Int-}
  rankingFeatureInterface :: HackageFeature

  -- May need to define a custom resource and branch it out, see TagsResource

  -- queryGetPackagesByVotes      :: MonadIO m => m [(PackageName, Int)],
  -- queryGetSinglePackageVotes   :: MonadIO m => PackageName -> m voteTally
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

    performInitProcedure = do
      pkgIndex <- queryGetPackageIndex
      let pkgs = PackageIndex.allPackagesByName pkgIndex
          list = [display . pkgName . pkgInfoId $ pkg | pkg <- L.map L.head pkgs]
          tups = L.zip list [0,0..]
          items = Map.fromList tups
      writeMemState votesCache items

    -- | Resources passed to featureResources in constructor
    getVoteResource = (resourceAt "/packages/vote") {
      resourceDesc = [(GET, "Get the number of votes a package has.")],
      resourceGet  = [("json", getVotesMap)]
    }


    modifyVoteResource = (resourceAt "/packages/vote/:key") {
      resourceDesc = [(PUT, "Get the number of votes a package has.")],
      resourceGet  = [("json", modifyVotesMap)]
    }

    modifyVotesMap dpath = do
      let theKey = maybe mzero return (L.lookup "key" dpath >>= fromReqURI)
      theMap <- readMemState votesCache
      let newMap = adjust (1 +) theKey theMap
      writeMemState votesCache newMap
      ok. toResponse $ toJSON $ Map.toList newMap


    -- | Trivial json response
    getVotesMap _ = do
        theMap <- readMemState votesCache
        let
          arr = Map.toList theMap

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
