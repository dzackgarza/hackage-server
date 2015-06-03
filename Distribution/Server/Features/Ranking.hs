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

import qualified Data.HashMap.Strict as HashMap
import Data.Aeson (Value(..), toJSON)
import qualified Data.Vector as Vector
import Data.Map as Map
import Data.Set as Set
import Data.Text as Text


type VoteTally = Int
type UniquePackageNames = Set PackageName
type VoteMap = Map UniquePackageNames VoteTally

-- | Define the prototype for this feature
data RankingFeature = RankingFeature {
  {-votesNum :: Int-}
  rankingFeatureInterface :: HackageFeature,

  getVoteResource :: Resource
  -- May need to define a custom resource and branch it out, see TagsResource

  -- queryGetPackagesByVotes      :: MonadIO m => m [(PackageName, Int)],
  -- queryGetSinglePackageVotes   :: MonadIO m => PackageName -> m voteTally
}

-- | Implement the isHackageFeature 'interface'
instance IsHackageFeature RankingFeature where
  getFeatureInterface = rankingFeatureInterface -- rankingInterface -- #ToDo: Define rankingInterface.

-- | Called from Features.hs to initialize this feature
initRankingFeature :: ServerEnv -> IO ( IO RankingFeature)
initRankingFeature ServerEnv{serverStateDir} = do
  return $ do
    let feature = rankingFeature
    return feature

-- | Default constructor for building a feature.
rankingFeature :: RankingFeature

rankingFeature
  = RankingFeature{..}
  where
    rankingFeatureInterface = (emptyHackageFeature "ranking") {
        featureResources        = [getVoteResource]
        , featureState          = []
        , featureCaches         = []
      }

    getVoteResource = (resourceAt "/packages/vote") {
          resourceDesc = [(GET, "Get the number of votes a package has.")],
          resourceGet  = [("json", returnOne)]
      }

-- | Trivial json response
returnOne :: DynamicPath -> ServerPartE Response
returnOne d = ok . toResponse $
            toJSON arr
              where arr = [ ("param1", 1 :: VoteTally)
                          , ("param2", 2 :: VoteTally)
                          ]

array :: [Value] -> Value
array = Array . Vector.fromList

object :: [(Text.Text, Value)] -> Value
object = Object . HashMap.fromList

string :: String -> Value
string = String . Text.pack

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
