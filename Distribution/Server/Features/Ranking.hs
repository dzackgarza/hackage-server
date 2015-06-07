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
   {-initialUserVotesCache <- newMemStateWHNF Map.empty        -- Tracks which packages a user has voted on-}
  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = rankingFeature
                  initialVoteCache -- initialUserVotesCache
                  coref userf
    return feature

-- | Default constructor for building a feature.
rankingFeature :: MemState (Map String (Set UserId))         -- Package -> # of Votes
                {--> MemState (Map UserId [String]) -- UserId -> [packageNames]-}
                -> CoreFeature               -- To get package list
                -> UserFeature               -- To authenticate users
                -> RankingFeature

rankingFeature packageVotesCache
              {-userVotesCache-}
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
          namesTups = L.zip namesList (repeat Set.empty) -- For now, assume no packages have been voted on at startup.
          namesItems = Map.fromList namesTups
      writeMemState packageVotesCache namesItems

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

      packageVotesMap <- readMemState packageVotesCache
      let pName = extractName pkgname
          newPackageVoteSet = adjust (Set.insert userID) pName packageVotesMap
      writeMemState packageVotesCache newPackageVoteSet

      {-let pName = extractName pkgname-}
          {-newPVMap = adjust (1 +) pName packageVotesMap-}
      {-writeMemState packageVotesCache newPVMap-}

      {-userVotesMap    <- readMemState userVotesCache-}
      {-let newUVMap = adjust (pName :) userID userVotesMap-}
      {-writeMemState userVotesCache newUVMap-}

      ok . toResponse $
        "Package \"" ++ pName ++ "\" "
        ++ "upvoted successfully by " ++ (show userID)

    -- Retrive the entire map (from package names to # of votes)
    -- (Admin/debug function)
    getAllPackageVotes :: DynamicPath -> ServerPartE Response
    getAllPackageVotes _ = do
        guardAuthorised [InGroup adminGroup]
        packageVotesMap <- readMemState packageVotesCache
        let  arr = Map.toList packageVotesMap
        ok. toResponse $ toJSON arr

    -- Get a single package's number of votes
    getPackageVotes :: DynamicPath -> ServerPartE Response
    getPackageVotes dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      packageVotesMap <- readMemState packageVotesCache
      let pName = extractName pkgname
          uids = packageVotesMap Map.! pName
          {-arr = Set.toList uids-}
          numVotes = Set.size uids
          arr = objectL
                  [ ("packageName", string $ pName)
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

-- Unbox the string representation of a package's name
extractName :: PackageName -> String
extractName (PackageName { unPackageName = n }) = n
