{-# LANGUAGE RankNTypes, NamedFieldPuns, RecordWildCards, LambdaCase #-}

-- | Implements of ranking system for all packages based on
-- | stars supplied by users.
module Distribution.Server.Features.Ranking
  ( RankingFeature(..)
  , initRankingFeature
  ) where

import Distribution.Server.Features.Ranking.Types
  ( Stars(..)
  , initialStars
  , getNumberOfStarsFor
  , enumerate
  )

import Distribution.Server.Framework as F

import Distribution.Server.Features.Core
import Distribution.Server.Features.Users

import qualified Distribution.Server.Packages.Types as PT
import Distribution.Server.Users.Types (UserId(..))

import qualified Distribution.Server.Features.Ranking.State as RState
import qualified Distribution.Server.Features.Ranking.Types as RTypes
import qualified Distribution.Server.Features.Ranking.Render as Render

import Distribution.Package
{-import qualified Distribution.Server.Users.Users as Users-}
import qualified Distribution.Server.Packages.PackageIndex as PackageIndex

import Data.Aeson
import qualified Data.HashMap.Strict as HashMap

import Data.List as L
import Data.Map as Map
import Data.Set as Set
import Distribution.Text as DT
import qualified Data.Text as T
import qualified Text.XHtml.Strict as X

import Control.Arrow (first)


-- | Define the prototype for this feature
data RankingFeature = RankingFeature {
    rankingFeatureInterface :: HackageFeature
  , starHook                :: Hook (PackageName) ()
  , didUserStar             :: MonadIO m => PackageName -> UserId -> m Bool
  , pkgNumStars             :: MonadIO m => PackageName -> m Int
  , renderStarsHtml         :: PackageName -> ServerPartE (String, X.Html)
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
  initialStarsCache <- newMemStateWHNF initialStars
  dbStarsState      <- starsStateComponent serverStateDir
  updateStars       <- newHook

  return $ \coref@CoreFeature{..} userf@UserFeature{..} -> do
    let feature = rankingFeature
                  initialStarsCache
                  dbStarsState
                  coref userf
                  updateStars
    return feature

starsStateComponent :: FilePath -> IO (StateComponent AcidState Stars)
starsStateComponent stateDir = do
  st <- openLocalStateFrom (stateDir </> "db" </> "Stars") initialStars
  return StateComponent {
      stateDesc    = "Backing store for Map PackageName -> Users who starred it"
    , stateHandle  = st
    , getState     = query st RState.DbGetStars
    , putState     = F.update st . RState.DbReplaceStars
    , resetState   = starsStateComponent
    , backupState  = undefined -- \_ -> dumpBackup
    , restoreState = undefined -- restoreBackup
   }


-- | Default constructor for building this feature.
rankingFeature ::  MemState Stars             -- PackageName -> Set UserId
                -> StateComponent AcidState Stars
                -> CoreFeature                -- To get site package list
                -> UserFeature                -- To authenticate users
                -> Hook (PackageName) ()
                -> RankingFeature

rankingFeature  starsCache
                starsState
                CoreFeature { coreResource =
                  CoreResource {
                    packageInPath
                  , guardValidPackageName
                  }
                , queryGetPackageIndex
                }
                UserFeature{..}
                starHook
  = RankingFeature{..}
  where
    rankingFeatureInterface =
      (emptyHackageFeature "Package stars") {
        featureResources      = [ getEntireMapResource
                                , addStarResource
                                , removeStarResource
                                , allUsersWhoStarredResource
                                ]
      , featurePostInit       = performInitProcedure
      , featureState          = [abstractAcidStateComponent starsState]
      , featureCaches         = [
          CacheComponent {
            cacheDesc         = "Cached map of package names to stars",
            getCacheMemSize   = memSize <$> readMemState starsCache
          }
        ]
      }


    -- | Called in phase 2 of Feature.hs, since it requires the package
    -- | list to be populated in the Core feature.
    performInitProcedure = do
      -- Populate the Stars map with package names
      pkgIndex <- queryGetPackageIndex
      let pkgs        = PackageIndex.allPackagesByName pkgIndex
          namesList   = [display . pkgName . PT.pkgInfoId $
            pkg | pkg <- L.map L.head pkgs]
           -- For now, assume no packages have been starred at startup.
          namesTups   = L.zip namesList (repeat Set.empty)
          namesItems  = Map.fromList namesTups
          emptyStars  = Stars namesItems
      writeMemState starsCache emptyStars

      {-void $ updateState starsState $ RState.DbReplaceStars initialStars-}
      {-dbStarsMap <- queryState starsState $ RState.DbGetStars-}
      {-case Map.null (extractMap dbStarsMap) of-}
        {-True -> do-}
          {-putStrLn "Empty database found. Populating.."-}
          {-void $ updateState starsState $ RState.DbReplaceStars emptyStars-}
        {-False -> do-}
          {-putStrLn "Database found."-}


      -- Populate the map of userIDs -> packages they've starred.
      {-userlist <- Users.enumerateActiveUsers <$> queryGetUserDb-}
      {-let users = [ uid | (uid, _) <- userlist ]-}
          {-userTups = L.zip users (repeat [])-}
          {-userItems = Map.fromList userTups-}
      {-writeMemState userStarsCache userItems-}


    -- | Define resources for this feature's URIs

    -- GET: Get the entire map from package names -> # of stars as a JSON object.
    getEntireMapResource :: Resource
    getEntireMapResource =
      (resourceAt "/package/starstate") {
        resourceDesc  = [(GET, "Returns the entire database of package stars.")]
      , resourceGet   = [("json", getPackageStarMap)]
    }

    -- POST: Add a star to a single package (package name must be an exact match)
    -- GET: Get the number of stars a package currently has.
    -- (Dev Note: path must contain ':package' exactly to use packageInPath)
    addStarResource :: Resource
    addStarResource  =
      (resourceAt "/package/star/:package") {
        resourceDesc  = [ (GET, "Returns the number of stars a package has.")
                        , (POST, "Adds a star to this package.")
                        ]
      , resourceGet   = [("json", getPackageNumStars)]
      , resourcePost  = [("",     starPackage)]
    }

    -- POST: Remove a user's star from a package.
    removeStarResource :: Resource
    removeStarResource =
      (resourceAt "/package/unstar/:package") {
        resourceDesc  = [(POST, "Adds a star to this package.")]
      , resourcePost  = [("",     removeStar)]
    }

    allUsersWhoStarredResource :: Resource
    allUsersWhoStarredResource =
      (resourceAt "/package/allstars/:package") {
        resourceDesc  = [(GET, "Returns all of the users who have starred a package.")]
      , resourceGet   = [("",     getAllPackageStars)]
    }
    -- Get the entire map of userIDs -> packages they've starred as JSON
    {-getUserStarMapResource :: Resource-}
    --{-getUserStarMapResource = (resourceAt "/packages/userstars") {-}
      {-resourceDesc = [(PUT, "Get the state of the user->star map.")],-}
      {-resourceGet  = [("json", getUserStars)]-}
    {-}-}

    -- | Implementations of the how the above resources are handled.

    -- Add a star to :packageName (must match name exactly)
    -- (Must be authenticated as any known user.)
    starPackage :: DynamicPath -> ServerPartE Response
    starPackage dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      {-packageStarsMap <- readMemState starsCache-}
      {-let newStarMap = addStar pkgname userID packageStarsMap-}
      {-writeMemState starsCache newStarMap-}

      tryAuthenticated >>= \case
        Nothing ->
          ok . toResponse $
              "Error: You must log in to star a package."
        Just uid -> do
          updateState starsState $ RState.DbAddStar pkgname uid
          runHook_ starHook pkgname
          -- Redirect back to package page.
          seeOther previousPage (toResponse $ responseMsg)
          where
            previousPage = "/package/" ++ unPackageName pkgname
            responseMsg = "Package \"" ++ unPackageName pkgname ++ "\" "
              ++ "starred successfully by: " ++ (show uid)

    -- Removes a user's star from a package. If the user has not
    -- not starred this package, does nothing.
    -- (Must be authenticated as any known user.)
    removeStar :: DynamicPath -> ServerPartE Response
    removeStar dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      tryAuthenticated >>= \case
        Nothing ->
          ok . toResponse $
              "Error: You must log in to unstar a package."
        Just uid -> do
          updateState starsState $ RState.DbRemoveStar pkgname uid
          runHook_ starHook pkgname
          -- Redirect back to package page.
          seeOther previousPage (toResponse $ responseMsg)
          where
            previousPage = "/package/" ++ unPackageName pkgname
            responseMsg = "Package \"" ++ unPackageName pkgname ++ "\" "
              ++ "unstarred successfully by: " ++ (show uid)

    -- Retrive the entire map (from package names -> # of stars)
    -- (Must be authenticated as an admin.)
    getPackageStarMap :: DynamicPath -> ServerPartE Response
    getPackageStarMap _ = do
      guardAuthorised [InGroup adminGroup]
      {-memoryStarsMap <- readMemState starsCache-}
      dbStarsMap <- queryState starsState RState.DbGetStars
      ok. toResponse $ toJSON $ enumerate dbStarsMap

    -- Get the number of stars a package has. If the package
    -- has never been starred, returns 0.
    getPackageNumStars :: DynamicPath -> ServerPartE Response
    getPackageNumStars dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      {-memoryStarsMap <- readMemState starsCache-}
      dbStarsMap <- queryState starsState RState.DbGetStars
      let numStars = getNumberOfStarsFor pkgname dbStarsMap
          arr = objectL
                  [ ("packageName", string $ unPackageName pkgname)
                  , ("numStars",    toJSON numStars)
                  ]

      ok . toResponse $ toJSON arr

    getAllPackageStars :: DynamicPath -> ServerPartE Response
    getAllPackageStars dpath = do
      pkgname <- packageInPath dpath
      guardValidPackageName pkgname

      dbStarsMap <- queryState starsState RState.DbGetStars

      let arr = Set.toList $ (extractMap dbStarsMap) Map.! (unPackageName pkgname)

      ok . toResponse $ toJSON arr

    -- Get the map of user names to packages they've starred.
    -- (Admin/debug function)
    {-getUserStars :: DynamicPath -> ServerPartE Response-}
    {-getUserStars _ = do-}
        {-guardAuthorised [InGroup adminGroup]-}
        {-userStarsMap    <- readMemState userStarsCache-}
        {-let  arr = Map.toList userStarsMap-}
        {-ok. toResponse $ toJSON arr-}

    -- | Helper Functions (Used outside of responses, e.g. by other features.)

    -- Returns true if a user has previously starred the
    -- package in question.
    didUserStar :: MonadIO m => PackageName -> UserId -> m Bool
    didUserStar pkgname uid = do
      dbStarsMap <- queryState starsState RState.DbGetStars
      return $ RTypes.askUserStarred pkgname uid dbStarsMap

    -- Returns the number of stars a package has.
    pkgNumStars :: MonadIO m => PackageName -> m Int
    pkgNumStars pkgname =  do
      dbStarsMap <- queryState starsState RState.DbGetStars
      return $ getNumberOfStarsFor pkgname dbStarsMap

    -- Renders the HTML for the "Stars:" section on package pages.
    renderStarsHtml :: PackageName -> ServerPartE (String, X.Html)
    renderStarsHtml pkgname = do
      numStars <- pkgNumStars pkgname
      userid <- tryAuthenticated
      case userid of
        Just uid -> do
          alreadyStarred <- didUserStar pkgname uid
          return $
            Render.renderStarsAuthenticatedAs uid  numStars pkgname alreadyStarred
        Nothing ->
          return $
            Render.renderStarsAnon numStars pkgname



-- | Helper functions for constructing JSON responses.

-- Use to construct a list of tuples that can be toJSON'd
objectL :: [(String, Value)] -> Value
objectL = Object . HashMap.fromList . L.map (first T.pack)

-- Use inside an objectL to transform strings into json values
string :: String -> Value
string = String . T.pack
