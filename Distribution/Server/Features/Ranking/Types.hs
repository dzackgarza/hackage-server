{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Server.Features.Ranking.Types
  ( Stars(..)
  , StarMap
  , initialStars
  , addStar
  , removeStar
  , getUsersWhoStarred
  , getNumberOfStarsFor
  , askUserStarred
  , enumerate
  ) where

import Distribution.Package (PackageName(..))
import Distribution.Server.Users.Types (UserId(..))

import Data.Typeable
import Data.Map as Map
import Data.Set as Set

type StarMap = Map String (Set UserId)

data Stars = Stars {
  extractMap:: !StarMap
  } deriving (Typeable, Show, Eq)

initialStars :: Stars
initialStars = Stars Map.empty

addStar :: PackageName -> UserId -> Stars -> Stars
addStar pkgname uid v = Stars $
  adjust (Set.insert uid) pname somemap
    where
      pname = unPackageName pkgname
      vmap  = extractMap v
      somemap = if pname `Map.member` vmap
        then vmap
        else Map.insert pname Set.empty vmap


removeStar :: PackageName -> UserId -> Stars -> Stars
removeStar pkgname uid vmap = Stars $
  adjust (Set.delete uid) (unPackageName pkgname) (extractMap vmap)

getUsersWhoStarred :: PackageName -> Stars -> Set UserId
getUsersWhoStarred p v =
  case pkgname `Map.member` vmap of
    True -> vmap Map.! pkgname
    False -> Set.empty
  where
    vmap    = extractMap v
    pkgname = unPackageName p

askUserStarred :: PackageName -> UserId -> Stars -> Bool
askUserStarred  p uid v =
  case pkgname `Map.member` vmap of
    True -> uid `Set.member` (vmap Map.! pkgname)
    False -> False
  where
    vmap = extractMap v
    pkgname = unPackageName p

getNumberOfStarsFor :: PackageName -> Stars -> Int
getNumberOfStarsFor pkgname vmap =
  Set.size (getUsersWhoStarred pkgname vmap)

enumerate :: Stars -> [(String, Set UserId)]
enumerate vmap = Map.toList (extractMap vmap)
