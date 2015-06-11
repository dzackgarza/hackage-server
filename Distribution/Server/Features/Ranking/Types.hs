{-# LANGUAGE DeriveDataTypeable #-}

module Distribution.Server.Features.Ranking.Types
  ( Votes(..)
  , VoteMap
  , initialVotes
  , addStar
  , removeStar
  , getUsersWhoStarred
  , getNumberOfStarsFor
  , enumerate
  ) where

import Distribution.Package (PackageName(..))
import Distribution.Server.Users.Types (UserId(..))

import Data.Typeable
import Data.Map as Map
import Data.Set as Set

type VoteMap = Map String (Set UserId)

data Votes = Votes {
  extractMap:: !VoteMap
  } deriving (Typeable, Show, Eq)

initialVotes :: Votes
initialVotes = Votes Map.empty

addStar :: PackageName -> UserId -> Votes -> Votes
addStar pkgname uid v = Votes $
  adjust (Set.insert uid) pname somemap
    where
      pname = unPackageName pkgname
      vmap  = extractMap v
      somemap = if pname `Map.member` vmap
        then vmap
        else Map.insert pname Set.empty vmap


removeStar :: PackageName -> UserId -> Votes -> Votes
removeStar pkgname uid vmap = Votes $
  adjust (Set.delete uid) (unPackageName pkgname) (extractMap vmap)

getUsersWhoStarred :: PackageName -> Votes -> Set UserId
getUsersWhoStarred pkgname vmap =
  (extractMap vmap) Map.! (unPackageName pkgname)

getNumberOfStarsFor :: PackageName -> Votes -> Int
getNumberOfStarsFor pkgname vmap =
  Set.size (getUsersWhoStarred pkgname vmap)

enumerate :: Votes -> [(String, Set UserId)]
enumerate vmap = Map.toList (extractMap vmap)
