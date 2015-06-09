{-# LANGUAGE DeriveDataTypeable, TypeFamilies, TemplateHaskell, RecordWildCards #-}

module Distribution.Server.Features.Ranking.State where

import Distribution.Package (PackageName)

import Distribution.Server.Features.Ranking.Types
  ( Votes(..)
  , VoteMap
  , initialVotes
  , addStar
  , removeStar
  , getUsersWhoStarred
  , getNumberOfStarsFor
  , enumerate
  )

import Distribution.Server.Users.Types (UserId)
import Distribution.Server.Users.State ()
import Distribution.Server.Framework.MemSize

import Data.Acid     (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Typeable

import Data.Maybe (fromMaybe)
import Control.Monad (liftM)
import qualified Control.Monad.State as State
import Control.Monad.Reader.Class (ask, asks)

deriveSafeCopy 0 'base ''Votes

instance MemSize Votes where
    memSize (Votes a) = memSize1 a

dbAddStar :: PackageName -> UserId -> Update Votes ()
dbAddStar pkgname uid = do
  state <- State.get
  State.put $ addStar pkgname uid state

dbRemoveStar :: PackageName -> UserId -> Update Votes ()
dbRemoveStar pkgName uid = do
  state <- State.get
  State.put $ removeStar pkgName uid state

dbGetVotes :: Query Votes Votes
dbGetVotes = ask

-- Replace the entire map
dbReplaceVotes :: Votes -> Update Votes ()
dbReplaceVotes = State.put

makeAcidic
  ''Votes
  [ 'dbAddStar
  , 'dbRemoveStar
  , 'dbGetVotes
  , 'dbReplaceVotes
  ]
