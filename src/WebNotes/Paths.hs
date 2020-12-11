{-# LANGUAGE RecordWildCards, AllowAmbiguousTypes #-}

module WebNotes.Paths 
  (
    Item,
    Source(..),
    Work(..),
    Output(..),
    saltFor,
    sourceExt,
    workExt,
    getFileName,
    toWork,
    toOutput,
    toOutputExt,
    toSourceFile,
    toWorkFile,
    toOutputFile,
    toPath,
    toRelPath
  )
where

import WebNotes.ConfigParser
import WebNotes.Utils

import System.FilePath
import Data.Hashable

newtype Item a = Item { unItem :: FilePath }
  deriving (Show, Eq)

data Source
data Work
data Output

instance Hashable (Item a) where
  hashWithSalt salt = hashWithSalt salt . unItem


saltFor :: Item Source -> Int
saltFor = hash

sourceExt :: Item Source -> Extension
sourceExt (Item src) = 
  let (_, _, ext) = explode src
   in ext

workExt :: Item Work -> Extension
workExt (Item src) = 
  let (_, _, ext) = explode src
   in ext

getFileName :: Item Source -> String
getFileName (Item src) = 
  let (_, file, _) = explode src
   in file

toSourceFile :: FilePath -> Item Source
toWorkFile :: FilePath -> Item Work
toOutputFile :: FilePath -> Item Output
toSourceFile = Item
toWorkFile = Item
toOutputFile = Item


class ItemType a where
  toPath :: Config -> Item a -> FilePath
  toRelPath :: Item a -> FilePath

instance ItemType Source where
  toPath config item = (sourceDir config) </> (unItem item)
  toRelPath = unItem

instance ItemType Work where
  toPath config item = (workDir config) </> (unItem item)
  toRelPath = unItem

instance ItemType Output where
  toPath config item = (outputDir config) </> (unItem item)
  toRelPath = unItem


toWork :: Item Source -> Item Work
toWork = Item . unItem

class OutputSource a
instance OutputSource Source
instance OutputSource Work

toOutput :: (OutputSource a) => Item a -> Item Output
toOutput = Item . unItem

toOutputExt :: (OutputSource a) => Item a -> Extension -> Item Output
toOutputExt item ext = Item $ unItem item <.> ext

