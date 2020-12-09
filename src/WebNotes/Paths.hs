{-# LANGUAGE RecordWildCards, AllowAmbiguousTypes #-}

module WebNotes.Paths 
  (
    Item,
    Source(..),
    Work(..),
    Output(..),
    sourceExt,
    toWork,
    toOutput,
    toOutputExt,
    toSourceFile,
    toWorkFile,
    toOutputFile,
    toPath,
  )
where

import WebNotes.ConfigParser
import WebNotes.Utils

import System.FilePath

newtype Item a = Item { unItem :: FilePath }

data Source
data Work
data Output

sourceExt :: Item Source -> Extension
sourceExt (Item src) = 
  let (_, _, ext) = explode src
   in ext

toSourceFile :: FilePath -> Item Source
toWorkFile :: FilePath -> Item Work
toOutputFile :: FilePath -> Item Output
toSourceFile = Item
toWorkFile = Item
toOutputFile = Item


class ItemType a where
  toPath :: Config -> Item a -> FilePath

instance ItemType Source where
  toPath config item = (sourceDir config) </> (unItem item)

instance ItemType Work where
  toPath config item = (workDir config) </> (unItem item)

instance ItemType Output where
  toPath config item = (outputDir config) </> (unItem item)


toWork :: Item Source -> Item Work
toWork = Item . unItem

class OutputSource a
instance OutputSource Source
instance OutputSource Work

toOutput :: (OutputSource a) => Item a -> Item Output
toOutput = Item . unItem

toOutputExt :: (OutputSource a) => Item a -> Extension -> Item Output
toOutputExt item ext = Item $ unItem item <.> ext

