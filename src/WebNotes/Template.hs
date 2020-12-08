{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module WebNotes.Template
  ( templatePage,
    templateConvert,
    templateIndex,
    PageData(..),
    ConvertData(..),
    IndexItem(..),
    IndexData(..)
  )
where

import Data.Aeson ((.=), object)
import Data.ByteString as BS
import Data.Function ((&))
import Data.Text.Lazy (toStrict)
import Data.Time (UTCTime(..))
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Text.Microstache as M

data PageData = PageData
  { originalPath :: FilePath, -- original path of the content file 
    finalPath :: FilePath,    -- final path of the content file
    pageContents :: T.Text, -- final contents
    lastModified :: UTCTime
  }

data ConvertData = ConvertData
  { sourcePath :: FilePath,
    destPath :: FilePath
  }  

data IndexItem = IndexItem
  { displayPath :: FilePath,
    lastMod     :: UTCTime
  }


data IndexData = IndexData 
  { items :: [IndexItem]  -- paths where the final generated pages are stored
  }
    
templateConvert :: M.Template -> ConvertData -> T.Text
templateConvert templateData (ConvertData {..}) = toStrict $ 
  M.renderMustache templateData $ object $
    [ 
      "source_path" .= sourcePath,
      "dest_path" .= destPath
    ]

templatePage :: M.Template -> PageData -> T.Text
templatePage templateData (PageData {..}) = toStrict $ 
  M.renderMustache templateData $ object $
    [ 
      "original_path" .= originalPath,
      "final_path" .= finalPath, 
      "contents" .= pageContents,
      "last_modified" .= (T.pack $ show lastModified)
    ]

templateIndex :: M.Template -> IndexData -> T.Text
templateIndex templateData (IndexData {..}) = toStrict $
  M.renderMustache templateData $ object $
    [ 
      "items" .= ((\item -> 
        object $ 
          [ 
            "display_path" .= (T.pack $ displayPath item),
            "last_modified" .= (T.pack . show $ lastMod item)
          ]) <$> items)
    ]
