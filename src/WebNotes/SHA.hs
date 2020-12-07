module WebNotes.SHA
  ( computeSHA,
  )
where

import Conduit
import Crypto.Hash.SHA256 as SHA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Data.Function ((&))
import Data.Functor ((<&>))
import System.FilePath

type ResIO = ResourceT IO

type Hash = BS.ByteString

source :: FilePath -> ConduitT () BS.ByteString ResIO ()
source path = sourceFileBS path

hasher :: SHA.Ctx -> ConduitT BS.ByteString Void ResIO Hash
hasher sha = do
  stuff <- await
  case stuff of
    Nothing -> return $ SHA.finalize sha
    Just line -> line & SHA.update sha & hasher

byteStringToHex :: BS.ByteString -> String
byteStringToHex = BSC.unpack . BSL.toStrict . BSB.toLazyByteString . BSB.byteStringHex

computeSHA filepath =
  source filepath .| hasher SHA.init & runConduit & runResourceT <&> byteStringToHex
