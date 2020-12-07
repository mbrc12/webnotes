module WebNotes.Utils 
  ( doBoth,
    explode,
    Extension(..),
    emptyOrUnMaybe
  )
where

import System.FilePath
import Control.Monad (MonadPlus, mzero)

type Extension = String

doBoth :: (Monad m) => m a -> m b -> m (a, b)
doBoth eff1 eff2 = do
  res1 <- eff1
  res2 <- eff2
  return (res1, res2)


explode :: FilePath -> (String, String, Extension)
explode path = 
  let (rest, ext) = splitExtension path
      (dir, fileName) = splitFileName rest
  in (dir, fileName, ext)


emptyOrUnMaybe :: (MonadPlus m) => m (Maybe b) -> m b
emptyOrUnMaybe x = do
  z <- x
  case z of 
    Nothing -> mzero
    Just y -> return y

