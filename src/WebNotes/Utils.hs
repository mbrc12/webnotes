module WebNotes.Utils 
  ( doBoth,
    explode,
    Extension(..),
  )
where

import System.FilePath


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

