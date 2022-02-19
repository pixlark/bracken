{-# LANGUAGE CApiFFI
           , ScopedTypeVariables #-}

module CC (compileCExecutable) where

import Control.Exception (catch, IOException)
import System.Directory (removeFile)
import System.IO
import System.Process (callProcess)

ccErrorMsg = "Error: Internal Error: Couldn't compile generated C source."

compileCExecutable :: String -> String -> IO (Maybe String)
compileCExecutable source cc = do
  (path, handle) <- openTempFile "." "cmsrc.c"
  hPutStr handle source
  hClose handle

  let execName = "executable"
  ok <- catch (do callProcess cc [path, "-o " ++ execName]; return True)
        $ \(e :: IOException) -> do hPutStrLn stderr ccErrorMsg; return False

  removeFile path

  return $ if ok then Just execName else Nothing
