{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Raw where

import Network.Wai
import Network.Wai.Application.Static
import WaiAppStatic.Storage.Filesystem
import WaiAppStatic.Types

libPieces = [unsafeToPiece "lib.js"]
rtsPieces = [unsafeToPiece "rts.js"]
outPieces = [unsafeToPiece "out.js"]
runmainPieces = [unsafeToPiece "runmain.js"]
indexPieces = [unsafeToPiece "index.html"]

codePieceses = [ libPieces, outPieces, rtsPieces, runmainPieces ]

rawServer :: FilePath -> Application
rawServer root =
  staticApp $ (defaultWebAppSettings root) { ssLookupFile = lookup' }
  where lookup = ssLookupFile (defaultWebAppSettings root)
        lookup' pieces = lookup pieces'
          where pieces' = if pieces `elem` codePieceses then pieces
                          else indexPieces
