{-
Copyright (c) 2004, Philippa Jane Cowderoy
All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright 
      notice, this list of conditions and the following disclaimer in the 
      documentation and/or other materials provided with the distribution.
    * Neither the name of the original author nor the names of any 
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.
-}

module PageIO (isPagename,
               getPageLastUpdated,
               writePage,
               getPage,
               getPagenames)
where

import System.Time
import System.Directory
import System.IO
import Control.Monad
import qualified Control.Exception as E
import Data.Convertible (convert)

import DeWikify(pagenamePattern)
import Text.ParserCombinators.Parsec as Parsec

dataDirectory = "data"

pagenameToFilename pn = dataDirectory ++ "/" ++ pn                     

isPagename pn = case (parse (do {pagenamePattern; eof;}) "" pn) of
                 Left _ -> False
                 Right _ -> True

getPageLastUpdated :: String -> IO (Maybe ClockTime)
getPageLastUpdated p =   
  E.catch (do date <- getModificationTime (pagenameToFilename p)
              return (Just (convert date))
        )
          (\e -> (e :: E.SomeException) `seq` return Nothing)

deCRLF s = case parse (many (Parsec.try (do string "\r\n"; return '\n')
                             <|> 
                             anyChar 
                            )
                      )
                      "" 
                      s of
            Right r -> r

writePage p text = do h <- openBinaryFile (pagenameToFilename p) WriteMode
                      hPutStr h (deCRLF text)
                      hClose h

getPage p env = do exists <- doesFileExist fn
                   if (exists) then
                    do xs <- readFile fn
                       case (deCRLF xs) of
                        [] -> return (Just [])
                        dxs -> return (last dxs `seq` Just dxs)
                    else return Nothing
              where fn = pagenameToFilename p

getPagenames = do dir <- getDirectoryContents dataDirectory                  
                  filterM con dir 
             where con fp = do c1 <- doesFileExist (pagenameToFilename fp)
                               c2 <- return (isPagename fp)
                               return (c1 && c2)
