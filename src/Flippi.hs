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

module Main (main) where

import Network.CGI
import Network.CGI.Compat (wrapper)
import Text.XHtml
import System.IO
import System.Time
import List
import Maybe

import PageIO
import DeWikify
import PageTemplates
import Scripts
import System.Environment

main :: IO ()
main = wrapper cgi

cgi :: [(String, String)] -> IO Html
cgi env = do m <- return (maybe "GET" id (lookup "REQUEST_METHOD" env))
             qs <- return (maybe "" id (lookup "QUERY_STRING" env))
             case m of
              'G':'E':'T':[] -> handleGet qs env
              'P':'O':'S':'T':[] -> handlePost qs env
              _ -> malformedQueryPage env

data PParms = Default |
              View String |
              Edit String |
              Script String

parseParms p = case p of
                ('e':'d':'i':'t':'=':[]) -> Default
                ('e':'d':'i':'t':'=':e) -> Edit e
                ('v':'i':'e':'w':'=':[]) -> Default
                ('v':'i':'e':'w':'=':v) -> View v
                ('s':'c':'r':'i':'p':'t':'=':s) -> Script s
                [] -> Default
                s -> View s

handleGet qs env = do action <- return (parseParms qs)
                      case action of
                       View v -> if (isPagename v) then
                                   viewPage v env
                                  else defaultPage env
                       Default -> defaultPage env
                       Edit e -> if (isPagename e) then
                                   editPage e env
                                  else
                                   editMalformedPage e env
                       Script s -> runScript s env

handlePost qs env = do action <- return (parseParms qs)
                       case action of
                        View v -> if (isPagename v) then
                                   viewPage v env
                                  else defaultPage env
                        Default -> defaultPage env
                        Edit e -> if (isPagename e) then
                                    updatePage e env
                                   else
                                    updateMalformedPage e env
                        Script s -> runScript s env

updatePage p env = do text <- return (lookup "Text" env)
                      oldmodified <- return (lookup "oldDate" env)
                      oldmodified <-
                        case oldmodified of
                         Nothing -> return Nothing
                         Just ct -> return (Just ((read ct)::CalendarTime))
                      modified <- getPageLastUpdated p
                      modified <- case modified of
                         Nothing -> return Nothing
                         Just dt -> do ct <- toCalendarTime dt
                                       return (Just ct)
                      case (oldmodified, modified) of
                       (Nothing, Nothing) -> tryWrite text
                       (Nothing, Just d) -> updateMalformedPage p env
                       (Just d, Nothing) -> tryWrite text
                       (Just od, Just d) -> if (od < d) then
                                             editConflictPage p env
                                             else if (od > d) then
                                              updateMalformedPage p env
                                               else tryWrite text
                 where tryWrite t = case t of
                                     Just text -> do writePage p text
                                                     updateSuccessPage p env
                                     Nothing -> do updateMalformedPage p env
