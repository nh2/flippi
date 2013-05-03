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

module PageTemplates where

import Text.XHtml
import DeWikify
import PageIO
import System.Time


htmlPage p t env = return $ (header $ thetitle t) +++ bodystuff p
                 where bodystuff h = body $ (viewform +++                                             
                                             hr +++ 
                                             p)
                       sn = maybe "" id (lookup "SCRIPT_NAME" env)
                       editform = stringToHtml "Edit a page: "
                                  +++
                                  form (
                                   input ! [thetype "text", 
                                            value "",
                                            name "edit"]
                                   +++
                                   input ! [thetype "submit", value "Submit"]
                                  ) ! [action sn, method "get"]
                       viewform = stringToHtml "View a page: "
                                  +++
                                  form (
                                   input ! [thetype "text", 
                                            value "",
                                            name "view"]
                                   +++
                                   input ! [thetype "submit", value "Submit"]
                                  ) ! [action sn, method "get"]

page p t env = do sn <- return (maybe "" id (lookup "SCRIPT_NAME" env))
                  htmlPage (deWikify sn p) (stringToHtml t) env                       

viewPage p env = do text <- getPage p env
                    case text of
                     Just text -> htmlPage ((deWikify sn text) +++ viewFooter)
                                           (stringToHtml p) 
                                           env                      
                     Nothing -> pageNotFound p env
  where viewFooter = hr +++ (anchor (stringToHtml "Edit this page")
                              ! [(href (sn ++ "?edit=" ++ p))]
                            )                     
        sn = maybe "" id (lookup "SCRIPT_NAME" env)

editPage p env = do sn <- return (maybe "" id (lookup "SCRIPT_NAME" env))
                    text <- getPage p env
                    modified <- getPageLastUpdated p
                    modified <- case modified of
                                 Just d -> toCalendarTime d
                                 Nothing -> do ct <- getClockTime
                                               toCalendarTime ct
                    htmlPage (stringToHtml ("editing " ++ p)
                              +++
                              br
                              +++
                              form (
                                textarea (
                                 case text of 
				  Nothing -> stringToHtml ""
                                  Just t -> stringToHtml t                                                  
                                ) ! [cols "80",
                                     rows "18",
                                     name "Text"]
                                +++
                                input ! [thetype "hidden", 
                                         name "oldDate", 
                                         value $ (show modified)
                                        ]
                                +++
                                input ! [thetype "submit", value "Submit"]
                              ) ! [action (sn ++ "?edit=" ++ p),
                                   method "post"]
                             ) 
                             (stringToHtml ("editing " ++ p)) 
                             env

pageNotFound p env = do sn <- return (maybe "" id (lookup "SCRIPT_NAME" env))
                        htmlPage (pbody sn) 
                                 (stringToHtml "Page not found")
                                 env
                   where pbody sn = (deWikify sn ("Page not found: " ++ p))
                                    +++
                                    br 
                                    +++ 
                                    (anchor (stringToHtml ("Create " ++ p)) 
                                      ! [(href (sn ++ "?edit=" ++ p))]                                      
                                    )
                        
updateSuccessPage p env = page ("Page " ++ p ++ " updated")
                               "Update Successful"
                               env
updateMalformedPage p env = page "Malformed update request"
                                 "Update Failed"
                                 env
editMalformedPage p env = page "Malformed edit request"
                               "Edit Failed"
                               env
editConflictPage p env =  page ("Somebody else updated " ++ p
                                 ++ " while you were editing - "
                                 ++ "your edit has been discarded"
                               )
                                "Update Failed"
                                env


malformedQueryPage env = page "Malformed query"
                              "Error"
                              env

unknownScriptPage s env = page ("Unknown script \"" ++ s ++ "\"")
                               "Error"
                               env


defaultPage env = viewPage "FrontPage" env
