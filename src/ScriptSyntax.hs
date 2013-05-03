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

module ScriptSyntax (scriptLink, matchScriptLink) where

import Text.XHtml
import Text.ParserCombinators.Parsec

data ScriptParm = Field String | Value String String

scriptname = many1 alphaNum

fieldname = many1 alphaNum

parmstring = do char '"'
                ps <- manyTill anyChar (char '"')
                return ps

scriptparm = try (do f <- fieldname
                     char '='
                     p <- parmstring
                     return (Value f p)
                 )
             <|> 
             do f <- fieldname
                return (Field f)

scriptLink sn = do char '['
                   char ':'
                   s <- scriptname
                   char '|'
                   ps <- many (do p <- scriptparm
                                  many (char ' ') 
                                  return p
                              )
                   char '|'
                   t <- manyTill anyChar (try $ do char ':'; char ']';)
                   t <- case t of
                           [] -> return s
                           _ -> return t
                   case ps of 
                    [] -> linkToScript s t
                    ps -> return (form (
                                    (foldl1 (\a b->a +++ br +++ b)
                                            (map parmToField ps)
                                    )
                                    +++
                                    input ! [thetype "submit", value t]
                                  ) ! [action (sn ++ "?script=" ++ s),
                                       method "post"
                                      ]
                                 )
              where parmToField (Value f v) = 
                      input ! [thetype "hidden", 
                               name f, 
                               value v
                              ] 
                    parmToField (Field f) = 
                      (f ++ ": ")
                      +++
                      input ! [thetype "text", 
		               name f, 
		               value ""
                              ]
                    linkToScript s t = 
                      return (anchor (stringToHtml t)
                              ! [href (sn ++ "?script=" ++ s)
                                ]
                             )

matchScriptLink = do char '['
                     char ':'
                     s <- scriptname
                     char '|'
                     ps <- many (try (do f <- fieldname
                                         char '='
                                         p <- parmstring
                                         spaces <- many (char ' ')
                                         return (f ++ "=\"" 
                                                 ++ p ++ 
                                                 "\"" ++ 
                                                 spaces
                                                )
                                     )
                                 <|>
                                 (do f <- fieldname
                                     spaces <- many (char ' ')
                                     return (f ++ spaces)
                                 )
                                )
                     char '|'
                     t <- manyTill anyChar (try $ do char ':'; char ']';)
                     return (stringToHtml ("[:" ++ s ++ "|" ++ 
                                           (concat ps) ++ "|" 
                                           ++ t ++ ":]"
                                          )
                            )
