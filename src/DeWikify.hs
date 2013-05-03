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

module DeWikify (deWikify, pagenamePattern) 
where

import Text.XHtml
import Text.ParserCombinators.Parsec

import ScriptSyntax

wikiParser sn = do l <- many (prependedUnderscore
                              <|> try (wikiLink sn)
                              <|> try (scriptLink sn)
                              <|> try newParagraph
                              <|> aCharacter
                             )
                   case l of 
                    (_:_) -> return (foldl1 (+++) l)
                    [] -> return (stringToHtml "")

newParagraph = do string "\n\n"
                  return (br +++ br)
               <|>
               do string "\r\n\r\n"
                  return (br +++ br)

wikiLink sn = do (l,t,_) <- linkPattern
                 return (anchor (stringToHtml t)
		         ! [href (sn ++ "?view=" ++ l)]
                        )

prependedUnderscore = do char '_'
                         try nextUnderscore
                          <|> try matchLink
                          <|> try matchScriptLink
                          <|> return (stringToHtml "_")                         
                    where matchLink = do (_,_,p) <- linkPattern 
                                         return (stringToHtml p)
                          nextUnderscore = do pu <- prependedUnderscore
                                              return (stringToHtml "_" +++ pu)

pagenamePattern = studlyCapsPattern

linkPattern = try $ do char '['
                       l <- pagenamePattern
                       char '|'
                       t <- manyTill anyChar (char ']')
                       return (l,t, ("["++l++"|"++t++"]"))
              <|>
              do p <- studlyCapsPattern
                 return (p,p,p)

studlyCapsPattern = do u1 <- upper
                       ls1 <- many1 lower
                       u2 <- upper
                       l2 <- lower
                       as <- many letter
                       return ([u1]++ls1++[u2]++[l2]++as)

aCharacter = do c <- anyChar
                return (stringToHtml [c])

deWikify sn wt = case (parse (wikiParser sn) "" wt) of
                  Left e -> stringToHtml ("Error in wikitext: "
                                          ++ 
                                          (show e)
                                         )
                  Right t -> t
