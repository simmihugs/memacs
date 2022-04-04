module Parser.QQ where

import Language.Haskell.TH 
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Parser.Parser
import Parser.Json
import Parser.Open

readJson = readFile

-- | quasiquoter for Open parser (experiment)
openQQ :: QuasiQuoter
openQQ = QuasiQuoter
         { quoteExp = \str -> do
             let pString = run open str
             case pString of
               Nothing   -> [| "Invalid Input" |]
               Just (OpenList [f,t],_) -> do
                 let (FileName fileName') = f
                 let (FileFunc fileFunc') = t
                 let fileName'' = case fileName' of
                                    Right name -> varE (mkName name)
                                    Left value -> [| value |]
                 let fileFunc'' = case fileFunc' of
                                    Right name -> varE (mkName name)
                                    Left value -> [| value |]
                 appE (appE [| \x -> if x=="json"
                              then readJson
                              else readJson |] fileFunc'') fileName''
               _ -> [| "Invalid Input" |]
         , quotePat  = undefined
         , quoteType = undefined
         , quoteDec  = undefined
         }

-- | quasiquoter for parsing json data
settingsQQ :: QuasiQuoter
settingsQQ = QuasiQuoter
             { quoteExp = \str -> do
                 let content = run json str
                 case content of
                   Nothing         -> dataToExpQ (const Nothing) JNull
                   Just (jvalue,_) -> dataToExpQ (const Nothing) jvalue
             , quotePat  = undefined
             , quoteType = undefined
             , quoteDec  = undefined
             }
