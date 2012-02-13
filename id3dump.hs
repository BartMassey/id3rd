-- Copyright © 2011 Bart Massey
-- Dump all the ID3 tag information for a file

import qualified Data.Map as M
import ID3
import System.Console.ParseArgs
import System.Exit

data Index = IndexFrame | IndexFile 
  deriving (Eq, Ord, Enum, Show)

argd :: [Arg Index]
argd = [ 
  Arg { argIndex = IndexFrame,
        argAbbr = Just 'f',
        argName = Just "frame",
        argData = argDataOptional "tag-name" ArgtypeString,
        argDesc = "Return the value for this frame"},
  Arg { argIndex = IndexFile,
        argAbbr = Nothing,
        argName = Nothing,
        argData = argDataRequired "tagged-file" ArgtypeString,
        argDesc = "Inspect this file" } ]
        
lookupTarget :: ID3Tag -> String -> Maybe String
lookupTarget tag target =
  case target of
    "date" ->
      let lt = lookupTarget tag in
      case (lt "TYER", lt "TDAT") of
        (Just [y1,y2,y3,y4], Just [d1,d2,m1,m2]) ->
          Just [y1,y2,y3,y4,'-',m1,m2,'-',d1,d2]
        _ -> Nothing
    _ -> 
      M.lookup target (tagFrames tag) >>= \targetFrame ->
        case frInfo_ targetFrame of
          Text _ str -> Just str
          _ -> error "unknown frame type"

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  maybeTag <- readTag $ getRequiredArg args IndexFile
  case maybeTag of
    Just tag ->
      case getArg args IndexFrame of
        Nothing -> putStr $ show tag
        Just target -> 
          case lookupTarget tag target of
            Just frameData -> putStrLn frameData
            Nothing -> exitFailure
    Nothing -> exitFailure
