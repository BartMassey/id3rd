-- Copyright © 2011 Bart Massey
-- Dump all the ID3 tag information for a file

import Codec.Text.IConv
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import ID3
import System.Console.ParseArgs

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
        
-- Convert input encoding into UTF-8
decode :: ID3Tag -> Integer -> String -> String
decode tag encoding str =
  let (id3version, _) = tagVersion_ $ tagHeader tag in
  let encodingName =
        case encoding of
          0 -> "LATIN1"
          1 -> 
            case id3version of
              2 -> "UCS-2"
              3 -> "UCS-2"
              4 -> "UTF-16"
              _ -> error "unknown id3 version in encoding"
          2 -> 
            case id3version of
              2 -> "UTF-16BE"
              _ -> error "unknown id3 version in encoding"
          3 -> "UTF-8"
          _ -> error "unknown encoding"
          in
   B.unpack $ convert encodingName "UTF-8" $ B.pack str

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  maybeTag <- readTag $ getRequiredArg args IndexFile
  case maybeTag of
    Just tag ->
      case getArg args IndexFrame of
        Nothing -> putStr $ show tag
        Just target -> 
          let maybeTargetFrame = M.lookup target $ tagFrames tag in
          case maybeTargetFrame of
            Just targetFrame -> 
              case frInfo_ targetFrame of
                Text enc str -> putStrLn $ decode tag enc str
