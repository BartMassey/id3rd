-- Copyright © 2011 Bart Massey
-- [This program is licensed under the "MIT License"]
-- Please see the file COPYING in the source
-- distribution of this software for license terms.

-- Read ID3 tag information for a file

import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import ID3
import System.Console.ParseArgs
import System.Exit
import Text.Printf (printf)

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
        
showBytes :: [Word8] -> String
showBytes ws =
  intercalate " " (map (printf "%02x") ws)

showNLText :: String -> String
showNLText s =
  intercalate " / " (lines s)

showFrameInfo :: FrameInfo -> String
showFrameInfo (UFID {owner = s, ID3.id = ws}) = 
  s ++ " / " ++ showBytes ws
showFrameInfo (Text {text = s}) = 
  s
showFrameInfo (TXXX {descr = ds, text = s}) = 
  ds ++ ": " ++ s
showFrameInfo (URL {url = s}) = 
  s
showFrameInfo (WXXX {descr = ds, url = s}) = 
  ds ++ ": " ++ s
--- XXX Should decode MCDI, but cannot find the
--- information necessary to do so right now.
showFrameInfo (MCDI {tocData = ws}) =
  showBytes ws
showFrameInfo (USLT {lang = l, descr = d, text = s}) =
  l ++ ": " ++ d ++ ": " ++ showNLText s
showFrameInfo (COMM {lang = l, descr = d, text = s}) =
  l ++ ": " ++ d ++ ": " ++ s
showFrameInfo (APIC {mime = m, picType = t, descr = d, picData = ws}) =
  show (length ws) ++ ": " ++ m ++ ": " ++ picTypeString t ++ ": " ++ d
    where
      picTypeString t =
        [ "other",
          "icon-32x32-PNG",
          "icon",
          "front-cover",
          "back-cover",
          "leaflet-page",
          "media",
          "lead-artist",
          "artist",
          "conductor",
          "band",
          "composer",
          "lyricist",
          "location",
          "during-recording",
          "during-performance",
          "screen-capture",
          "bright-coloured-fish",
          "illustration",
          "band-logotype",
          "studio-logotype" ] !! fromIntegral t
showFrameInfo (PCNT {counter = c}) =
  show c
showFrameInfo (POPM {email = e, rating = r, counter = c}) =
  e ++ ": " ++ show c ++
  case r of
    0 -> ""
    _ -> ": " ++ show r ++ "/255"
showFrameInfo (USER {lang = l, text = s}) =
  l ++ ": " ++ s
showFrameInfo (PRIV {ownerId = o, privateData = ws}) =
  show (length ws) ++ ": " ++ o
showFrameInfo (TCMP {isPart = True}) =
  "1"
showFrameInfo _ = "?"

lookupTarget :: ID3Tag -> FrameID -> Maybe String
lookupTarget tag target =
  case target of
    "date" ->
      let lt = lookupTarget tag in
      case (lt "TYER", lt "TDAT") of
        (Just y, Just [d1,d2,m1,m2]) ->
          Just $ y ++ ['-',m1,m2,'-',d1,d2]
        (Just y, Nothing) ->
          Just y
        (Nothing, Just [d1,d2,m1,m2]) ->
          Just [m1, m2, '-', d1, d2]
        _ -> Nothing
    _ -> 
      M.lookup target (tagFrames tag) >>= \targetFrame ->
        Just (showFrameInfo $ frInfo_ targetFrame)

main :: IO ()
main = do
  args <- parseArgsIO ArgsComplete argd
  src <- getArgStdio args IndexFile ReadMode
  maybeTag <- hReadTag src
  case maybeTag of
    Just tag ->
      case getArg args IndexFrame of
        Nothing ->
          putStr $ unlines $ 
            map (\frid -> frid ++ ": " ++ fromJust (lookupTarget tag frid)) $
            tagFramesOrder tag
        Just target -> 
          case lookupTarget tag target of
            Just frameData -> putStrLn frameData
            Nothing -> exitFailure
    Nothing -> exitFailure
