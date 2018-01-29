{-# LANGUAGE RecordWildCards, ViewPatterns, RankNTypes, ScopedTypeVariables, LambdaCase #-}

import qualified System.Directory as Dir
import qualified Data.Map as Map
import System.Environment (getArgs)
import System.Process (readProcess, callProcess)
import System.Random (RandomGen, randomR, getStdGen)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate, find, nub)
import Data.List.Split (splitOn)
import Prelude hiding ((.))

-- Util

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

httpDownload :: String -> IO String
httpDownload url = readProcess "wget" ["--no-cache", "-q", "-O", "-", url] ""

-- Tracklist

type Username = String

newtype Likes = Likes { likes :: Map Username Bool } deriving Eq

data Track = Track { title, url :: String, trackLikes :: Likes } deriving Eq
data Header = Header { headerLevel :: Int, headerLikes :: Likes }

score :: Likes -> Int
score (Likes m) = likes - dislikes * 2
    where
        likes = length $ filter snd (Map.toList m)
        dislikes = Map.size m - likes

overrideLikes :: Likes -> Likes -> Likes
overrideLikes (Likes old) (Likes new) = Likes $ Map.union new old -- Map.union's bias is used here

addLikes :: Likes -> Track -> Track
addLikes l t = t{trackLikes = overrideLikes l (trackLikes t)}

assignGroupLikes :: [Either Track Header] -> [Track]
assignGroupLikes = go
    where
        go :: [Either Track Header] -> [Track]
        go [] = []
        go (Left t : more) = t : go more
        go (Right Header{..} : (span (isBelow headerLevel) -> (content, rest))) =
            (addLikes headerLikes . go content) ++ go rest
        isBelow _ (Left _) = True
        isBelow n (Right Header{..}) = headerLevel > n

-- Parsing

parseError :: Int -> String -> a
parseError num msg = error $ "line " ++ show num ++ ": parse error: " ++ msg

parseLikes :: String -> Likes
parseLikes (':': (dropWhile isSpace -> x)) = Likes $ Map.fromList $ parseLike . words x
    where
        parseLike :: String -> (Username, Bool)
        parseLike ('-':s) = (s, False)
        parseLike s = (s, True)
parseLikes _ = Likes mempty

parseLine :: Int -> String -> Maybe (Either Track Header)
parseLine _ [] = Nothing
parseLine _ ('#':(span (=='#') -> (octos, (dropWhile (/= ':') ->
            (parseLikes -> headerLikes))))) = Just (Right Header{headerLevel = length octos + 1, ..})
parseLine _ ('-' : (dropWhile isSpace ->
            ('[' : (span (/= ']') -> (title, ']':
            '(' : (span (/= ')') -> (url, ')':(parseLikes -> trackLikes)))))))) = Just (Left Track{..})
parseLine num line = parseError num line

parseMarkdown :: String -> [Track]
parseMarkdown = assignGroupLikes . mapMaybe (uncurry parseLine) . zip [1..] . lines

-- Rendering

renderTrack :: Track -> [String]
renderTrack e = ["#EXTINF:1," ++ replace "," " " (title e), url e]

renderTracks :: [Track] -> String
renderTracks = unlines . ("#EXTM3U" :) . concatMap renderTrack

instance Show Likes where
    show (Likes m) = if Map.null m then "" else ": " ++ unwords (showLike . Map.toList m)
        where showLike (u, b) = (if b then "" else "-") ++ u

instance Show Track where show Track{..} = "- [" ++ title ++ "](" ++ url ++ ")" ++ show trackLikes

-- Youtube

youtubePrefix :: String
youtubePrefix = "https://www.youtube.com/watch?v="

findCachedYoutubeTrack :: String -> IO (Maybe String)
findCachedYoutubeTrack videoId = do
    list <- Dir.getDirectoryContents "cache"
    return $ ("cache/" ++) . find (videoId `isInfixOf`) list

downloadYoutubeTrack :: String -> IO ()
downloadYoutubeTrack url = do
    cwd <- Dir.getCurrentDirectory
    Dir.setCurrentDirectory "cache"
    callProcess "youtube-dl" ["-x", url]
    Dir.setCurrentDirectory cwd

findOrAddYoutubeTrack :: String -> IO String
findOrAddYoutubeTrack url = findCachedYoutubeTrack videoId >>= \case
    Just filename -> return filename
    Nothing -> downloadYoutubeTrack url >> findCachedYoutubeTrack videoId >>= \case
        Just f -> return f
        Nothing -> error $ "youtube-dl failed for " ++ url
  where videoId = drop (length youtubePrefix) url

-- Url rewriting

isRelative :: String -> Bool
isRelative x = not $ any (`isPrefixOf` x) ["https:", "http:"]

rewriteTrack :: String -> Track -> IO Track
rewriteTrack baseUrl e
        -- rewrite github urls to point to the raw file:
        | "https://github.com/" `isPrefixOf` full
        , not ("?raw=true" `isSuffixOf` full) =
            return e{ url = full ++ "?raw=true"}
        -- download youtube files into cache:
        | youtubePrefix `isPrefixOf` full = do
            filename <- findOrAddYoutubeTrack full
            return e{url = filename}
        -- leave other urls as-is:
        | otherwise = return e{url = full}
    where
        full = (if isRelative (url e) then baseUrl else "") ++ url e

-- Tracklist retrieval

downloadTracklistFile :: String -> IO String
downloadTracklistFile url
    | "https://github.com/" `isPrefixOf` url = httpDownload (replace "/blob/" "/raw/" url)
    | "https://gitlab." `isPrefixOf` url = httpDownload (replace "/blob/" "/raw/" url)
    | "http" `isPrefixOf` url = httpDownload url
    | otherwise = readFile url

getTracklist :: String -> IO [Track]
getTracklist url = parseMarkdown . downloadTracklistFile url

-- Playlist generation

randomTracks :: forall gen . RandomGen gen => [Track] -> gen -> Int -> [Track]
randomTracks tracks = (nub .) . go
    where
        weigh :: Track -> [Track]
        weigh x = replicate (score $ trackLikes x) x

        weighted :: [Track]
        weighted = tracks >>= weigh

        randomIndex :: gen -> (Int, gen)
        randomIndex = randomR (0, length weighted - 1)

        go :: gen -> Int -> [Track]
        go _ 0 = []
        go g n = (weighted !! i) : go g' (n - 1)
            where (i, g') = randomIndex g

generatePlaylist :: String -> [Track] -> IO String
generatePlaylist baseUrl trackList = do
    stdGen <- getStdGen
    let selection = randomTracks trackList stdGen 100
    renderTracks . mapM (rewriteTrack baseUrl) selection

-- Main

data CmdLine = CmdLine { cmdLineUrl :: String }

parseCmdLine :: [String] -> CmdLine
parseCmdLine [x] = CmdLine{ cmdLineUrl = x }
parseCmdLine _ = error "args: url"

main :: IO ()
main = do
    CmdLine{..} <- parseCmdLine . getArgs
    Dir.createDirectoryIfMissing False "cache"
    let baseUrl = reverse $ dropWhile (/= '/') $ reverse cmdLineUrl
    trackList <- getTracklist cmdLineUrl
    writeFile "flat-tracklist.txt" $ unlines $ show . trackList
    generatePlaylist baseUrl trackList >>= writeFile "playlist.m3u8"
