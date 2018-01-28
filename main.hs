{-# LANGUAGE RecordWildCards, ViewPatterns, RankNTypes, ScopedTypeVariables #-}

import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import System.Random (RandomGen, randomR, getStdGen)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, intercalate, find)
import Prelude hiding ((.))
import Data.List.Split (splitOn)
import System.Process (readProcess, callProcess)
import System.Directory (getDirectoryContents, getCurrentDirectory, setCurrentDirectory)
import qualified Data.Map as Map
import Data.Map (Map)

-- Util

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new = intercalate new . splitOn old

httpDownload :: String -> IO String
httpDownload url = readProcess "wget" ["--no-cache", "-q", "-O", "-", url] ""

-- Track list

type Username = String

type Likes = Map Username Bool

data Track = Track { title, url :: String, trackLikes :: Likes }
data Header = Header { headerLevel :: Int, headerLikes :: Likes }
score :: Likes -> Int
score m = likes - dislikes * 2
    where
        likes = length $ filter snd (Map.toList m)
        dislikes = Map.size m - likes

-- Parsing

parseError :: Int -> String -> a
parseError num msg = error $ "line " ++ show num ++ ": parse error: " ++ msg

parseLikes :: String -> Likes
parseLikes (':': (dropWhile isSpace -> x)) = Map.fromList $ parseLike . words x
    where
        parseLike :: String -> (Username, Bool)
        parseLike ('-':s) = (s, False)
        parseLike s = (s, True)
parseLikes _ = mempty

parseLine :: Int -> String -> Maybe (Either Track Header)
parseLine _ [] = Nothing
parseLine _ ('#':(span (=='#') -> (octos, (dropWhile (/= ':') ->
            (parseLikes -> headerLikes))))) = Just (Right Header{..})
    where headerLevel = length octos + 1
parseLine _ ('-' : (dropWhile isSpace ->
            ('[' : (span (/= ']') -> (title, ']':
            '(' : (span (/= ')') -> (url, ')':(parseLikes -> trackLikes)))))))) = Just (Left Track{..})
parseLine num line = parseError num line

addLikes :: Likes -> Track -> Track
addLikes l t = t{trackLikes = mappend (trackLikes t) l}

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

parseMarkdown :: [String] -> [Track]
parseMarkdown = assignGroupLikes . mapMaybe (uncurry parseLine) . zip [1..]

-- Youtube

youtubePrefix :: String
youtubePrefix = "https://www.youtube.com/watch?v="

findCachedYoutubeTrack :: String -> IO (Maybe String)
findCachedYoutubeTrack videoId = do
    list <- getDirectoryContents "cache"
    return $ ("cache/" ++) . find (videoId `isInfixOf`) list

downloadYoutubeTrack :: String -> IO ()
downloadYoutubeTrack url = do
    cwd <- getCurrentDirectory
    setCurrentDirectory "cache"
    callProcess "youtube-dl" ["-x", "--audio-format", "mp3", url]
    setCurrentDirectory cwd

findOrAddYoutubeTrack :: String -> IO String
findOrAddYoutubeTrack url = do
    let videoId = drop (length youtubePrefix) url
    cached <- findCachedYoutubeTrack videoId
    case cached of
        Just filename -> return filename
        Nothing -> do
            downloadYoutubeTrack url
            f <- findCachedYoutubeTrack videoId -- should exist now
            case f of
                Nothing -> error "youtube-dl failed"
                Just f' -> return f'

-- Url rewriting

rewriteTrack :: String -> Track -> IO Track
rewriteTrack baseUrl e
    | "https://github.com/" `isPrefixOf` url e, not ("?raw=true" `isSuffixOf` url e) =
        return e{ url = url e ++ "?raw=true"}
    | youtubePrefix `isPrefixOf` url e = do
        filename <- findOrAddYoutubeTrack (url e)
        return e{url = filename}
    | "http:" `isPrefixOf` url e || "https:" `isPrefixOf` url e = return e
    | otherwise = rewriteTrack baseUrl e{url = baseUrl ++ url e}

-- Tracklist retrieval

downloadTracklistFile :: String -> IO String
downloadTracklistFile url
    | "https://github.com/" `isPrefixOf` url = httpDownload (replace "/blob/" "/raw/" url)
    | "http" `isPrefixOf` url = httpDownload url
    | otherwise = readFile url

getTracklist :: String -> IO [Track]
getTracklist url = parseMarkdown . lines . downloadTracklistFile url

-- Playlist generation

randomTracks :: forall g . RandomGen g => [Track] -> g -> Int -> [Track]
randomTracks tracks = go
    where
        weigh :: Track -> [Track]
        weigh x = replicate (score $ trackLikes x) x
        weighted = tracks >>= weigh

        go :: g -> Int -> [Track]
        go _ 0 = []
        go gen n =
            let (i :: Int, gen') = randomR (0, length weighted - 1) gen
            in (weighted !! i) : go gen' (n - 1)

renderTrack :: Track -> [String]
renderTrack e = ["#EXTINF:1," ++ replace "," " " (title e), url e]

renderTracks :: [Track] -> String
renderTracks = unlines . ("#EXTM3U" :) . concatMap renderTrack

generatePlaylist :: String -> [Track] -> IO String
generatePlaylist baseUrl trackList = do
    stdGen <- getStdGen
    let selection = randomTracks trackList stdGen 10
    renderTracks . mapM (rewriteTrack baseUrl) selection

-- Main

data CmdLine = CmdLine { cmdLineUrl :: String }

parseCmdLine :: [String] -> CmdLine
parseCmdLine [x] = CmdLine{ cmdLineUrl = x }
parseCmdLine _ = error "args: url"

main :: IO ()
main = do
    CmdLine{..} <- parseCmdLine . getArgs
    let baseUrl = reverse $ dropWhile (/= '/') $ reverse cmdLineUrl
    getTracklist cmdLineUrl >>= generatePlaylist baseUrl >>= writeFile "playlist.m3u8"
