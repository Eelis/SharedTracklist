{-# LANGUAGE RecordWildCards, ViewPatterns, RankNTypes, ScopedTypeVariables #-}

import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import System.Random (RandomGen, randomR, getStdGen)
import Data.List (isPrefixOf, isSuffixOf, intercalate)
import Prelude hiding ((.))
import Data.List.Split (splitOn)
import System.Process

-- Util

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

replace old new = intercalate new . splitOn old

httpDownload :: String -> IO String
httpDownload url = readProcess "wget" ["--no-cache", "-q", "-O", "-", url] ""

-- Track list

type Username = String

data Entry = Entry
    { title, url :: String
    , likes, dislikes :: [Username] }
    deriving Show

score :: Entry -> Int
score Entry{..} = length likes - length dislikes * 2

-- Parsing

isMdHeader :: String -> Bool
isMdHeader s = length s /= 0 && head s == '#'

parseError :: Int -> String -> a
parseError num msg = error $ "line " ++ show num ++ ": parse error: " ++ msg

parseLikes :: Int -> String -> ([Username], [Username]) -- returns likes, dislikes
parseLikes lineNum = partitionEithers . (parseLike .) . words
    where
        parseLike :: String -> Either Username Username
        parseLike ('-':s) = Right s
        parseLike s = Left s

parseLine :: Int -> String -> Maybe Entry
parseLine _ "" = Nothing
parseLine _ line | isMdHeader line = Nothing
parseLine num ('-' : (dropWhile isSpace ->
            ('[' : (span (/= ']') -> (title, ']':
            '(' : (span (/= ')') -> (url, ')':':':
            (dropWhile isSpace -> parseLikes num -> (likes, dislikes))))))))) = Just Entry{..}
parseLine num line = parseError num line

parseMarkdown :: [String] -> [Entry]
parseMarkdown = mapMaybe (uncurry parseLine) . zip [1..]

-- Track selection

randomEntries :: forall g . RandomGen g => [Entry] -> g -> Int -> [Entry]
randomEntries entries = go
    where
        weigh :: Entry -> [Entry]
        weigh x = replicate (score x) x
        weighted = entries >>= weigh

        go :: g -> Int -> [Entry]
        go _ 0 = []
        go gen n =
            let (i :: Int, gen') = randomR (0, length weighted - 1) gen
            in (weighted !! i) : go gen' (n - 1)

-- Url rewriting

rewriteEntry :: String -> Entry -> Entry
rewriteEntry baseUrl e
    | "https://github.com/" `isPrefixOf` url e, not ("?raw=true" `isSuffixOf` url e) = e{ url = url e ++ "?raw=true"}
--    | "https://www.youtube.com/" `isPrefixOf` url e =
    | "http:" `isPrefixOf` url e || "https:" `isPrefixOf` url e = e
    | otherwise = rewriteEntry baseUrl e{url = baseUrl ++ url e}

-- Tracklist retrieval

downloadTracklistFile :: String -> IO String
downloadTracklistFile url
    | "https://github.com/" `isPrefixOf` url = httpDownload (replace "/blob/" "/raw/" url)
    | "http" `isPrefixOf` url = httpDownload url
    | otherwise = readFile url

getTracklist :: String -> IO [Entry]
getTracklist url = parseMarkdown . lines . downloadTracklistFile url

-- Playlist generation

renderEntry :: Entry -> String
renderEntry = url

renderEntries :: [Entry] -> String
renderEntries = unlines . (renderEntry .)

generatePlaylist :: String -> [Entry] -> IO String
generatePlaylist baseUrl trackList = do
    stdGen <- getStdGen
    return $ renderEntries $ rewriteEntry baseUrl . randomEntries trackList stdGen 10

-- Main

data CmdLine = CmdLine { cmdLineUrl :: String }

parseCmdLine :: [String] -> CmdLine
parseCmdLine [x] = CmdLine{ cmdLineUrl = x }
parseCmdLine _ = error "args: url"

main :: IO ()
main = do
    CmdLine{..} <- parseCmdLine . getArgs
    let baseUrl = reverse $ dropWhile (/= '/') $ reverse cmdLineUrl
    getTracklist cmdLineUrl >>= generatePlaylist baseUrl >>= putStr
