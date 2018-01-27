{-# LANGUAGE RecordWildCards, ViewPatterns, RankNTypes, ScopedTypeVariables #-}

import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Data.Char (isSpace)
import Data.Either (partitionEithers)
import System.Random
import Prelude hiding ((.))

-- Util

(.) :: Functor f => (a -> b) -> (f a -> f b)
(.) = fmap

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
        parseLike ('-':'@':s) = Right s
        parseLike ('+':'@':s) = Left s
        parseLike ('@':s) = Left s
        parseLike s = parseError lineNum ("malformed (dis)like: " ++ s)

parseLine :: Int -> String -> Maybe Entry
parseLine _ "" = Nothing
parseLine _ line | isMdHeader line = Nothing
parseLine num ('-' : (dropWhile isSpace ->
            ('[' : (span (/= ']') -> (title, ']':
            '(' : (span (/= ')') -> (url, ')':
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

-- Playlist generation

renderEntry :: Entry -> String
renderEntry = url

renderEntries :: [Entry] -> String
renderEntries = unlines . (renderEntry .)

-- Main

data CmdLine = CmdLine { cmdLineUrl :: String }

parseCmdLine :: [String] -> CmdLine
parseCmdLine [x] = CmdLine{ cmdLineUrl = x }
parseCmdLine _ = error "args: url"

main :: IO ()
main = do

    CmdLine{..} <- parseCmdLine . getArgs

    contents <- readFile cmdLineUrl

    let trackList = parseMarkdown $ lines contents

    stdGen <- getStdGen
    let playList = randomEntries trackList stdGen 10
    putStr $ renderEntries playList
