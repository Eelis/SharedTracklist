## Summary

This tool:

- is run once a day
- downloads track list (containing urls and (dis)likes) from collaboratively edited gitlab repo, in a [subset of markdown format](doc/format.md)
- supports arbitrary vlc-supported urls in track list
- maintains cache of downloaded tracks (needed for e.g. youtube, which vlc's native support for seems broken)
- picks random tracks
- generates an extended m3u8 playlist for use with e.g. vlc
- has special support for youtube/github/gitlab urls

Benefits of the approach:

- basing track list on markdown means list is automatically rendered nicely as html on gitlab, no need for separate render step
- plain text track list in git repo provides all the benefits of text-based version control: diff, history, blame, revert, fork, etc.
- (dis)likes declared on section headers (e.g. "### Mahtz Mixes: Jimmy Bob") enable natural mass-(dis)like support with hierarchical overrides
- not committed to any playback/streaming system. m3u8 playlists importable with everything
- easy to perform edits such as "remove all of X's votes" when X leaves the company
- easy to run a link checker on the html-rendered track list

## Prerequisites

- wget
- youtube-dl
- ghc

## Usage

    runhaskell main.hs url/for/tracklist.md

The url may also be a path.

## Output

- playlist.m3u8
- flat-tracklist.txt
- cache/*
