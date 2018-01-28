This tool:

- is run once a day
- downloads track list (containing urls and (dis)likes) from collaboratively edited gitlab repo, in a [subset of markdown format](doc/format.md)
- supports arbitrary vlc-supported urls in track list
- maintains cache of downloaded tracks (needed for e.g. youtube, which vlc's native support for seems broken)
- picks random tracks
- generates an extended m3u8 playlist for use with e.g. vlc
- has special support for youtube/github/gitlab urls

Benefits of the approach:

- subset of markdown means list is rendered nicely on gitlab, no need for separate hosting
- we get all the features of text-based version control for free: backup, diff, blame, revert, fork, etc.
- natural grouping using section headers (e.g. "### Mahtz Mixes: Jimmy Bob"), with support for group-(dis)likes
- not committed to any playback/streaming system. m3u8 playlists importable with everything
- easy to perform edits such as "remove all of X's votes" when X leaves the company
- easy to run a link checker on the html-rendered track list

Prerequisites:

- wget
- youtube-dl
- ghc

Usage:

    runhaskell main.hs url/for/tracklist.md
    vlc playlist.m3u8
