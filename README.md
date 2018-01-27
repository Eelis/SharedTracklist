This tool:

- is run once a day by Eelis (or whoever)
- downloads track list (containing urls and (dis)likes) that we collaboratively edit in a gitlab repo, in a subset of markdown format
- supports arbitrary vlc-supported urls in track list
- maintains cache of downloaded tracks (needed for e.g. youtube, which vlc's native support for seems broken)
- picks random tracks
- generates an extended m3u8 playlist for use with e.g. vlc
- has special support for youtube/github/gitlab urls

Benefits of the approach:

- subset of markdown means list is rendered nicely on gitlab, no need for separate hosting
- we get all the features of version control for free: backup, blaming, reverting, forking, etc.
- grouping using headers (e.g. "# Mahtz Mixes") (naturally extends to group-likes)
- not committed to any playback/streaming system. m3u8 playlists importable with everything
- easy to perform edits such as "remove all of X's votes" when X leaves the company
- easy to run a link checker on the rendered track list
