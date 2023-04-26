# Easy file handling for Haskell

This is a Haskell package of cross-platform file handling for Unix/Mac/Windows.

The current Haskell modules have following shortcomings:

- getModificationTime exists in System.Directory. But getAccessTime,
  getStatusChangeTime, getCreationTime do not exist.

- getModificationTime returns obsoleted type, ClockTime. It should
  return modern type, UTCTime, I believe.

- Some file functions are missing. A function to tell the link counter,
  for instance.

- Path separator is not unified. Even though Windows accepts '/' as a
  file separator, getCurrentDirectory in System.Directory returns '\\'
  as a file separator. So, we need to specify regular expression like
  this: "[/\\]foo[/\\]bar[/\\]baz".

- getHomeDirectory returns HOMEDRIVE/HOMEPATH instead of the HOME
  environment variable on Windows.

This package aims to resolve these problems.
