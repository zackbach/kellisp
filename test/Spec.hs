{-# OPTIONS_GHC -F -pgmF hspec-discover #-}

{-
This allows Hspec to search for relevant .spec files,
meaning we can have testing directory that matches our
actual project.

Note that HLS will given an error when the preprocessor,
in this case hspec-discover, is not in the PATH. To fix
this, you can simply install hspec-discover globally with
stack or cabal (as long as the install location is in $PATH).
-}
