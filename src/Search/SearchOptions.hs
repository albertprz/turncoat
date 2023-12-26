module Search.SearchOptions where

import AppPrelude


data SearchOptions = SearchOptions
  { depth :: Int
  }


defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions {
  depth = 4
}
  

includeDepth :: Int -> SearchOptions -> SearchOptions
includeDepth depth opts = opts {
  depth = depth
  }
