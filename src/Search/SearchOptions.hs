module Search.SearchOptions where

import           Models.Score (Depth)


data SearchOptions = SearchOptions
  { depth :: Depth
  }


defaultSearchOptions :: SearchOptions
defaultSearchOptions = SearchOptions {
  depth = 12
}


includeDepth :: Depth -> SearchOptions -> SearchOptions
includeDepth depth opts = opts {
  depth = depth
  }
