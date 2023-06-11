module Parts.Builder.NonMonadic where

import Data.Tagged (Tagged(..))

type ABuilder t b = Tagged b (b -> b) -> Tagged t (b -> t)

aBuilder :: (b -> t) -> ABuilder t b
aBuilder bt _ = Tagged bt

build :: ABuilder t b -> b -> t
build l = unTagged (l $ Tagged id)
