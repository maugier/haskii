module Haskii.Text where

import Data.Text as T hiding (zip, map)
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.IO as Lazy
import Haskii
import Haskii.Internal
import Haskii.Types

instance Sliceable Text where
    take = T.take
    drop = T.drop
    length = T.length

instance Paddable Text where
    padding n = T.replicate n (T.singleton ' ')

multiline :: Text -> Render Text
multiline t = do
    (c,l) <- oneOf . zip [0..] . T.lines $ t
    drawAt (c,0) l

render :: Render Text -> Lazy.Text
render = Lazy.unlines . map Lazy.fromChunks . renderChunks

putStrLn :: Render T.Text -> IO ()
putStrLn = Lazy.putStrLn . render
