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
multiline t = WriterT [ (l,(Sum n, Sum 0)) | (n,l) <- zip [0..] $ T.lines t ]

render :: Render Text -> Text
render = T.unlines . map T.concat . renderChunks

putStrLn :: Render Text -> IO ()
putStrLn = T.putStrLn . render
