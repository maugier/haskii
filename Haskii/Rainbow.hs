module Haskii.Rainbow where

import Data.ByteString as BS
import Haskii.Types
import Lens.Simple
import Rainbow

instance Sliceable a => Sliceable (Chunk a) where
    take = over yarn . take
    drop = over yarn . drop
    length = length . view yarn 

instance Paddable a => Paddable (Chunk a) where
    sPad = chunk . sPad 


colorRender8 :: (Renderable t, Paddable t) => Render (Chunk t) -> [ByteString]
colorRender8 = concat . map (++[ pack "\n" ]) . map colorize . render where
    colorize = chunksToByteStrings toByteStringsColors8

colorPrint8 :: Render (Chunk Text) -> IO ()
colorPrint8 = mapM_ BS.putStr . colorRender8
