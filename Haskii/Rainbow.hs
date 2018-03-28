import Data.ByteString as BS
import Rainbow

instance Sliceable a => Sliceable (Chunk a) where
    sTake = over yarn . sTake
    sDrop = over yarn . sDrop
    sLength = sLength . view yarn 

instance Paddable a => Paddable (Chunk a) where
    sPad = chunk . sPad 


colorRender8 :: (Renderable t, Paddable t) => Render (Chunk t) -> [ByteString]
colorRender8 = concat . map (++[ pack "\n" ]) . map colorize . render where
    colorize = chunksToByteStrings toByteStringsColors8

colorPrint8 :: Render (Chunk Text) -> IO ()
colorPrint8 = mapM_ BS.putStr . colorRender8
