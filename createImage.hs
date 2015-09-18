import Codec.Picture

imageCreator :: String -> IO ()
imageCreator path = writePng path $ generateImage pixelRenderer 40 40
    where pixelRenderer x y = PixelRGB8 (fromIntegral x) (fromIntegral y) 128

main =
    imageCreator "test.png"