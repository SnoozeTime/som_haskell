module ExtractImage where
import Codec.Picture

isMiddlePixelRed :: FilePath -> IO (Maybe Bool)
isMiddlePixelRed fp = do
	image <- readImage fp
	case image of 
		Left _ -> return Nothing
		Right image' -> return (go image')
	where
		go :: DynamicImage -> Maybe Bool
		go (ImageRGB8 image@(Image w h _)) = 
			Just (isRed (pixelAt image (w `div` 2) (h `div` 2)))
		go _ = Nothing
		isRed :: PixelRGB8 -> Bool
		isRed (PixelRGB8 r g b) = r == maxBound && g == 0 && b == 0

getColors :: FilePath -> IO (Maybe [PixelRGB8])
getColors fp = do
	image <- readImage fp
	case image of 
		Left _ -> return Nothing
		Right image' -> return (go image')
	where
		go :: DynamicImage -> Maybe [PixelRGB8]
		go (ImageRGB8 image@(Image w h _)) = 
			Just [ pixel | x <- [0..(w-1)], y <- [0..(h-1)], let pixel = pixelAt image x y]


pixelToInt :: PixelRGB8 -> (Int, Int, Int)
pixelToInt (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

pixelToDouble :: PixelRGB8 -> (Double, Double, Double)
pixelToDouble (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

getColorsBack :: Maybe [PixelRGB8] -> [(Double, Double, Double)]
getColorsBack Nothing = []
getColorsBack (Just colors) = map pixelToDouble colors




imageCreator :: String -> [(Double, Double, Double)] -> IO ()
imageCreator path colors = writePng path $ generateImage pixelRenderer 4 4
    where pixelRenderer x y = let (r, g, b) = colors !! (x*y + x) in PixelRGB8 (truncate r) (truncate g) (truncate b)


--main = do
--	fp <- getLine
--	result <- getColors fp
--	putStrLn $ show result