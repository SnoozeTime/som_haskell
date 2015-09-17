import System.Random

-- Some constants
iters :: Double
iters = 100

networkWidth :: Double
networkWidth = 5

networkHeight :: Double
networkHeight = 5

mapRadius :: Double
mapRadius = 0.5 * (max networkWidth networkHeight)

learningRate :: Double
learningRate = 0.1

-- ------------------------------------------------------------------------------------------------------------
-- The time constants and decay functions for 
-- 1) neighbourhood of the Best matching unit
-- 2) learning rate in the adjust weights function
-- ------------------------------------------------------------------------------------------------------------
timeConstant :: Double ->Double
timeConstant 0 = 1
timeConstant initial_value = iters / ( log initial_value )

-- Used for neighbourhood and learning rate.
decay_function :: Double -> Double -> Double
decay_function 0 _ = 0
decay_function initial_value iteration = initial_value * exp ( - iteration / (timeConstant initial_value))

-- --------------------------------------------------------------------------------------------------------------
-- Model of a Node. It has two double coordinates (x and y) and has a weight vector.
-- This weight vector is the same type than the network inputs
-- --------------------------------------------------------------------------------------------------------------

data Node a = Node { x :: Double
                   , y :: Double
                   , weights :: a}
                   deriving (Show, Read)

-- Distance between nodes (geometric with x and y) - a does not matter here
distanceBetweenNodes :: Node a -> Node a -> Double
distanceBetweenNodes (Node xa ya _) (Node xb yb _) = sqrt ( (xa - xb) * (xa - xb) + (ya - yb) * (ya - yb))
 

-----------------------------------------------------------------------------------------------------------------
-- Input for our network

data Color = Color { r :: Double
                   , g :: Double
                   , b :: Double}
                   deriving (Eq, Ord, Show, Read)

distanceBetweenColors :: Color -> Color -> Double
distanceBetweenColors (Color ra ga ba) (Color rb gb bb) = sqrt ( (ra - rb) * (ra - rb) + (ga - gb) * (ga - gb) + (ba - bb) * (ba - bb))

-----------------------------------------------------------------------------------------------------------------

closestNode :: Color -> Node Color -> Node Color -> Node Color
closestNode c (Node xa ya ca) (Node xb yb cb) = if (distanceBetweenColors c ca) > (distanceBetweenColors c cb)
	                                            then (Node xb yb cb)
	                                            else (Node xa ya ca)

findBmi :: Color -> [Node Color] -> Node Color
findBmi c [] = (Node 0 0 c)
findBmi c network =
	foldl1 (closestNode c) network

		                                              
-- Apply one iteration of the SOM algorithm 
-- Args: Network of node color (for the moment)
--		 inputs :: [Color] is the learning set of the network	
--       gen :: StdGen is necessary for picking a random vector in the inputs list
--       s :: Double : current interation
-- Return the network after one iteration and a new random generator.	 
epoch :: [Node Color] -> [Color] -> StdGen -> [Double] -> ([Node Color], StdGen)
epoch [] _ gen _ = ([], gen)
epoch _ [] gen _ = ([], gen)
epoch network inputs gen s =
	let
	    randomColor = (Color 0 0 0)
	    bmi = findBmi randomColor network
	in (network, gen)


main = do
	initial_value_string <- getLine
	let initial_value = read initial_value_string :: Double
	    decay_values = map (decay_function initial_value) [1..iters]
	mapM_ (putStrLn . show) decay_values