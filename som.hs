import System.Random
import ExtractImage
import Codec.Picture

-- Some constants
iters :: Double
iters = 10000

networkWidth :: Double
networkWidth = 4

networkHeight :: Double
networkHeight = 4

mapRadius :: Double
mapRadius = 0.8 * (max networkWidth networkHeight)

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

-- Save some resources
distanceSquareBetweenNodes :: Node a -> Node a -> Double
distanceSquareBetweenNodes (Node xa ya _) (Node xb yb _) =  (xa - xb) * (xa - xb) + (ya - yb) * (ya - yb)
 

-----------------------------------------------------------------------------------------------------------------
-- Input for our network

data Color = Color { r :: Double
                   , g :: Double
                   , b :: Double}
                   deriving (Eq, Ord, Show, Read)

(<+>) :: Color -> Color -> Color
(<+>) (Color ra ga ba) (Color rb gb bb) = (Color (ra + rb) (ga + gb) (ba + bb))

(<->) :: Color -> Color -> Color
(<->) (Color ra ga ba) (Color rb gb bb) = (Color (ra - rb) (ga - gb) (ba - bb))

multColorByScalar :: Color -> Double -> Color
multColorByScalar (Color r g b) scalar = (Color (r*scalar) (g*scalar) (b*scalar))

distanceBetweenColors :: Color -> Color -> Double
distanceBetweenColors (Color ra ga ba) (Color rb gb bb) = sqrt ( (ra - rb) * (ra - rb) + (ga - gb) * (ga - gb) + (ba - bb) * (ba - bb))

--Give back a random color and a new random generator
randomColor :: StdGen -> (Color, StdGen)
randomColor gen = 
	let
	    (r, newGen) = randomR (0, 255) gen
	    (g, newGen') = randomR (0, 255) newGen
	    (b, newGen'') = randomR (0, 255) newGen'
	in ((Color r g b), newGen'')

-- Many random colors yay
randomColors :: Double -> StdGen -> ([Color], StdGen)
randomColors 0 gen = ([], gen) 
randomColors n gen =
	let (color, newGen) = randomColor gen
	    (colors, newGen') = randomColors (n - 1) newGen
	in (color : colors, newGen')

-----------------------------------------------------------------------------------------------------------------

closestNode :: Color -> Node Color -> Node Color -> Node Color
closestNode c (Node xa ya ca) (Node xb yb cb) = if (distanceBetweenColors c ca) > (distanceBetweenColors c cb)
	                                            then (Node xb yb cb)
	                                            else (Node xa ya ca)

findBmi :: Color -> [Node Color] -> Node Color
findBmi c [] = (Node 0 0 c)
findBmi c network =
	foldl1 (closestNode c) network

-- Find the neighbour of a node
-- first Node is the bmi, second is the node to test
-- Third argument is the iteration number. Neighbourhood shrinks over time
isInNeighbourhood :: (Node a) -> (Node a) -> Double -> Bool
isInNeighbourhood bmi node s = (distanceBetweenNodes bmi node) * (distanceBetweenNodes bmi node) < neighbourhood * neighbourhood
	                        where neighbourhood = decay_function mapRadius s

-- Change the weight of a node 
-- node :: Node a is the node we want to change
-- bmi :: Node a is the closer node to the input
-- input :: a is the input we want to get closer to
-- s :: Double is the current iteration
-- Return the new node
adjustWeight :: Node Color -> Node Color -> Color -> Double -> Node Color
adjustWeight node bmi input s = (Node (x node) (y node) (computeWeight (weights node) (distanceSquareBetweenNodes node bmi) s input))

computeWeight :: Color -> Double -> Double -> Color -> Color
computeWeight w dsquare iter input = w <+> (multColorByScalar ( input <-> w) ((decay_function learningRate iter) * ( exp ( - dsquare / ( 2 * mapRadius * mapRadius)) )))
		                                              
-- Apply one iteration of the SOM algorithm 
-- Args: Network of node color (for the moment)
--		 inputs :: [Color] is the learning set of the network	
--       gen :: StdGen is necessary for picking a random vector in the inputs list
--       s :: Double : current interation
-- Return the network after one iteration and a new random generator.	 
epoch :: [Node Color] -> [Color] -> StdGen -> Double -> ([Node Color], StdGen)
epoch [] _ gen _ = ([], gen)
epoch _ [] gen _ = ([], gen)
epoch network inputs gen s =
	let
	    (random_index, newGen) = randomR (0, (length inputs) - 1) gen
	    random_color = inputs !! random_index
	    bmi = findBmi random_color network
	    new_network = map (\node -> if (isInNeighbourhood bmi node s) then (adjustWeight node bmi random_color s) else node) network
	in (new_network, newGen)

-- -----------------------------------------------------------------------------------------------------------------------------------------
-- get a network with random values
getNetwork :: StdGen -> ([Node Color], StdGen)
getNetwork gen = 
	let (colors, newGen) = randomColors (networkHeight * networkWidth) gen
	in ([(Node x y (colors !! index)) | x <- [1..networkWidth], y <- [1..networkHeight], let index = truncate (x*(y-1) + x - 1)], newGen)

-- Create input array from triplet of double
createInputColors :: [(Double, Double, Double)] -> [Color]
createInputColors doubles = map toColor doubles
    where toColor :: (Double, Double, Double) -> Color
          toColor (r, g, b) = (Color r g b)


launchAlgo :: [Color] -> IO [Node Color]
launchAlgo [] = return []
launchAlgo inputs = do
	gen <- getStdGen
	let (network, new_gen) = getNetwork gen
	go 1 inputs network new_gen
	where
		go s inputs network gen | s < iters = let (new_network, newGen) = epoch network inputs gen s in go (s+1) inputs new_network newGen
		                        | otherwise = return network


displayColor :: Node Color -> (Double, Double, Double)
displayColor (Node _ _ c) = (r c, g c, b c)

main = do
	fp <- getLine
	result <- getColors fp
	let colors = createInputColors $ getColorsBack result
	network <- launchAlgo colors
	--mapM (putStrLn . show . displayColor) network
	--putStrLn (show $ length network)
	imageCreator "lol.png" (map displayColor network)