-- Inf2d Assignment 1 2017-2018
-- Matriculation number:
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Data.Ord
import Debug.Trace
import ConnectFour

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6

{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the 3pm Tuesday 13th March 2018.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.
{-
-- Calculate the the adjacent node to the current node(xs), dependign on the value of x and y
-}
nextNode::Branch-> Int-> Int-> Node
nextNode xs x y = (fst(head xs)+x, snd(head xs)+ y)

next::Branch-> [Branch]
next branch = [(nextNode branch x y):branch|(x,y)<-[(0,1),(1,0),(0,-1),(-1,0)],(not ((nextNode branch x y) `elem` branch)), ((nextNode branch x y) `elem` [(x,y) | x <- [1..6], y <- [1..6]])]
        --    | fst(head(branch))+1 <= 6  and not (fst(head(branch))+1, snd(head(branch))) `elem` branch = (fst(head(branch))+1, snd(head(branch))):branch

-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
checkArrival::Node-> Node-> Bool
checkArrival destination curNode = destination == curNode

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.
breadthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]->[Node]-> Maybe Branch
breadthFirstSearch destination next branches exploredList
   | null branches = Nothing
   | head(head branches) `elem` exploredList = breadthFirstSearch destination next (tail branches) exploredList
   | checkArrival destination (head(head branches)) = Just (head branches)
   | otherwise = breadthFirstSearch destination next (tail branches ++ next (head branches)) (head(head branches) : exploredList)


-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.

depthFirstSearch::Node-> (Branch-> [Branch])-> [Branch]-> [Node]-> Maybe Branch
depthFirstSearch destination next  branches exploredList
   | null branches = Nothing
   | head(head branches) `elem` exploredList = depthFirstSearch destination next (tail branches) exploredList
   | checkArrival destination (head(head branches)) = Just (head branches)
   | otherwise = depthFirstSearch destination next (next (head branches) ++ tail branches) (head(head branches) : exploredList)

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree..

depthLimitedSearch::Node-> (Branch-> [Branch])-> [Branch]-> Int-> [Node]-> Maybe Branch
depthLimitedSearch destination next  branches d exploredList
   | null branches = Nothing
   | (head(head branches) `elem` exploredList)= depthLimitedSearch destination next (tail branches) d exploredList
   | checkArrival destination (head(head branches)) = Just (head branches)
   | otherwise = if (length (head branches)) == d then depthLimitedSearch destination next (tail branches) d exploredList else depthLimitedSearch destination next (tail branches ++ next (head branches)) d exploredList

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by d if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found, the depth should be increased.
iterDeepSearch:: Node-> (Branch-> [Branch])-> Node-> Int-> Maybe Branch
iterDeepSearch destination next initialNode d = if depthLimitedSearch destination next [[initialNode]] d [] == Nothing
                                                then iterDeepSearch destination next initialNode (d+1)
                                                else  depthLimitedSearch destination next [[initialNode]] d []

-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the current position and the destination position.
{-
-- Essentially performs the same function as the manhattan function ex
-}
calcDist::Node->Node->Int
calcDist (x1,y1) (x2,y2) = abs(x1-x2) + abs(y1-y2)

manhattan::Node-> Node-> Int
manhattan position destination = calcDist position destination

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.
bestFirstSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> [Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList
   | null branches = Nothing
   | checkArrival (head (head branches)) destination = Just (head branches)
   | otherwise = bestFirstSearch destination next heuristic (sortBy (comparing (heuristic.head)) (tail(branches) ++ next(head branches))) (head (head branches):exploredList)
   -- | otherwise = map (\xs-> manhattan destination (head xs)) branches

-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

{-
-- Calculates the cost of a node by calculating the sum of values returned by the heuristic and cost functions
-}
totalCost :: (Node->Int)->(Branch->Int)->Branch->Int
totalCost heuristic cost xs = (heuristic(head xs)) + cost xs

aStarSearch:: Node-> (Branch-> [Branch])-> (Node->Int)-> (Branch-> Int)-> [Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList
   | null branches = Nothing
   | checkArrival (head (head branches)) destination = Just (head branches)
   | otherwise = aStarSearch destination next heuristic cost (sortBy (comparing (totalCost heuristic cost)) (tail(branches) ++ next(head branches))) (head (head branches):exploredList)

-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch-> Int
cost branch = length(branch) - 1

-- In this section, the function determines the score of a terminal state, assigning it a value of +1, -1 or 0:
eval :: Game-> Int
eval game
   | checkWin game maxPlayer = 1
   | checkWin game minPlayer = -1
   | otherwise = 0
-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Role-> Game-> Int
minimax player game
   | terminal game = eval game
   | player == maxPlayer = foldl (max) (-2) (map (minimax minPlayer) (moves game player))
   | otherwise = foldl (min) (2) (map (minimax maxPlayer) (moves game player))

-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

{-
-- Calculates the utility value of the maxPlayer node
-}
maxValue:: Game->Int->Int->Int
maxValue state alpha beta
   | terminal state = eval state
   | otherwise = forMax (moves state maxPlayer) (-2) alpha beta

{-
-- Calculates the utility value of the minPlayer node
-}
minValue:: Game->Int->Int->Int
minValue state alpha beta
   | terminal state = eval state
   | otherwise = forMin (moves state minPlayer) (2) alpha beta

{-
-- This performs the function of the forloop inside the maxValue Function of the alphabeta pruning pseudocode
-}
forMax:: [Game]->Int->Int->Int->Int
forMax states v alpha beta
   | null states = v
   | v >=beta = max v (minValue (head states) (max v alpha) beta)
   | otherwise = forMax (tail states) (max v (minValue (head states) (max v alpha) beta))
                                      (max v alpha) beta

{-
-- This performs the function of the forloop inside the minValue Function of the alphabeta pruning pseudocode
-}
forMin:: [Game]->Int->Int->Int->Int
forMin states v alpha beta
   | null states = v
   | v <= alpha = min v (maxValue (head states) alpha (min v beta))
   | otherwise = forMin (tail states) (min v (maxValue (head states) alpha (min v beta)))
                      alpha (min v beta)

alphabeta:: Role-> Game-> Int
alphabeta player game
   | terminal game = eval game
   | player == maxPlayer = maxValue game (-2) (2)
   | otherwise = minValue game (-2) (2)


{- Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms below.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
-}
