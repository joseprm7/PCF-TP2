{-# LANGUAGE FlexibleInstances #-}
module Adventurers where

import DurationMonad
import Data.List

-- The list of adventurers
data Adventurer = P1 | P2 | P5 | P10 deriving (Show,Eq)
-- Adventurers + the lantern
type Objects = Either Adventurer ()

-- The time that each adventurer needs to cross the bridge
getTimeAdv :: Adventurer -> Int
getTimeAdv P1 = 1
getTimeAdv P2 = 2 
getTimeAdv P5 = 5
getTimeAdv _  = 10

{-- The state of the game, i.e. the current position of each adventurer
+ the lantern. The function (const False) represents the initial state
of the game, with all adventurers and the lantern on the left side of
the bridge. Similarly, the function (const True) represents the end
state of the game, with all adventurers and the lantern on the right
side of the bridge.  --}
type State = Objects -> Bool

instance Show State where
  show s = (show . (fmap show)) [s (Left P1),
                                 s (Left P2),
                                 s (Left P5),
                                 s (Left P10),
                                 s (Right ())]

instance Eq State where
  (==) s1 s2 = and [s1 (Left P1) == s2 (Left P1),
                    s1 (Left P2) == s2 (Left P2),
                    s1 (Left P5) == s2 (Left P5),
                    s1 (Left P10) == s2 (Left P10),
                    s1 (Right ()) == s2 (Right ())]



-- The initial state of the game
gInit :: State
gInit = const False

-- Changes the state of the game for a given object
changeState :: Objects -> State -> State
changeState a s = let v = s a in (\x -> if x == a then not v else s x)

-- Changes the state of the game of a list of objects 
mChangeState :: [Objects] -> State -> State
mChangeState os s = foldr changeState s os
                               
twrite :: Int -> ListDur a -> ListDur a 
twrite t l = LD $ let l' = remLD l in 
                     map (\(Duration(t',x)) -> Duration (t + t', x)) l' 

possibleStates :: State -> [[Objects]]
possibleStates s = filter (\p -> x p && y1 p && y2 p) . subsequences . filter z $ l
                where
                  x = elem (Right ())
                  y1 = (>) 4 . length
                  y2 = (<) 1 . length
                  z = \x -> s (Right()) == s x
                  l = [Left P1, Left P2, Left P5, Left P10, Right ()]


objectToTime :: Objects -> Int 
objectToTime (Left x) = getTimeAdv x 
objectToTime _ = 0

getStateTime :: [Objects] -> Int 
getStateTime l = foldr (\o -> max (objectToTime o)) 0 l


{-- For a given state of the game, the function presents all the
possible moves that the adventurers can make.  --}
-- To implement
allValidPlays :: State -> ListDur State
allValidPlays s = manyChoice $ 
   map (\l -> twrite (getStateTime l) (return (mChangeState l s))) (possibleStates s)

{-- For a given number n and initial state, the function calculates
all possible n-sequences of moves that the adventures can make --}
-- To implement 
exec :: Int -> State -> ListDur State
exec 0 s = allValidPlays s
exec n s = do s1 <- allValidPlays s
              s2 <- LD $ (remLD s1) ++ (remLD (exec (n-1) s1))
              return s2 

{-- Is it possible for all adventurers to be on the other side
in <=17 min and not exceeding 5 moves ? --}
-- To implement
leq17 :: Bool
leq17 = length (
   filter (\(Duration (t,s)) -> t <= 17 && s == const True) 
      (remLD  (exec 4 gInit))) > 0


{-- Is it possible for all adventurers to be on the other side
in < 17 min ? --}
-- To implement
l17 :: Bool
l17 = length (
   filter (\(Duration (t,s)) -> t < 17 && s == const True) 
      (remLD  (exec 4 gInit))) > 0

--------------------------------------------------------------------------
{-- Implementation of the monad used for the problem of the adventurers.
Recall the Knight's quest --}

data ListDur a = LD [Duration a] deriving Show

remLD :: ListDur a -> [Duration a]
remLD (LD x) = x

-- To implement
instance Functor ListDur where
   fmap f = LD . (map (fmap f)) . remLD

-- To implement
instance Applicative ListDur where
   pure x = LD [ Duration(0,x) ]
   l1 <*> l2 = LD $ do x <- remLD l1
                       y <- remLD l2
                       return $ x <*> y 
                         

-- To implement
instance Monad ListDur where
   return = pure
   l >>= k = LD $ do x <- remLD l
                     g x where
                        g (Duration (d, a)) = let u = remLD (k a) in 
                           map (\(Duration (d', a)) -> (Duration (d + d', a))) u


manyChoice :: [ListDur a] -> ListDur a
manyChoice = LD . concat . (map remLD)
--------------------------------------------------------------------------
