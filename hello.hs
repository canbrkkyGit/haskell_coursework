{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use guards" #-}

import Data.List



eliminate xs = [x | (x:y:_) <- tails xs, x<y] --4 with Guard, it always iterates with duples



compute :: [(Int,Int)] -> [Int]
compute lst = [if y>x then x*y else x+y | (x,y)<-lst] --3 Pattern matching


gpa :: [(String,Float,String)] -> Float
gpa lst = sum [if z == "A" then 4*y else (if z=="B" then 3*y else (if z=="C" then 2*y else y)) | (_,y,z)<-lst] / sum [y | (_,y,z)<-lst] --5 Pattern matching


classifyG :: (RealFloat a) => a -> String  
classifyG score  
    | score >= 90.0 = "A"  
    | score >= 80.0 && score<=89.99 = "B"  
    | score >= 70.0 && score<=79.99 = "C"  
    | otherwise   = "D"  --8 with Guard


classifyI :: Integer -> String
classifyI score =
    if score >= 90
        then "A"
        else
          if score >= 80
            then "B"
            else 
                if score >= 70
                    then "C"
                    else "D" --9 with if-else conditionals

                        

    
    
        
    

firstOdds n  = take n [1,3..] -- 10


fltr f lst = [x | x <- lst, f x] -- 2

howMany :: Eq a => a -> [a] -> Int
howMany x = length . filter (x==) -- 6

pairLists lst1 lst2 = zip lst1 lst2 --7

order :: Ord a => (a,a,a) -> (a,a,a) 
order (a,b,c) = (x,y,z) 
    where 
        [x,y,z] = sort [a,b,c] --1



