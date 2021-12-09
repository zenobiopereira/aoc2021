module Main where

data Movement a
  = Forward a
  | Up a
  | Down a
  | Ignore

type Depth = Int;
type Displacement = Int;
type Aim = Int

parse :: [String] -> Movement Int
parse ["up", x] = (Up . read) x
parse ["down", x] = (Down .read) x
parse ["forward", x] = (Forward . read) x
parse _ = Ignore 

calc :: Displacement -> Depth -> Aim -> [Movement Int] -> (Displacement, Depth)
calc x y aim [] = (x, y);
calc x y aim (m:ms) = 
  calc x' y' aim' ms
  where
    (x', y', aim') = 
      case m of
        Forward d -> (x + d, y + aim * d, aim)
        Up d -> (x, y, aim - d)
        Down d -> (x, y, aim + d)
        Ignore -> (x, y, aim)


main :: IO ()
main =  readFile "input" >>= (print . uncurry (*) . calc 0 0 0 .  map (parse . words) . lines)
  