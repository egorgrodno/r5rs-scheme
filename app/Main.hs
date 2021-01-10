module Main where

import           Parser                         ( betweenSepByComma
                                                , digit
                                                , list1
                                                , parse
                                                )

main :: IO ()
main =
  let str = "[ 127, 255, 511, 1023 ]"
      parseInt = (read :: String -> Int) <$> list1 digit
   in putStrLn ("Parsing " ++ show str)
        >> print (parse (betweenSepByComma '[' ']' parseInt) str)
