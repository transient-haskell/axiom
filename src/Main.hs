-- {-# LANGUAGE OverloadedStrings #-}
module Main where
import Haste
import Haste.DOM(withElem)
import View
import Builder
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import Prelude hiding (div)

main= do
  withElem "idelem" $ runWidget $  sum3 **> sum2 **> sum1 **> return ()

sum3 = p  "This widget sums three numbers and append the result" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number  "
             ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
             <*> fromStr "second number "
             ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> noWidget)



sum1 :: View JSBuilder IO ()
sum1 = p  "This widget sums recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `raiseEvent` OnKeyUp
    if r'== 0 then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
              else do
               b (show $ r+r') ++> br ++> return ()
               sumr (r+r')

--sum2 :: view JSBuilder IO ()
sum2 =  p  "This widget sums three numbers and append the result" ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take 3 $ repeat $ inputInt Nothing `raiseEvent` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0


