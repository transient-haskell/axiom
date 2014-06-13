{-# LANGUAGE OverloadedStrings #-}
module Main where
import Haste
import Haste.DOM(withElem)
import View
import Builder
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid

main= do
  withElem "idelem" $  runWidget $  sum2

sum = do
     r <- (+)
            <$> fromStr "first number  "
            ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
            <*> fromStr "second number "
            ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
     fromStr "result: " ++>  b (show r) ++> noWidget



sum1 :: Int -> View JSBuilder IO ()
sum1 r= do
     r' <- inputInt Nothing `raiseEvent` OnKeyUp
     if r'== 0 then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
                          else do
                           b (show $ r+r') ++> br ++> return ()
                           sum1 (r+r')

sum2 :: view JSBuilder IO ()
sum2 = do
         r <- foldl (<>)  (return 0) . take 3 $ repeat $ inputInt Nothing `raiseEvent` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ()

instance Monoid Int where
  mappend= (+)
  mempty= 0

nelemc :: ToElem a => String ->  a -> JSBuilder
nelemc tag cont= nelem tag `child` cont

b= nelemc "b"



(<<) ce cont= ce cont

