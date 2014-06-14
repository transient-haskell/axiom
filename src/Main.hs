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


main= withElem "idelem" $ runWidget $ sumv 2


main2= withElem "idelem" . runWidget $
         table  <<<(tr ! atr "style" "vertical-align:top"
                     <<< ((td <<< sum3)
                     **>  (td <<< sumn 3)
                     **>  (td <<< sum1))  )
--                **> tr ! atr "style" "vertical-align:top"
--                     <<< td <<< sumv 3)
--                     **> return ()

table rows= nelem "table" `child` rows

tr rows= nelem "tr" `child` rows

td e= nelem "td" `child` e



sum3 = p  "This widget sums two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number" ++> br
              ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
              <*> fromStr "second number " ++> br
              ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())



sum1 :: View JSBuilder IO ()
sum1 = p  "This widget sums recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `raiseEvent` OnKeyUp
    if r'== 0 then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
              else do
               b (show $ r+r') ++> br ++> return ()
               sumr (r+r')


sumn n =  p  "This widget sums three numbers and append the result using a fold" ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take n $ repeat $ inputInt Nothing `raiseEvent` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0


sumv n = do
    sumn n **> onemore <++  br
    liftIO $ print $ "reexec"
    sumv $ n +1
  where
  onemore= submitButton "+" `raiseEvent` OnClick
