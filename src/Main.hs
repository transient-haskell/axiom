-- {-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Main where
import Haste
import Haste.DOM(withElem)
import View
import Haste.Perch
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import Prelude hiding (div)
import qualified Data.IntMap as V
import Data.Maybe



main= do
   withElem "idelem" . runWidget $
         table  <<<(tr ! atr "style" "vertical-align:top"
                     <<< ((td <<< sum3)
                     **>  (td <<< sumn 3)
                     **>  (td <<< sum1))
                **> tr ! atr "style" "vertical-align:top"
                     <<< (td <<< sumv 3
                     **> td <<< sums)
                   )
                     **> return ()
                     <++ b << "bottom"

table rows= nelem "table" `child` rows

tr rows= nelem "tr" `child` rows

td e= nelem "td" `child` e


sum3 :: View Perch IO ()
sum3 = p  "This widget sums two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number"  ++> br
              ++> inputInt Nothing `raiseEvent` OnKeyUp `validate` less3 <++ br
              <*> fromStr "second number " ++> br
              ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())

  where
  less3 x= if x < 3 then return Nothing else  return . Just $ b "more than 2"

sum1 :: View Perch IO ()
sum1 = p  "This widget sums recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `raiseEvent` OnKeyUp
    if r'== 0
      then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
      else do
        b (show $ r+r') ++> br ++> return ()
        sumr (r+r')


sumn n =  p  ("This widget sums "++ show n ++" numbers and append the result using a fold") ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take n $ repeat $ inputInt Nothing `raiseEvent` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0


sumv n =
    (sumn n **> onemore <++  br)
   `wcallback` const (sumv $ n +1)
  where
  onemore= submitButton "+" `raiseEvent` OnClick


sums ::  View Perch IO ()
sums = p  "This widget sums recursively n numbers, but remember the\
          \ previos entries when one entry is edited" ++> sumr 0 0
  where
  sumr i r=do
    r' <- cell i
    b (show $ r+r') ++> br ++> return ()
    sumr (i +1) (r+r')

cell i=  do
    stored <- Just <$> getNumber i <|> return Nothing
    r' <- inputInt stored `raiseEvent` OnKeyUp <|> fromM stored
    addNumber i r'
    return r'
  where
  addNumber i x= do
       xs <- getSData <|> return  V.empty
       setSData $ V.insert i x xs

--  getNumber :: Int -> View Perch IO Int
  getNumber i= do
       xs <- getSData
       case  V.lookup i xs of
         Nothing -> empty
         Just x  -> return x

  fromM Nothing = empty
  fromM (Just x) = return x




