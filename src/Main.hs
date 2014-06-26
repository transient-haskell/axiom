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
import qualified Data.Map as V
import Data.Maybe



main= do
   withElem "idelem" . runWidget $

         table  <<<(tr ! atr "style" "vertical-align:top"
                     <<< ((td <<< sumtwonumbers)
                     **>  (td <<< sumfold 3)
                     **>  (td <<< recursivesum))
                **> tr ! atr "style" "vertical-align:top"
                     <<< (td <<< counter 3
                     **>  td <<< sumcell)
                   )
                     **> return ()
                     <++  b << "bottom"

table rows= nelem "table" `child` rows

tr rows= nelem "tr" `child` rows

td e= nelem "td" `child` e


sumtwonumbers :: View Perch IO ()
sumtwonumbers = p  "This widget sumcell two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number"  ++> br
              ++> inputInt Nothing `raiseEvent` OnKeyUp `validate` less3 <++ br
              <*> fromStr "second number " ++> br
              ++> inputInt Nothing `raiseEvent` OnKeyUp <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())

  where
  less3 x= if x < 3 then return Nothing else  return . Just $ b "more than 2"

recursivesum :: View Perch IO ()
recursivesum = p  "This widget sumcell recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `raiseEvent` OnKeyUp
    if r'== 0
      then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
      else do
        b (show $ r+r') ++> br ++> return ()
        sumr (r+r')


sumfold n =  p  ("This widget sum "++ show n ++" numbers and append the result using a fold") ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take n $ repeat $ inputInt Nothing `raiseEvent` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0


counter n =
   (b (show n) ++> onemore) -- (sumfold n **> onemore <++  br)
   `wcallback` (const $ do
      liftIO $ print n
      counter $ n +1)
  where
  onemore=  submitButton "+" `raiseEvent` OnClick


sumcell ::  View Perch IO ()
sumcell = p  "This widget sum recursively n numbers, but remember the\
          \ previos entries when one entry is edited" ++> sumr 0 0
  where
  sumr i r=do
    r' <- mkcell (show i) (\v -> inputInt v `raiseEvent` OnKeyUp) Nothing
    b (show $ r+r') ++> br ++> return ()
    sumr (i +1) (r+r')

mkcell name widget initial =  do
    stored <- Just <$> getNumber name <|> return initial
    r' <- widget stored {- inputInt stored `raiseEvent` OnKeyUp -} <|> fromM stored
    addNumber name r'
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

