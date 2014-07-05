{-# LANGUAGE  DeriveDataTypeable #-}
module Main where
import Haste
import Haste.DOM(withElem)
import Haste.Graphics.Canvas
import View
import Cell
import Haste.Perch
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad
import Data.Monoid
import Data.Typeable
import Prelude hiding (div)
import qualified Data.Map as V
import Data.Maybe
import Data.Default


main= do
   withElem "idelem" . runWidget $
        h1 ! style "text-align:center" << "hplayground examples" ++>
        (table  <<<(tr ! atr "style" "vertical-align:top"
                     <<< ((td <<< sumTwo)
                     <|>  (td <<< sumfold 3)
                     <|>  (td <<< sumRecursive))
                <|> tr ! atr "style" "vertical-align:top"
                     <<<  (td <<< counter 3
                     <|>   td <<< sumCell
                     <|>   td <<< showpascal 4)
                <|> tr ! style "vertical-align:top"
                     <<<  (td <<< drawcanvas
                     <|>   td <<< gallery
                     <|>   td <<< mouse )
                   ))

        <++  b << "bottom of the page"

-- Dont't be scared by the operators:
-- <|> is the Alternantive combinator, to combine Widget() entries
-- and the <<< combinator simply encloses a widget within a HTML tag.
-- ++> prepend HTML to a widget
-- <++ postpend it
--


sumTwo :: Widget ()
sumTwo = p  "This widget sum two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number"  ++> br
                   ++> inputInt Nothing `raiseEvent` OnKeyUp  <++ br
              <*> fromStr "second number " ++> br
                   ++> inputInt Nothing `raiseEvent` OnKeyUp  <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())

  where
  less3 x= if x < 3 then return Nothing else  return . Just $ b " no more than 2 please"

sumRecursive :: Widget ()
sumRecursive = p  "This widget sum recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `raiseEvent` OnKeyUp
    if r'== 0
      then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
      else do
        b (show $ r+r') ++> br ++> return ()
        sumr (r+r')

sumfold :: Int -> Widget ()
sumfold n =  p  ("This widget sum "++ show n ++" numbers and append the result using a fold") ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take n $ repeat $ inputInt Nothing `raiseEvent` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0





counter :: Int -> Widget ()
counter n = p " A counter. wcallback erases the previous rendering of the widget an regenerates it again "
            ++> br ++> counter1 n
 where
 counter1 n= (b (show n) ++> onemore)  `wcallback` (const $ counter1 $ n +1)

 onemore=  submitButton "+" `raiseEvent` OnClick






sumCell :: Widget ()

sumCell = p  "This widget sum recursively n numbers, but remember the\
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

   getNumber :: Int -> Widget Int
   getNumber i= do
        xs <- getSData
        case  V.lookup i xs of
          Nothing -> empty
          Just x  -> return x

   fromM Nothing = empty
   fromM (Just x) = return x







-- pascal triangle http://www.haskell.org/haskellwiki/Blow_your_mind

pascal = iterate (\row -> zipWith (+) ([0] ++ row) (row ++ [0])) [1] :: [[Int]]

showpascal n= p << ("Show " ++ show n ++ " rows of the Pascal triangle ")
   ++> mconcat[p ! atr "style" "text-align:center" $ row | row <- take n pascal]
   ++> empty   -- the applicative empty === noWidget







drawcanvas :: Widget ()
drawcanvas=
 p << "This example draw a function of x between 10 and -10. You can define the function\
      \ using javascript expressions"  ++>

 (center <<< do
      let initial= "Math.pow(x,2)+x+10;"
      expr <- inputString (Just initial) `raiseEvent` OnKeyUp <++ br <|> return initial
      wraw $ canvas ! Haste.Perch.id "canvas"  $ noHtml
      wraw $ draw expr)


  where

  draw expr= liftIO $ do
    Just can <- getCanvasById "canvas"
    let range= [-10..10]
        exprs = map (\v -> subst 'x' (show v) expr) range
    ps <- mapM evalFormula  exprs
    render can $ scale (3,1) $ translate (50,130) $ rotate pi $ stroke $do
        line (-10,0) (10,0)
        line (0,30) (0,-30)
        path $ zip range ps

  subst _ _ []= []
  subst c v (c':cs)
    | c==c' =  v++ subst c v cs
    | otherwise= c':subst c v cs







newtype GalleryIndex= G Int deriving Typeable

gallery :: Widget ()
gallery = p "this example show a image gallery. It advances each 20 seconds and by\
               \ pressing the button" ++>
 (wtimeout 20000 $ do
  G i <- getSData <|> return (G 0)
  let i' = if i == length gall-1  then 0 else  i+1
  setSData $ G i'
  wraw $ do
      img ! src (gall !! i) ! width "100%" ! height "100%" $ noHtml   -- raw Perch code
      br
  submitButton ">" `raiseEvent` OnClick
 `wcallback` \_ -> gallery)

  where
  gall=["http://almaer.com/blog/uploads/interview-haskell.png"
       ,"https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQmmC4kV3NPFIpGL_x4H_iHG_p-c93DGjWfkxVtjxEFVng7A8o-nw"
       ,"https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcQ_ywj-zxDq3h_B4l48XHsjTywrdbK5egxvhxkYJ1HOkDFXd_-H"
       ,"https://encrypted-tbn3.gstatic.com/images?q=tbn:ANd9GcS53axpzkDyzEUAdaIP3YsaHuR-_YqN9qFK3W4bp_D2OBZfW5BU_Q"
       ,"https://encrypted-tbn1.gstatic.com/images?q=tbn:ANd9GcSfP70npv4FOrkBjScP0tVu2t3veSNoFQ6MMxX6LDO8kldNeu-DxQ"
       ,"https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRAgKkpDyzk8kdIqk5ECsZ14XgbpBzyWFvrCrHombkSBAUn6jFo"
       ]


mouse :: Widget ()
mouse= do
    wraw (div  ! style "height:100px;background-color:green" $ "mouse events here")
                            `raiseEvent` OnMouseOut
                            `raiseEvent` OnMouseOver
                            `raiseEvent` OnMouseDown
                            `raiseEvent` OnMouseMove
                            `raiseEvent` OnMouseUp
                            `raiseEvent` OnClick
                            `raiseEvent` OnDblClick
                            `raiseEvent` OnKeyPress
    evdata  <- getEventData
    wraw $ p << ( (evName evdata) ++" "++ show (evData evdata))

