{-# LANGUAGE  DeriveDataTypeable #-}
module Main where
import Haste
import Haste.Foreign
import Haste.Graphics.Canvas
import Haste.HPlay.View
import Haste.HPlay.Cell as Cell
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Typeable
import Prelude hiding (div,all,id,print,getChar, putStr, putStrLn,getLine)
import qualified Data.Map as V
import Data.Maybe
import Data.List(isInfixOf)


main= runBody $ do  -- PerchM monad
      h1 ! style "text-align:center" $ "hplayground examples"
      h3 $ center $ do
         a ! href "https://github.com/agocorona/hplayground" $ "Git repository"
         toElem "   "
         a ! href "https://github.com/agocorona/hplayground/blob/master/src/Main.hs" $ "Examples code"
         toElem "   "
         a ! href "haskell-web.blogspot.com.es/2014/07/hplayground-translate-your-console.html" $ "Article"
    ++> do   -- Widget monad
        (table ! style "border-collapse:collapse"
                <<<(tr ! style "vertical-align:top"
                     <<< ( tds <<< sumTwo
                     <|>   tds <<< sumfold 3
                     <|>   tds <<< sumRecursive)
                <|> tr ! style "vertical-align:top"
                     <<<  (tds <<< (counter 3 <|> buttons <|> linksample)
                     <|>   tds <<< sumCell
                     <|>   tds <<< showpascal 4)
                <|> tr ! style "vertical-align:top"
                     <<<  (tds <<< formWidget
                     <|>   tds <<< palindrome
                     <|>   tds <<< naiveTodo)
                <|> tr ! style "vertical-align:top"
                     <<<  (tds <<< drawcanvas
                     <|>   tds <<< gallery
                     <|>   tds <<< mouse )

                   ))

        <++  b << "bottom of the page"
   where
   tds= td ! style "padding:15px;border-style:dotted"

-- Don't be scared by the operators:
-- <|> is the Alternantive combinator, to combine Widget() entries
-- and the <<< combinator simply encloses a widget within a HTML tag.
-- ++> prepend HTML to a widget
-- <++ postpend it



sumTwo :: Widget ()
sumTwo = p  "This widget sum two numbers and append the result. Using applicative and monadic expressions" ++>
  (p <<< do
     r <- (+) <$> fromStr "first number"  ++> br
                   ++> inputInt Nothing `fire` OnKeyUp  <++ br
              <*> fromStr "second number " ++> br
                   ++> inputInt Nothing `fire` OnKeyUp  <++ br
     p <<< fromStr "result: " ++>  b (show r) ++> return())

  where
  less3 x= if x < 3 then return Nothing else  return . Just $ b " no more than 2 please"

sumRecursive :: Widget ()
sumRecursive = p  "This widget sum recursively n numbers. When enters 0, present the result" ++> sumr 0
  where
  sumr r=do
    r' <- inputInt Nothing `fire` OnKeyUp
    if r'== 0
      then  br ++> fromStr "result: " ++>  b (show r) ++> noWidget
      else do
        b (show $ r+r') ++> br ++> return ()
        sumr (r+r')

sumfold :: Int -> Widget ()
sumfold n =  p  ("This widget sum "++ show n ++" numbers and append the result using a fold") ++>
       (p <<< do
         r <- foldl (<>)  (return 0) . take n $ repeat $ inputInt Nothing `fire` OnKeyUp <++  br
         br ++> fromStr "result: " ++>  b (show r) ++> return ())

instance Monoid Int where
  mappend= (+)
  mempty= 0



newtype Counter= Counter Int deriving Typeable

counter n = p "Two counters. One is pure and recursive, the other is stateful"
            ++> br ++> (center <<< (counter1 n <|> counter2 n))

 where
    -- a "pure" counter
    counter1 :: Int -> Widget ()
    counter1 n= (b (show n) ++> onemore)  `wcallback` (const $ counter1 $ n +1)

    onemore=  submitButton "+" `fire` OnClick


    -- a stateful counter
    counter2 n = do
      onemore -- isEmpty (getSData :: Widget Counter ) <|> (onemore >> return True)
      Counter n <- getSData <|> return (Counter n)
      wraw $ b (show n)
      setSessionData . Counter $ n +1



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
     r' <- inputInt stored `fire` OnKeyUp <|> fromM stored
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
   ++> mconcat[p ! style "text-align:center" $ row | row <- take n pascal]
   ++> empty   -- the applicative empty === noWidget




drawcanvas :: Widget ()
drawcanvas=
 p << "This example draw a function of x between 10 and -10. You can define the function\
      \ using javascript expressions"  ++>

 (center <<< do
      let initial= "x*x+x+10;"
          sanitize str= if isInfixOf "alert" str then initial else str
      expr <- inputString (Just initial) `fire` OnKeyUp <++ br <|> return initial
      wraw $ canvas ! id "canvas"  $ noHtml
      wraw $ draw $sanitize expr)


  where

  evalFormula :: String  -> IO Double
  evalFormula= ffi $ toJSString "(function(exp){ return eval(exp);})"

  draw expr= liftIO $ do
    Just can <- getCanvasById "canvas"
    let range= [-10..10]
        exprs = Prelude.map (\v -> subst 'x' (show v) expr) range
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
      img ! src (gall !! i) ! width "100%" ! height "100%"    -- raw Perch code
      br
  submitButton ">" `fire` OnClick
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
    wraw (div  ! style "height:100px;background-color:lightgreen;position:relative"
               $ h1 "Mouse events here")
                            `fire` OnMouseOut
                            `fire` OnMouseOver
                            `fire` OnMouseDown
                            `fire` OnMouseMove
                            `fire` OnMouseUp
                            `fire` OnClick
                            `fire` OnDblClick
                            `fire` OnKeyPress
                            `fire` OnKeyDown
                            `fire` OnKeyUp
    evdata  <- getEventData
    wraw $ p << ( (evName evdata) ++" "++ show (evData evdata))

linksample= br ++> wlink "Hi!" (toElem "This link say Hi!")`fire` OnClick >>= \r -> wraw( b (" returns "++ r))

buttons= p "Different input elements:" ++> checkButton
                                       **> br ++> br
                                       ++> radio
                                       **> br ++> br
                                       ++> select
                                       <++ br
    where
    checkButton=do
       rs <- getCheckBoxes(
                       ((setCheckBox False "Red"    <++ b "red")   `fire` OnClick)
                    <> ((setCheckBox False "Green"  <++ b "green") `fire` OnClick)
                    <> ((setCheckBox False "blue"   <++ b "blue")  `fire` OnClick))
       wraw $ fromStr " returns: " <> b (show rs)

    radio= do
       r <- getRadio [\n -> fromStr v ++> setRadioActive v n | v <- ["red","green","blue"]]

       wraw $ fromStr " returns: " <> b ( show r )

    select= do
       r <- getSelect (   setOption "red"   (fromStr "red")  
                      <|> setOption "green" (fromStr "green")
                      <|> setOption "blue"  (fromStr "blue"))
              `fire` OnClick

       wraw $ fromStr " returns: " <> b ( show r )

formWidget=  center <<< do -- PerchM monad
      p "Fields of a form appear in sequence. \
        \Some of the fields trigger events instantly. Some others use a button to trigger them. \
        \It also contains option buttons, radio buttons etc"
      p $ do
        toElem "This formulary is the same than the one "
        a ! href "http://mflowdemo.herokuapp.com/noscript/monadicwidgets/combination"
          $ "run in the server by MFlow"
  ++>
   do
      (n,s) <- (,) <$> p << "Who are you? "
                   ++> getString Nothing <! hint "name"     <++ br
                   <*> getString Nothing <! hint "surname"  <++ br
                   <** submitButton "ok" `fire` OnClick <++ br

      flag <- b << "Do you " ++> getRadio[radiob "work?",radiob "study?"] <++ br

      r<- case flag of
         "work?" -> Left  <$> b << "do you enjoy your work? "
                              ++> getBool True "yes" "no"
                              <** submitButton "ok" `fire` OnClick <++ br

         "study?"-> Right <$> b << "do you study in "
                              ++> getRadio[radiob "University"
                                          ,radiob "High School"]

      p << ("You are "++n++" "++s)

        ++> case r of
             Left fl ->   p << ("You work and it is " ++ show fl ++ " that you enjoy your work")
                            ++> noWidget

             Right stu -> p << ("You study at the " ++ stu)
                            ++> noWidget

  where
  hint s= [("placeholder",s)]
  radiob s n=  wlabel  (fromStr s) $ setRadioActive s n


palindrome= p << "To search palindromes: one box present the other's reversed. It is also\
                 \ an example of cell usage" ++> do
 let entry= boxCell "entry" :: Cell String
     revEntry= boxCell "revEntry"

 r <- center <<< ((mk entry    Nothing `fire` OnKeyUp >>= return . Left) <++ br <|>
                  (mk revEntry Nothing `fire` OnKeyUp >>= return . Right))

 case r of
  Left s  -> do
   r  <- Cell.get entry >>= return . reverse
   revEntry .= r
  Right s -> do
   r' <- Cell.get revEntry >>= return . reverse
   entry .= r'


newtype Tasks = Tasks [String] deriving Typeable

data TaskAction =  Done | Destroy

naiveTodo= do
 p $ do   -- PerchM monad
     toElem "Work in progress for a todo application to be added to "
     a ! href "http://todomvc.com" $ "todomvc.com"
 ++> todos

 where

 todos= center <<< h1 "todos" ++>
     do
      let entry = boxCell "todo"
      task <- mk entry Nothing `fire` OnKeyUp
      EventData _ (Key k) <- getEventData
      when( k == 13)  $ do
         entry .= ""
         Tasks tasks <- getSData <|> return (Tasks [])
         setSData . Tasks $ task:tasks

         at "list" Insert $ foldl (<|>) mempty [ display t | t <- (task:tasks)]
         return ()
     <|>
      wraw (div ! id "list" $ noHtml)



 display task=  li <<< (do
  ch <- getCheckBoxes $ setCheckBox False "check" `fire` OnClick
  case ch of
        ["check"] -> wraw  $ b ! style "text-decoration:line-through;" $ task
        _         -> wraw  $ b task

                                  )


