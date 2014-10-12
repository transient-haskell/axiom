-----------------------------------------------------------------------------
--
-- Module      :  View
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | The haste-hplayground framework.  <http://github.com/agocorona/hplayground>
--
-----------------------------------------------------------------------------

{-# LANGUAGE  FlexibleContexts, FlexibleInstances
    , TypeFamilies, DeriveDataTypeable, UndecidableInstances, ExistentialQuantification
    , GADTs
    #-}
module Haste.HPlay.View(
Widget,
-- * re-exported
module Control.Applicative,

-- * widget combinators and modifiers

static, dynamic, wcallback, (<+>), (**>), (<**), validate
,firstOf, manyOf, allOf
,(<<<),(<<),(<++),(++>),(<!)

-- * basic widgets
,wprint
,getString,inputString, getInteger,inputInteger,
getInt, inputInt,inputFloat, inputDouble,getPassword,inputPassword,
setRadio,setRadioActive,getRadio
,setCheckBox, getCheckBoxes
,getTextBox, getMultilineText,textArea,getBool
,getSelect,setOption,setSelectedOption, wlabel,
resetButton,inputReset, submitButton,
inputSubmit, wbutton, wlink, noWidget, stop,wraw, isEmpty

-- * out of flow updates
,at, UpdateMethod(..)

-- * Session data storage
,getSessionData,getSData,setSessionData,setSData
,delSessionData,delSData

-- * reactive and events
,resetEventData,getEventData,EventData(..),EvData(..)
,raiseEvent, fire, wake, react, pass
,continueIf, wtimeout, Event(..)

-- * running it
,runWidget,runWidgetId, runBody, addHeader

-- * Perch is reexported
,module Haste.Perch

-- * low level and internals
,getNextId,genNewId
,getParam
,FormInput(..)
,View(..),FormElm(..)

)  where
import Control.Applicative
import Data.Monoid
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Typeable
import Unsafe.Coerce
import Data.Maybe
import Haste
import Haste.Prim
import Haste.Foreign(ffi)
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Concurrent.MVar
import qualified Data.Map as M
import Control.Monad.Trans.Maybe
import Prelude hiding(id)
import Haste.Perch

--import Debug.Trace
--(!>)= flip trace

data NeedForm= HasForm | HasElems  | NoElems deriving Show
type SData= ()


data EventF= forall b c.EventF (Widget b) [(b -> Widget c,ElemID)]

data MFlowState= MFlowState { mfPrefix :: String,mfSequence :: Int
                            , needForm :: NeedForm, process :: EventF
                            , fixed :: Bool
                            , mfData :: M.Map TypeRep SData}

type Widget a=  View Perch IO a

type WState view m = StateT MFlowState m
data FormElm view a = FormElm view (Maybe a)
newtype View v m a = View { runView :: WState v m (FormElm v a)}

mFlowState0= MFlowState "" 0 NoElems  (EventF empty
                        [(const $ empty,"noid") ] ) True M.empty


instance Functor (FormElm view ) where
  fmap f (FormElm form x)= FormElm form (fmap f x)

instance (Monoid view) => Monoid (FormElm view a) where
  mempty= FormElm mempty Nothing
  mappend (FormElm f1 x1) (FormElm f2 x2)= FormElm (f1 <> f2) (x1 <|> x2)

instance  (Monad m,Functor m) => Functor (View view m) where
  fmap f x= View $   fmap (fmap f) $ runView x


instance (Monoid view,Functor m, Monad m) => Applicative (View view m) where
  pure a  = View  .  return . FormElm mempty $ Just a
  View f <*> View g= View $
                   f >>= \(FormElm form1 k) ->
                   g >>= \(FormElm form2 x) ->
                   return $ FormElm (form1 `mappend` form2) (k <*> x)

instance (Monoid view, Functor m, Monad m) => Alternative (View view m) where
  empty= View $ return $ FormElm mempty Nothing
  View f <|> View g= View $ do
                   FormElm form1 x <- f
                   FormElm form2 y <- g
                   return $ FormElm (form1 <> form2) (x <|> y)


strip st x= View $ do
    st' <- get
    put st'{mfSequence= mfSequence st}
    FormElm f mx <- runView x
    put st'
    return $ FormElm mempty mx

setEventCont :: Widget a -> (a -> Widget b)  -> String -> StateT MFlowState IO EventF
setEventCont x f  id= do
   st <- get
   let conf = process st
   case conf of
     EventF x' fs  -> do
       let f' x = View $ do

           --     modify $ \s -> s{process= EventF (strip s $ f x) (unsafeCoerce fs) } --(unsafeCoerce $ tail fs) }
                runView $ f x


           idx=  strip st x
       put st{process= EventF idx ((f',id): unsafeCoerce fs)  }
   return conf


resetEventCont cont= modify $ \s -> s {process= cont}

instance Monad (View Perch IO) where
    x >>= f = View $ do
       fixed <- gets fixed
       modify (\st -> st{fixed= True})
       id <- genNewId
       contold <- setEventCont x  f  id
       FormElm form1 mk <- runView x
       resetEventCont contold
       let span= nelem "span" `attrs` [("id", id)]
       case mk of
         Just k  -> do
     --       contold <- setEventCont (f k !> "secondev") (return)    id  !> "build second"
            FormElm form2 mk <- runView $ f k
     --       resetEventCont contold
            case fixed of
              False ->  return $ FormElm (form1 <> (span `child`  form2)) mk
              True  ->  return $ FormElm (form1 <> form2) mk
         Nothing ->
            case fixed of
              False -> return $ FormElm  (form1 <> span)  Nothing
              True  -> return $ FormElm  form1  Nothing


    return = View .  return . FormElm  mempty . Just
    fail msg= View . return $ FormElm (inred $ fromStr msg) Nothing

-- | To produce updates, each line of html produced by a "do" sequence in the Widget monad is included
-- within a 'span' tag. When the line is reexecuted after a event, the span is updated with the new
-- rendering.
--
-- static tell to the rendering that this widget does not change, so the extra 'span' tag for each
-- line in the sequence and the rewriting is not necessary. Thus the size of the HTML and the
-- performance is improved.

static w= View $ do
   st <- get
   let was = fixed st
   put st{fixed=True}
   r <- runView $ w
   modify $ \st -> st{fixed= was}
   return r

-- override static locally to permit dynamic effects inside a static widget. It is useful
-- when a monadic Widget computation which perform no rendering changes has a to do some update:
--
-- > launchMissiles= static $ do
-- >    t <- armLauncher
-- >    c <- fixTarget t
-- >    f <- fire c
-- >    dynamic $ displayUpdate t c f
-- >    return ()

dynamic w= View $ do
   st <- get
   let was = fixed st
   put st{fixed= False}
   r <- runView $ w
   modify $ \st -> st{fixed= was}
   return r

instance (FormInput v,Monad (View v m), Monad m, Functor m, Monoid a) => Monoid (View v m a) where
  mappend x y = mappend <$> x <*> y  -- beware that both operands must validate to generate a sum
  mempty= return mempty


-- | It is a callback in the view monad. The rendering of the second parameter substitutes the rendering
-- of the first paramenter when the latter validates without afecting the rendering of other widgets.
-- This allow the simultaneous execution of different dynamic behaviours in different page locations
-- at the same page.
wcallback
  ::  Widget a -> (a ->Widget b) -> Widget b

wcallback x f= View $ do
   nid <-  genNewId
   FormElm form mx <- runView $ do
             r <-  at nid Insert x
             at nid Insert $ f r

   return $ FormElm ((Haste.Perch.span ! atr "id" nid $ noHtml) <> form) mx



identified id w= View $ do
     let span= nelem "span" `attr` ("id", id)
     FormElm f mx <- runView w
     return $ FormElm (span `child` f) mx





instance  (FormInput view,Monad m,Monad (View view m)) => MonadState (View view m) where
  type StateType (View view m)= MFlowState
  get = View $  get >>=  return . FormElm mempty . Just
  put st = View $  put st >>=  return . FormElm mempty . Just


instance (FormInput view,Monad (View view m),MonadIO m) => MonadIO (View view m) where
    liftIO io=   let x= liftIO io in x `seq` lift x


----- some combinators ----

-- | Join two widgets in the same page
-- the resulting widget, when `ask`ed with it, return a 2 tuple of their validation results
-- if both return Noting, the widget return @Nothing@ (invalid).
--
-- it has a low infix priority: @infixr 2@
--
--  > r <- ask  widget1 <+>  widget2
--  > case r of (Just x, Nothing) -> ..
(<+>) , mix ::  (Monad m, FormInput view)
      => View view m a
      -> View view m b
      -> View view m (Maybe a, Maybe b)
mix digest1 digest2= View $ do
  FormElm f1 mx' <- runView  digest1
  s1 <- get
  FormElm f2 my' <- runView  digest2
  s2 <- get
  return $ FormElm (f1 <> f2)
         $ case (mx',my') of
              (Nothing, Nothing) -> Nothing
              other              -> Just other

infixr 2 <+>

(<+>)  = mix



-- | The first elem result (even if it is not validated) is discarded, and the secod is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the first elem fails.
--
-- The first element is displayed however, as happens in the case of '*>' .
--
-- Here @w\'s@ are widgets and @r\'s@ are returned values
--
--   @(w1 <* w2)@  will return @Just r1@ only if w1 and w2 are validated
--
--   @(w1 <** w2)@ will return @Just r1@ even if w2 is not validated
--
--  it has a low infix priority: @infixr 1@

(**>) :: (Functor m, Monad m, FormInput view)
      => View view m a -> View view m b -> View view m b

(**>) f g = View $ do
   FormElm form1 k <- runView $ valid f
   FormElm form2 x <- runView g
   return $ FormElm (form1 <> form2) (k *> x)



valid form= View $ do
   FormElm form mx <- runView form
   return $ FormElm form $ Just undefined

infixr 1  **>  ,  <**

-- | The second elem result (even if it is not validated) is discarded, and the first is returned
-- . This contrast with the applicative operator '*>' which fails the whole validation if
-- the validation of the second elem fails.
-- The second element is displayed however, as in the case of '<*'.
-- see the `<**` examples
--
--  it has a low infix priority: @infixr 1@
(<**) :: (Functor m, Monad m, FormInput view) =>
     View view m a -> View view m b -> View view m a
-- (<**) form1 form2 =  form1 <* valid form2
(<**) f g = View $ do
   FormElm form1 k <- runView f
   s1 <- get
   FormElm form2 x <- runView $ valid g
   s2 <- get


   return $ FormElm (form1 <> form2) (k <* x)





instance Monoid view => MonadTrans (View view) where
  lift f = View $  (lift  f) >>= \x ->  return $ FormElm mempty $ Just x


type Name= String
type Type= String
type Value= String
type Checked= Bool
type OnClick= Maybe String


-- | Minimal interface for defining the basic form and link elements. The core of MFlow is agnostic
-- about the rendering package used. Every formatting (either HTML or not) used with MFlow must have an
-- instance of this class.
-- See "MFlow.Forms.Blaze.Html for the instance for blaze-html" "MFlow.Forms.XHtml" for the instance
-- for @Text.XHtml@ and MFlow.Forms.HSP for the instance for Haskell Server Pages.
class (Monoid view,Typeable view)   => FormInput view where
    fromStr :: String -> view
    fromStrNoEncode :: String -> view
    ftag :: String -> view  -> view
    inred   :: view -> view
    flink ::  String -> view -> view
    flink1:: String -> view
    flink1 verb = flink verb (fromStr  verb)
    finput :: Name -> Type -> Value -> Checked -> OnClick -> view
    ftextarea :: String -> String -> view
    fselect :: String -> view -> view
    foption :: String -> view -> Bool -> view
    foption1 :: String -> Bool -> view
    foption1   val msel= foption val (fromStr val) msel
    formAction  :: String -> String -> view -> view
    attrs :: view -> Attribs -> view

type Attribs= [(String, String)]


data ParamResult v a= NoParam | NotValidated String v | Validated a deriving (Read, Show)

valToMaybe (Validated x)= Just x
valToMaybe _= Nothing

isValidated (Validated x)= True
isValidated _= False

fromValidated (Validated x)= x
fromValidated NoParam= error $ "fromValidated : NoParam"
fromValidated (NotValidated s err)= error $ "fromValidated: NotValidated "++ s

getParam1 :: (MonadIO m, MonadState  m, Typeable a, Read a, FormInput v)
          => String ->  m (ParamResult v a)
getParam1 par = do
   me <- elemById par
   case me of
     Nothing -> return  NoParam
     Just e ->  do
       mv <- getValue e
       case mv of
         Nothing -> return NoParam
         Just v -> do
           readParam v


type Params= Attribs



readParam :: (Monad m, MonadState  m, Typeable a, Read a, FormInput v)
           => String -> m (ParamResult v a)
readParam x1 = r
 where
 r= maybeRead x1

 getType ::  m (ParamResult v a) -> a
 getType= undefined
 x= getType r
 maybeRead str= do
   let typeofx = typeOf x
   if typeofx == typeOf  ( undefined :: String)   then
           return . Validated $ unsafeCoerce str
    else case readsPrec 0 $ str of
              [(x,"")] ->  return $ Validated x
              _ -> do
                   let err= inred . fromStr $ "can't read \"" ++ str ++ "\" as type " ++  show (typeOf x)
                   return $ NotValidated str err

-- | Validates a form or widget result against a validating procedure
--
-- @getOdd= getInt Nothing `validate` (\x -> return $ if mod x 2==0 then  Nothing else Just "only odd numbers, please")@
validate
  :: (FormInput view,  Monad m,Monad (View view m)) =>
     View view m a
     -> (a -> WState view m (Maybe view))
     -> View view m a
validate  formt val= do
   mx <- View $ do
         FormElm form mx <- runView  formt
         return $ FormElm form (Just mx)
   View $ do
     case mx of
        Just x -> do
          me <- val x
          case me of
             Just str -> return $ FormElm (inred  str) Nothing
             Nothing  -> return $ FormElm mempty mx
        _ -> return $ FormElm mempty Nothing

-- | Generate a new string. Useful for creating tag identifiers and other attributes.
--
-- if the page is refreshed, the identifiers generated are the same.
genNewId :: (StateType m ~ MFlowState, MonadState  m) =>  m String
genNewId=  do
      st <- get
      let n= mfSequence st
          prefseq=  mfPrefix st
      put $ st{mfSequence= n+1}

      return $ 'p':show n++prefseq


-- | get the next ideitifier that will be created by genNewId
getNextId :: (StateType m ~ MFlowState,MonadState  m) =>  m String
getNextId=  do
      st <- get
      let n= mfSequence st
          prefseq=  mfPrefix st
      return $ 'p':show n++prefseq


-- | Display a text box and return a non empty String
getString  :: (StateType (View view m) ~ MFlowState,FormInput view,Monad(View view m),MonadIO m) =>
     Maybe String -> View view m String
getString ms = getTextBox ms
--     `validate`
--     \s -> if Prelude.null s then return (Just $ fromStr "")
--                    else return Nothing

inputString  :: (StateType (View view m) ~ MFlowState,FormInput view,Monad(View view m),MonadIO m) =>
     Maybe String -> View view m String
inputString= getString

-- | Display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: (StateType (View view m) ~ MFlowState,FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getTextBox

inputInteger :: (StateType (View view m) ~ MFlowState,FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
inputInteger= getInteger

-- | Display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getTextBox

inputInt :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
inputInt =  getInt

inputFloat :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Float -> View view m Float
inputFloat =  getTextBox

inputDouble :: (StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) =>
     Maybe Double -> View view m Double
inputDouble =  getTextBox

-- | Display a password box
getPassword :: (FormInput view,StateType (View view m) ~ MFlowState,
     MonadIO m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

inputPassword :: (StateType (View view m) ~ MFlowState,FormInput view,
     MonadIO m) =>
     View view m String
inputPassword= getPassword

newtype Radio a= Radio a



-- | Implement a radio button
-- the parameter is the name of the radio group
setRadio :: (FormInput view,  MonadIO m,
             Typeable a, Eq a, Show a) =>
            a -> String -> View view m  (Radio a)
setRadio v n= View $ do
  id <- genNewId
  st <- get
  put st{needForm= HasElems}
  me <- liftIO $ elemById id
  checked <- case me of
       Nothing -> return ""
       Just e  -> liftIO $   getProp e "checked"
  let strs= if  checked=="true" then Just v else Nothing
--  let mn= if null strs then False else True
      ret= fmap  Radio  strs
      str = if typeOf v == typeOf(undefined :: String)
                   then unsafeCoerce v else show v
  return $ FormElm
      ( finput id "radio" str ( isJust strs ) Nothing `attrs` [("name",n)])
      ret


setRadioActive rs x= setRadio rs x `raiseEvent` OnClick

-- | encloses a set of Radio boxes. Return the option selected
getRadio
  :: (Monad (View view m), Monad m, Functor m, FormInput view) =>
     [String -> View view m (Radio a)] -> View view m a
getRadio ws = View $ do
   id <- genNewId
   fs <- mapM (\w -> runView (w id)) ws
   let FormElm render mx = mconcat fs
   return $ FormElm render $ fmap (\(Radio r) -> r) mx


data CheckBoxes a= CheckBoxes [a]

instance Monoid (CheckBoxes a) where
  mappend (CheckBoxes xs) (CheckBoxes ys)= CheckBoxes $ xs ++ ys
  mempty= CheckBoxes []


-- | Display a text box and return the value entered if it is readable( Otherwise, fail the validation)
setCheckBox :: (FormInput view,  MonadIO m, Typeable a , Show a) =>
                Bool -> a -> View view m  (CheckBoxes a)
setCheckBox checked' v= View $ do
  n  <- genNewId
  st <- get
  put st{needForm= HasElems}
  me <- liftIO $ elemById n
  checked <- case me of
       Nothing ->  return $ if checked' then "true" else ""
       Just e  -> liftIO $  getProp e "checked"
  let strs= if  checked=="true"  then [v] else []
--  let mn= if null strs then False else True
      ret= Just $ CheckBoxes  strs
      showv= case typeOf v== typeOf (undefined ::String) of
               True -> unsafeCoerce v
               False -> show v
  return $ FormElm
      ( finput n "checkbox" showv ( checked' ) Nothing)
      ret


getCheckBoxes :: (Monad m, FormInput view) =>  View view m  (CheckBoxes a) ->  View view m  [a]
getCheckBoxes w= View $ do
   FormElm render mcb <- runView w
   return $ FormElm render $ case mcb of
     Just(CheckBoxes rs) -> Just rs
     _                   -> Nothing



whidden :: (MonadIO m, FormInput v,Read a, Show a, Typeable a) => a -> View v m a
whidden x= res where
 res= View $ do
      n <- genNewId
      let showx= case cast x of
                  Just x' -> x'
                  Nothing -> show x
      r <- getParam1 n `asTypeOf` typef res
      return . FormElm (finput n "hidden" showx False Nothing) $ valToMaybe r
      where
      typef :: View v m a -> StateT MFlowState m (ParamResult v a)
      typef = undefined




getTextBox
  :: (FormInput view, StateType (View view m) ~ MFlowState,
      MonadIO  m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe a ->  View view m a
getTextBox ms  = getParam Nothing "text" ms


getParam
  :: (FormInput view,StateType (View view m) ~ MFlowState,
      MonadIO m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe String -> String -> Maybe a -> View view m  a
getParam look type1 mvalue= View $ getParamS look type1 mvalue

getParamS look type1 mvalue= do
    tolook <- case look of
       Nothing  -> genNewId
       Just n -> return n
    let nvalue x = case x of
           Nothing  -> ""
           Just v   ->
               case cast v of
                 Just v' -> v'
                 Nothing -> show v
    st <- get

    put st{needForm= HasElems}
    r <- getParam1 tolook
    case r of
       Validated x        -> return $ FormElm (finput tolook type1 (nvalue $ Just x) False Nothing) $ Just x
       NotValidated s err -> return $ FormElm (finput tolook type1 s False Nothing <> err) $ Nothing
       NoParam            -> return $ FormElm (finput tolook type1 (nvalue mvalue) False Nothing) $ Nothing




-- | Display a multiline text box and return its content
getMultilineText :: (FormInput view
                 ,  MonadIO m)
                   => String
                 ->  View view m String
getMultilineText nvalue = res where
 res= View $ do
    tolook <- genNewId
    r <- getParam1 tolook  `asTypeOf` typef res
    case r of
       Validated x        -> return $ FormElm (ftextarea tolook  x) $ Just x
       NotValidated s err -> return $ FormElm (ftextarea tolook   s)  Nothing
       NoParam            -> return $ FormElm (ftextarea tolook  nvalue)  Nothing
    where
    typef :: View v m String -> StateT MFlowState m (ParamResult v a)
    typef = undefined

-- | A synonim of getMultilineText
textArea :: (FormInput view
                 ,  MonadIO m)
                   => String
                 ->  View view m String
textArea= getMultilineText


--getBool :: (FormInput view,
--      Monad m, Monad (View view m), Functor m) =>
--      Bool -> String -> String -> View view m Bool
getBool mv truestr falsestr= do
   r <- getSelect $   setOption truestr (fromStr truestr)  <! (if mv then [("selected","true")] else [])
                  <|> setOption falsestr(fromStr falsestr) <! if not mv then [("selected","true")] else []
   if  r == truestr  then return True else return False



-- | Display a dropdown box with the options in the first parameter is optionally selected
-- . It returns the selected option.
getSelect :: (FormInput view,
      MonadIO m,Typeable a, Read a) =>
      View view m (MFOption a) ->  View view m  a
getSelect opts = res where
  res= View $ do
    tolook <- genNewId
    st <- get
    put st{needForm= HasElems}
    r <- getParam1 tolook `asTypeOf` typef res
--    setSessionData $ fmap MFOption $ valToMaybe r
    FormElm form mr <- (runView opts)
--
    return $ FormElm (fselect tolook  form)  $ valToMaybe r

    where
    typef :: View v m a -> StateT MFlowState m (ParamResult v a)
    typef = undefined

newtype MFOption a= MFOption a deriving Typeable

instance (FormInput view,Monad m, Functor m) => Monoid (View view m (MFOption a)) where
  mappend =  (<|>)
  mempty = Control.Applicative.empty

-- | Set the option for getSelect. Options are concatenated with `<|>`
setOption
  :: (Monad m, Monad (View view m), Show a, Eq a, Typeable a, FormInput view) =>
     a -> view -> View view m (MFOption a)
setOption n v = View $ do
--  mo <- getSessionData
  runView $ setOption1 n v False


-- | Set the selected option for getSelect. Options are concatenated with `<|>`
setSelectedOption
  :: (Monad m, Monad(View view m), Show a, Eq a, Typeable a, FormInput view) =>
     a -> view -> View view m (MFOption a)
setSelectedOption n v= View $ do
--  mo <- getSessionData
  runView $ setOption1 n v True
--   Just Nothing -> setOption1 n v True
--   Just (Just o) -> setOption1 n v $   n == o


setOption1 :: (FormInput view,
      Monad m, Typeable a, Eq a, Show a) =>
      a -> view -> Bool ->  View view m  (MFOption a)
setOption1 nam  val check= View $ do
    let n = if typeOf nam == typeOf(undefined :: String)
                   then unsafeCoerce nam
                   else show nam

    return . FormElm (foption n val check)  . Just $ MFOption nam


wlabel
  :: (Monad m, FormInput view) => view -> View view m a -> View view m a
wlabel str w =View $ do
   id <- getNextId
   FormElm render mx <- runView w
   return $ FormElm (ftag "label" str `attrs` [("for",id)] <> render) mx


-- passive reset button.
resetButton :: (FormInput view, Monad m) => String -> View view m ()
resetButton label= View $ return $ FormElm (finput  "reset" "reset" label False Nothing)
                        $ Just ()

inputReset :: (FormInput view, Monad m) => String -> View view m ()
inputReset= resetButton

-- passive submit button. Submit a form, but it is not trigger any event.
-- Unless you attach it with `trigger`
submitButton :: (Monad (View view m),StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) => String -> View view m String
submitButton label=  getParam Nothing "submit" $ Just label


inputSubmit :: (Monad (View view m),StateType (View view m) ~ MFlowState,FormInput view, MonadIO m) => String -> View view m String
inputSubmit= submitButton

-- | active button. When clicked, return the label value
wbutton :: a -> String -> Widget a
wbutton x label= static $ do
        input  ! atr "type" "submit" ! atr "value" label `pass` OnClick
        return x

-- | Present a link. Return the first parameter when clicked
wlink :: (Show a, Typeable a) => a -> Perch -> Widget a
wlink x v= static $ do
    (a ! href ("#/"++show1 x)   $ v) `pass` OnClick
    return x

   where
   show1 x | typeOf x== typeOf (undefined :: String) = unsafeCoerce x
           | otherwise= show x




-- | Concat a list of widgets of the same type, return a the first validated result
firstOf :: (FormInput view, Monad m, Functor m)=> [View view m a]  -> View view m a
firstOf xs= Prelude.foldl (<|>) noWidget xs

-- | from a list of widgets, it return the validated ones.
manyOf :: (FormInput view, MonadIO m, Functor m)=> [View view m a]  -> View view m [a]
manyOf xs=  (View $ do
      forms <- mapM runView  xs
      let vs  = mconcat $ Prelude.map (\(FormElm v _) ->   v) forms
          res1= catMaybes $ Prelude.map (\(FormElm _ r) -> r) forms
      return . FormElm vs $ Just res1)

-- | like manyOf, but does not validate if one or more of the widgets does not validate
allOf xs= manyOf xs `validate` \rs ->
      if length rs== length xs
         then return Nothing
         else return $ Just mempty

-- | show something enclosed in the <pre> tag, so ASCII formatting chars are honored
wprint :: ToElem a => a -> Widget ()
wprint = wraw . pre

-- | Enclose Widgets within some formating.
-- @view@ is intended to be instantiated to a particular format
--
-- NOTE: It has a infix priority : @infixr 5@ less than the one of @++>@ and @<++@ of the operators, so use parentheses when appropriate,
-- unless the we want to enclose all the widgets in the right side.
-- Most of the type errors in the DSL are due to the low priority of this operator.
--

(<<<) :: (Monad m,  Monoid view)
          => (view ->view)
         -> View view m a
         -> View view m a
(<<<) v form= View $ do
  FormElm f mx <- runView form
  return $ FormElm (v  f) mx


infixr 5 <<<

-- | A parameter application with lower priority than ($) and direct function application
(<<) :: (t1 -> t) -> t1 -> t
(<<) tag content= tag  content

infixr 7 <<


-- | Append formatting code to a widget
--
-- @ getString "hi" <++ H1 << "hi there"@
--
-- It has a infix prority: @infixr 6@ higuer that '<<<' and most other operators
(<++) :: (Monad m, Monoid v)
      => View v m a
      -> v
      -> View v m a
(<++) form v= View $ do
  FormElm f mx <-  runView  form
  return $  FormElm ( f <> v) mx

infixr 6  ++>
infixr 6 <++
-- | Prepend formatting code to a widget
--
-- @bold << "enter name" ++> getString Nothing @
--
-- It has a infix prority: @infixr 6@ higuer that '<<<' and most other operators
(++>) :: (Monad m,  Monoid view)
       => view -> View view m a -> View view m a
html ++> w =  --  (html <>) <<< digest
 View $ do
  FormElm f mx <- runView w
  return $ FormElm (html  <>  f) mx



-- | Add attributes to the topmost tag of a widget
--
-- it has a fixity @infix 8@
infixl 8 <!
widget <! attribs= View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  (fs `attrs` attribs) mx -- (head fs `attrs` attribs:tail fs) mx
--      case fs of
--        [hfs] -> return $ FormElm  [hfs `attrs` attribs] mx
--        _ -> error $ "operator <! : malformed widget: "++ concatMap (unpack. toByteString) fs



instance  Attributable (Widget a) where
 (!) widget atrib = View $ do
      FormElm fs  mx <- runView widget
      return $ FormElm  (fs `attr` atrib) mx



-- | Empty widget that does not validate. May be used as \"empty boxes\" inside larger widgets.
--
-- It returns a non valid value.
noWidget ::  (FormInput view,
     Monad m, Functor m) =>
     View view m a
noWidget= Control.Applicative.empty

-- | a sinonym of noWidget that can be used in a monadic expression in the View monad. it stop the
-- computation in the Widget monad.
stop :: (FormInput view,
     Monad m, Functor m) =>
     View view m a
stop= Control.Applicative.empty

-- | Render a Show-able  value and return it
--wrender
--  :: (Monad m, Functor m, Show a,Monad (View view m), FormInput view) =>
--     a -> View view m a
--wrender x = (fromStr $ show x) ++> return x

-- | Render raw view formatting. It is useful for displaying information.
wraw ::  Perch -> Widget ()
wraw x= View . return . FormElm x $ Just ()

-- | True if the widget has no valid input
isEmpty :: Widget a -> Widget Bool
isEmpty w= View $ do
  FormElm r mv <- runView w
  return $ FormElm r $ Just $ isNothing mv


-------------------------
instance   FormInput Perch  where
    fromStr = toElem
    fromStrNoEncode  = toElem
    ftag n v =  nelem n `child` v

    attrs tag  [] = tag
    attrs tag (nv:attribs) = attrs (attr tag nv) attribs

    inred msg=  ftag "b" msg `attrs` [("style","color:red")]

    finput n t v f c=
       let
        tag= ftag "input" mempty `attrs` [("type",  t), ("id",  n), ("value",  v)]
        tag1= if f then tag `attrs` [("checked", "")] else tag
       in case c of Just s -> tag1 `attrs` [("onclick", s)] ; _ -> tag1

    ftextarea nam text=
        ftag "textarea" mempty `attrs` [("id",  nam)] `child` text


    fselect nam list = ftag "select" mempty `attrs` [("id", nam)] `child` list
    foption  name v msel=
      let tag=  ftag "option" mempty `attrs` [("value", name)]  `child`  v
      in if msel then tag `attrs` [("selected", "")] else tag


    formAction action method1 form = ftag "form" mempty `attrs` [("acceptCharset", "UTF-8")
                                                         ,( "action", action)
                                                         ,("method",  method1)]
                                                         `child` form


    flink  v str = ftag "a" mempty `attrs` [("href",  v)] `child` str

-- | Get the session data of the desired type if there is any.
getSessionData ::  (StateType m ~ MFlowState,MonadState m,Typeable a) =>  m (Maybe a)
getSessionData =  resp where
 resp= gets mfData >>= \list  ->
    case M.lookup ( typeOf $ typeResp resp ) list of
      Just x  -> return . Just $ unsafeCoerce x
      Nothing -> return $ Nothing
 typeResp :: m (Maybe x) -> x
 typeResp= undefined

-- | getSessionData specialized for the View monad. if Nothing, the monadic computation
-- does not continue. getSData is a widget that does not validate when there is no data
--  of that type in the session.
getSData :: Typeable a =>Widget  a
getSData= View $ do
    r <- getSessionData
    return $ FormElm mempty r

-- | setSessionData ::  (StateType m ~ MFlowState, Typeable a) => a -> m ()
setSessionData  x=
  modify $ \st -> st{mfData= M.insert  (typeOf x ) (unsafeCoerce x) (mfData st)}

-- | a shorter name for setSessionData
setSData ::  (StateType m ~ MFlowState, MonadState  m,Typeable a) => a -> m ()
setSData= setSessionData

delSessionData x=
  modify $ \st -> st{mfData= M.delete (typeOf x ) (mfData st)}

delSData :: (StateType m ~ MFlowState, MonadState  m,Typeable a) => a -> m ()
delSData= delSessionData

---------------------------
data EvData =  NoData | Click Int (Int, Int) | Mouse (Int, Int) | Key Int deriving (Show,Eq)
data EventData= EventData{ evName :: String, evData :: EvData} deriving Show

eventData= unsafePerformIO . newMVar $ EventData "OnLoad" NoData

resetEventData :: MonadIO m => m ()
resetEventData= liftIO . modifyMVar_ eventData . const . return $ EventData "Onload" NoData

getEventData :: MonadIO m => m EventData
getEventData= liftIO $ readMVar eventData

-- | triggers the event when it happens in the widget.
--
-- What happens then?
--
-- 1)The event reexecutes all the monadic sentence where the widget is, (with no re-rendering)
--
-- 2) with the result of this reevaluaution, executes the rest of the monadic computation
--
-- 3) update the DOM tree with the rendering of the reevaluation in 2).
--
-- As usual, If one step of the monadic computation return empty, the reevaluation finish
-- So the effect of an event can be restricted as much as you may need.
--
-- Neither the computation nor the tree in the upstream flow is touched.
-- (unless you use out of stream directives, like `at`)
--
-- monadic computations inside monadic computations are executed following recursively
-- the steps mentioned above. So an event in a component deep down could or could not
-- trigger the reexecution of the rest of the whole.
raiseEvent ::  Widget a -> Event IO b ->Widget a
raiseEvent w event = View $ do
 r <- gets process
 case r of
  EventF x fs -> do
   FormElm render mx <- runView  w
   let proc = runIt x (unsafeCoerce fs)  >> return () -- runWidgetId (x >>= f)id >> return ()
   let nevent= evtName event :: String
   let putevdata dat= modifyMVar_ eventData $ const $ return dat
   let render' =  case event of
        OnLoad    -> addEvent (render :: Perch) event $ putevdata (EventData nevent NoData) >> proc
        OnUnload  -> addEvent (render :: Perch) event $ putevdata (EventData nevent NoData) >> proc
        OnChange  -> addEvent (render :: Perch) event $ putevdata (EventData nevent NoData) >> proc
        OnFocus   -> addEvent (render :: Perch) event $ putevdata (EventData nevent NoData) >> proc
        OnBlur    -> addEvent (render :: Perch) event $ putevdata (EventData nevent NoData) >> proc

        OnMouseMove -> addEvent (render :: Perch) event $ \(x,y) -> do
                         putevdata $ EventData nevent $ Mouse(x,y)
                         proc

        OnMouseOver -> addEvent (render :: Perch) event $ \(x,y) -> do
                         putevdata $  EventData nevent $ Mouse(x,y)
                         proc

        OnMouseOut -> addEvent (render :: Perch) event proc
        OnClick -> addEvent (render :: Perch) event $ \i (x,y) -> do
                         putevdata $  EventData nevent $ Click i (x,y)
                         proc

        OnDblClick -> addEvent (render :: Perch) event $ \i (x,y) -> do
                         putevdata $ EventData nevent $ Click i (x,y)
                         proc

        OnMouseDown -> addEvent (render :: Perch) event $ \i (x,y) -> do
                         putevdata $ EventData nevent $ Click i (x,y)
                         proc

        OnMouseUp -> addEvent (render :: Perch) event $ \i (x,y) -> do
                         putevdata $ EventData nevent $ Click i (x,y)
                         proc

        OnKeyPress -> addEvent (render :: Perch) event $ \i -> do
                         putevdata $ EventData nevent $ Key i
                         proc

        OnKeyUp  -> addEvent (render :: Perch) event $ \i -> do
                         putevdata $ EventData nevent $ Key i
                         proc

        OnKeyDown -> addEvent (render :: Perch) event $ \i -> do
                         putevdata $ EventData nevent $ Key i
                         proc

   modify $ \s -> s{fixed= False}
   return $ FormElm render' mx
   where
   runIt x fs= runBody $ x >>= compose fs

      where

      compose []= const empty
      compose ((f,id): fs)= \x -> at id Insert (f x) >>= compose fs

--   addEvent :: Perch -> Event IO a -> a -> Perch
--   addEvent be event action= Perch $ \e -> do
--     e' <- build be e
--     onEvent e' event  action
--     return e'

--   addto f f'=  do
--     mr <- f  !> "addto1"
--     case mr of
--       Nothing -> return Nothing  !> "addto1 nothing"
--       Just x' ->  f' x'       !> "addto1just"

-- | A shorter synonym for `raiseEvent`
fire ::  Widget a -> Event IO b ->Widget a
fire = raiseEvent

-- | A shorter and smoother synonym for `raiseEvent`
wake ::  Widget a -> Event IO b -> Widget a
wake = raiseEvent

-- | A professional synonym for `raiseEvent`
react :: Widget a -> Event IO b -> Widget a
react = raiseEvent

-- | pass trough only if the event is fired in this DOM element.
-- Otherwise, if the code is executing from a previous event, the computation will stop
pass :: Perch -> Event IO b -> Widget EventData
pass v event= static $ do
        resetEventData
        wraw v `wake` event
        e@(EventData typ _) <- getEventData
        continueIf (evtName event== typ) e

-- | return empty and the monadic computation stop if the condition is false.
-- If true, return the second parameter.
continueIf :: Bool -> a -> Widget a
continueIf True x  = return x
continueIf False _ = empty

-- | executes a widget each t milliseconds until it validates and return ()
wtimeout :: Int -> Widget () -> Widget ()
wtimeout t w= View $ do
    id <- genNewId
    let f= setTimeout t $ do
        me <- elemById  id
        case me of
         Nothing -> return ()
         Just e ->do
            r <- clearChildren e >> runWidget w e
            case r of
              Nothing -> f
              Just ()  -> return ()

    liftIO  f
    runView $ identified id w


globalState= unsafePerformIO $ newMVar mFlowState0

-- | run the widget as the content of a DOM element, the id is passed as parameter. All the
-- content of the element is erased previously and it is substituted by the new rendering
runWidgetId :: Widget b -> ElemID  -> IO (Maybe b)
runWidgetId ac id =  do
   me <- elemById id
   case me of
     Just e ->  do
      clearChildren e
      runWidget ac e
     Nothing -> do
          st <- takeMVar globalState
          (FormElm render mx, s) <- runStateT (runView ac) st
          liftIO $ putMVar globalState s
          return mx


-- | run the widget as the content of a DOM element
-- the new rendering is added to the element
runWidget :: Widget b -> Elem  -> IO (Maybe b)
runWidget action e = do
     st <- takeMVar globalState
     (FormElm render mx, s) <- runStateT (runView action) st
     liftIO $ putMVar globalState s
     build render e
     return mx



-- | add a header in the <header> tag
addHeader :: Perch -> IO ()
addHeader format= do
    head <- getHead
    build format head
    return ()
    where
    getHead :: IO Elem
    getHead= ffi $ toJSStr "(function(){return document.head;})"

-- | run the widget as the body of the HTML
runBody :: Widget a -> IO (Maybe a)
runBody w= do
  body <- getBody
  (flip runWidget) body w
  where
  getBody :: IO Elem
  getBody= ffi $ toJSStr "(function(){return document.body;})"

data UpdateMethod= Append | Prepend | Insert deriving Show

-- | Run the widget as the content of the element with the given id. The content can
-- be appended, prepended to the previous content or it can be the only content depending on the
-- update method.
at :: ElemID -> UpdateMethod -> Widget a -> Widget  a
at id method w= View $ do
 FormElm render mx <- (runView w)
 return $ FormElm  (set  render)  mx
 where
 set render= liftIO $ do
         me <- elemById id
         case me of
          Nothing -> return ()
          Just e -> case method of
             Insert -> do
                     clearChildren e
                     build render e
                     return ()
             Append -> do
                     build render e
                     return ()
             Prepend -> do
                     es <- getChildren e
                     case es of
                       [] -> build render e >> return ()
                       e':es -> do
                             span <- newElem "span"
                             addChildBefore span e e'
                             build render span
                             return()


