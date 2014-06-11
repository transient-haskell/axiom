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
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE  MultiParamTypeClasses, FlexibleInstances
    , UndecidableInstances, TypeFamilies, DeriveDataTypeable
    #-}
module View  where
import Control.Applicative
import Data.Monoid
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Typeable
import Data.ByteString.Lazy.Char8  as B hiding (length, foldr, take)
import Unsafe.Coerce
import Data.Maybe
import Haste.DOM
import Haste
import Unsafe.Coerce

import Builder
-- | @View v m a@ is a widget (formlet)  with formatting `v`  running the monad `m` (usually `IO`) and which return a value of type `a`
--
-- It has 'Applicative', 'Alternative' and 'Monad' instances.
--
-- Things to know about these instances:
--
--   If the View expression does not validate, ask will present the page again.
--
-- /Alternative instance/: Both alternatives are executed. The rest is as usual
--
-- /Monad Instance/:
--
--  The rendering of each statement is added to the previous. If you want to avoid this, use 'wcallback'
--
--  The execution is stopped when the statement has a formlet-widget that does not validate and
-- return an invalid response (So it will present the page again if no other widget in the expression validates).
--
--  The monadic code is executed from the beginning each time the page is presented or refreshed
--
--  use 'pageFlow' if your page has more than one monadic computation with dynamic behaviour
--
-- use 'pageFlow' to identify each subflow branch of a conditional
--
--  For example:
--
--  > pageFlow "myid" $ do
--  >      r <- formlet1
--  >      liftIO $ ioaction1 r
--  >      s <- formlet2
--  >      liftIO $ ioaction2 s
--  >      case s of
--  >       True  -> pageFlow "idtrue" $ do ....
--  >       False -> paeFlow "idfalse" $ do ...
--  >      ...
--
--  Here if  @formlet2@ do not validate, @ioaction2@ is not executed. But if @formLet1@ validates and the
--  page is refreshed two times (because @formlet2@ has failed, see above),then @ioaction1@ is executed two times.
--  use 'cachedByKey' if you want to avoid repeated IO executions.
data NeedForm= HasForm | HasElems  | NoElems deriving Show
data MFlowState= MFlowState { mfPrefix :: String,mfSequence :: Int
                            ,  needForm :: NeedForm, process :: IO ()}
type WState view m = StateT MFlowState m
data FormElm view a = FormElm view (Maybe a)
newtype View v m a = View { runView :: WState v m (FormElm v a)}

mFlowState0= MFlowState "" 0 NoElems  (return ())
--
--
--instance  FormInput v => MonadLoc (View v IO)  where
--    withLoc loc f = View $ do
--       withLoc loc $ do
--            s <- get
--            (r,s') <- lift $ do
--                       rs@(r,s') <- runStateT (runView f) s
--                                             `CE.catch` (handler1  loc s)
--                       case mfTrace s' of
--                            []     ->  return rs
--                            trace  ->  return(r, s'{mfTrace=  loc:trace})
--            put s'
--            return r
--
--       where
--       handler1 loc s (e :: SomeException)= do
--        case CE.fromException e :: Maybe WFErrors of
--           Just e  -> CE.throw e                -- !> ("TROWN=" ++ show e)
--           Nothing ->
--             case CE.fromException e :: Maybe AsyncException of
--                Just e -> CE.throw e            -- !> ("TROWN ASYNC=" ++ show e)
--                Nothing ->
--                  return (FormElm mempty Nothing, s{mfTrace= [show e]}) -- !> loc
--






instance Functor (FormElm view ) where
  fmap f (FormElm form x)= FormElm form (fmap f x)

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


   
instance  (FormInput view, Monad m) => Monad (View view m) where
    View x >>= f = View $ do
           FormElm form1 mk <- x   
           case mk of
             Just k  -> do
                 
                FormElm form2 mk <- runView $ f k


                return $ FormElm (form1 <> form2) mk
             Nothing -> 
                return $ FormElm form1 Nothing
                        

    return = View .  return . FormElm  mempty . Just
--    fail msg= View . return $ FormElm [inRed msg] Nothing


  

instance (FormInput v,Monad m, Functor m, Monoid a) => Monoid (View v m a) where
  mappend x y = mappend <$> x <*> y  -- beware that both operands must validate to generate a sum
  mempty= return mempty


-- | It is a callback in the view monad. The callback rendering substitutes the widget rendering
-- when the latter is validated, without afecting the rendering of other widgets. This allow
-- the simultaneous execution of different behaviours in different widgets in the
-- same page. The inspiration is the callback primitive in the Seaside Web Framework
-- that allows similar functionality (See <http://www.seaside.st>)
--
-- This is the visible difference with 'waction' callbacks, which execute a
-- a flow in the FlowM monad that takes complete control of the navigation, while wactions are
-- executed whithin the same page.
wcallback
  :: Monad m =>
     View view m a -> (a -> View view m b) -> View view m b
wcallback (View x) f = View $ do
   FormElm form1 mk <- x
   case mk of
     Just k  -> do
       runView (f k)
       
     Nothing -> return $ FormElm form1 Nothing



instance  (FormInput view,Monad m) => MonadState (View view m) where
  type StateType (View view m) = MFlowState
  get = View $  get >>=  return . FormElm mempty . Just 
  put st = View $  put st >>=  return . FormElm mempty . Just 

--instance  (Monad m)=> MonadState (MFlowState view) (FlowM view m) where
--  get = FlowM $  get >>= \x ->  return $ FormElm [] $ Just x
--  put st = FlowM $  put st >>= \x ->  return $ FormElm [] $ Just x


instance (FormInput view,MonadIO m) => MonadIO (View view m) where
    liftIO io= let x= liftIO io in x `seq` lift x -- to force liftIO==unsafePerformIO on the Identity monad


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
    toByteString :: view -> B.ByteString

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
  :: (FormInput view,  Monad m) =>
     View view m a
     -> (a -> WState view m (Maybe view))
     -> View view m a
validate  formt val= View $ do
   FormElm form mx <- runView  formt
   case mx of
    Just x -> do
      me <- val x
      case me of
         Just str ->
           return $ FormElm ( form <> inred  str) Nothing
         Nothing  -> return $ FormElm form mx
    _ -> return $ FormElm form mx

-- | Generate a new string. Useful for creating tag identifiers and other attributes.
--
-- if the page is refreshed, the identifiers generated are the same.
genNewId :: (StateType m ~ MFlowState,MonadState  m) =>  m String
genNewId=  do
      st <- get
      let n= mfSequence st
          prefseq=  mfPrefix st
      put $ st{mfSequence= n+1}

      return $ 'p':show n++prefseq  


---- | get the next ideitifier that will be created by genNewId
--getNextId :: (StateType m ~ MFlowState,MonadState  m) =>  m String
--getNextId=  do
--      st <- get
--      let n= mfSequence st
--          prefseq=  mfPrefix st
--      return $ 'p':show n++prefseq


-- | Display a text box and return a non empty String
getString  :: (FormInput view,MonadIO m) =>
     Maybe String -> View view m String
getString ms = getTextBox ms
     `validate`
     \s -> if Prelude.null s then return (Just $ fromStr "")
                    else return Nothing

inputString  :: (FormInput view,MonadIO m) =>
     Maybe String -> View view m String
inputString= getString

-- | Display a text box and return an Integer (if the value entered is not an Integer, fails the validation)
getInteger :: (FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
getInteger =  getTextBox

inputInteger :: (FormInput view,  MonadIO m) =>
     Maybe Integer -> View view m  Integer
inputInteger= getInteger

-- | Display a text box and return a Int (if the value entered is not an Int, fails the validation)
getInt :: (FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
getInt =  getTextBox

inputInt :: (FormInput view, MonadIO m) =>
     Maybe Int -> View view m Int
inputInt =  getInt

-- | Display a password box
getPassword :: (FormInput view,
     MonadIO m) =>
     View view m String
getPassword = getParam Nothing "password" Nothing

inputPassword :: (FormInput view,
     MonadIO m) =>
     View view m String
inputPassword= getPassword

newtype Radio a= Radio a


--
--whidden :: (Monad m, FormInput v,Read a, Show a, Typeable a) => a -> View v m a
--whidden x= View $ do
--  n <- genNewId
--
--  let showx= case cast x of
--              Just x' -> x'
--              Nothing -> show x
--  r <- getParam1 n 
--  return . FormElm (finput n "hidden" showx False Nothing) $ valToMaybe r
--
--



getTextBox
  :: (FormInput view,
      MonadIO  m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe a ->  View view m a
getTextBox ms  = getParam Nothing "text" ms


getParam
  :: (FormInput view,
      MonadIO m,
      Typeable a,
      Show a,
      Read a) =>
     Maybe String -> String -> Maybe a -> View view m  a
getParam look type1 mvalue = View $ do
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



--getCurrentName :: MonadState (MFlowState view) m =>  m String
--getCurrentName= do
--     st <- get
--     let parm = mfSequence st
--     return $ "p"++show parm


---- | Display a multiline text box and return its content
--getMultilineText :: (FormInput view
--                 ,  Monad m)
--                   => String
--                 ->  View view m String
--getMultilineText nvalue = View $ do
--    tolook <- genNewId
--
--    r <- getParam1 tolook 
--    case r of
--       Validated x        -> return $ FormElm (ftextarea tolook  x) $ Just x
--       NotValidated s err -> return $ FormElm (ftextarea tolook  s)  Nothing
--       NoParam            -> return $ FormElm (ftextarea tolook  nvalue)  Nothing




resetButton :: (FormInput view, Monad m) => String -> View view m ()
resetButton label= View $ return $ FormElm (finput  "reset" "reset" label False Nothing)
                        $ Just ()

inputReset :: (FormInput view, Monad m) => String -> View view m ()
inputReset= resetButton

submitButton :: (FormInput view, MonadIO m) => String -> View view m String
submitButton label= getParam Nothing "submit" $ Just label

inputSubmit :: (FormInput view, MonadIO m) => String -> View view m String
inputSubmit= submitButton

-- | Concat a list of widgets of the same type, return a the first validated result
firstOf :: (FormInput view, Monad m, Functor m)=> [View view m a]  -> View view m a
firstOf xs= Prelude.foldl (<|>) noWidget xs

-- | Enclose Widgets within some formating.
-- @view@ is intended to be instantiated to a particular format
--
-- NOTE: It has a infix priority : @infixr 5@ less than the one of @++>@ and @<++@ of the operators, so use parentheses when appropriate,
-- unless the we want to enclose all the widgets in the right side.
-- Most of the type errors in the DSL are due to the low priority of this operator.
--
-- This is a widget, which is a table with some links. it returns an Int
--
-- > import MFlow.Forms.Blaze.Html
-- >
-- > tableLinks :: View Html Int
-- > table ! At.style "border:1;width:20%;margin-left:auto;margin-right:auto"
-- >            <<< caption << text "choose an item"
-- >            ++> thead << tr << ( th << b << text  "item" <> th << b << text "times chosen")
-- >            ++> (tbody
-- >                 <<< tr ! rowspan "2" << td << linkHome
-- >                 ++> (tr <<< td <<< wlink  IPhone (b << text "iphone") <++  td << ( b << text (fromString $ show ( cart V.! 0)))
-- >                 <|>  tr <<< td <<< wlink  IPod (b << text "ipad")     <++  td << ( b << text (fromString $ show ( cart V.! 1)))
-- >                 <|>  tr <<< td <<< wlink  IPad (b << text "ipod")     <++  td << ( b << text (fromString $ show ( cart V.! 2))))
-- >                 )
(<<<) :: (Monad m,  Monoid view)
          => (view ->view)
         -> View view m a
         -> View view m a
(<<<) v form= View $ do
  FormElm f mx <- runView form 
  return $ FormElm (v  f) mx


infixr 5 <<<






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
  return $ FormElm ( f <> v) mx

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



-- | Empty widget that does not validate. May be used as \"empty boxes\" inside larger widgets.
--
-- It returns a non valid value.
noWidget ::  (FormInput view,
     Monad m, Functor m) =>
     View view m a
noWidget= Control.Applicative.empty

-- | a sinonym of noWidget that can be used in a monadic expression in the View monad does not continue
stop :: (FormInput view,
     Monad m, Functor m) =>
     View view m a
stop= Control.Applicative.empty

-- | Render a Show-able  value and return it
wrender
  :: (Monad m, Functor m, Show a, FormInput view) =>
     a -> View view m a
wrender x = (fromStr $ show x) ++> return x

-- | Render raw view formatting. It is useful for displaying information.
wraw :: Monad m => view -> View view m ()
wraw x= View . return . FormElm x $ Just ()
-------------------------


instance   FormInput JSBuilder  where
    toByteString  =  error "toByteString not defined"
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



raiseEvent :: View JSBuilder IO a -> Event IO b -> View JSBuilder IO a
raiseEvent w event = View $ do
   action <- gets process
   FormElm render mx <- runView  w
   let render' = do
        addEvent (render :: JSBuilder) event  action
--        addAttr render "autofocus" ""
   return $ FormElm render' mx


runWidget action e = do
     let iteration= runWidget action e
     (FormElm render mx, s) <- runStateT (runView action) mFlowState0{process = iteration}
     build render e
     return ()


--refresh w= View $ do
--    id <- genNewId
--    me <- elemById id
--    FormElm render mx <- runView w
--    case me of
--      Just e ->  do
--         clearChildren e
--         let JSBuilder be= render
--         let render'= JSBuilder . const $ be e
--
--         return (FormElm ( render') mx)
--      Nothing -> return $ FormElm (nelem "div" `attr` ("id",id) `child` render) mx

refresh w= View $ do
    id <- genNewId
    me <- elemById id
    FormElm render mx <- runView w
    case me of
      Just e -> do
         setAttr e "id" $ id <> "old"
         let render'= render
                          <> JSBuilder (\e' -> do
--                                   parent <- parentNode e
--                                   chs    <- getChildren parent :: IO [Elem]
--                                   atr    <- getAttr( Prelude.head chs) "id"
--                                   print "hello"
--                                   print atr
--                                   removeChild e parent  -- remove the old node
                                   clearChildren e
                                   return e')
         return $ FormElm (nelem "div" `attr` ("id",id) `child` render') mx

      Nothing -> return $ FormElm (nelem "div" `attr` ("id",id) `child` render) mx

--norefresh w = View $ do
--    id <- genNewId
--    me <- elemById  id
--    FormElm render mx <- runView w
--    case me of
--     Nothing -> return $ FormElm (nelem "div" `attr` ("id",id) `child` render) mx
--     Just _ ->  return $ FormElm mempty mx

norefresh w = View $ do
    id <- genNewId

    FormElm render mx <- runView w

    me <- elemById id
    case me of
     Nothing -> do
        midold <- elemById $ id <> "old"
        case  midold of

           Nothing -> return $ FormElm (nelem "div" `attr` ("id",id) `child` render) mx
           Just elem  ->  do -- found the old node. it has been renamed by a previus "refresh"
               let render'= JSBuilder $ \e -> do
                                   parent <- parentNode elem
                                   removeChild elem parent  -- remove the old node
                                   setAttr elem  "id" id
                                   addChild elem e
                                   return e
               return $ FormElm render' mx
     Just _ -> -- the node is there, keep it
           return $ FormElm mempty mx





--updateWith expr :: set value with a expression
--  expr executed after the main expr.
