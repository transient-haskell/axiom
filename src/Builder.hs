-----------------------------------------------------------------------------
--
-- Module      :  Builder
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | Monad and Monoid instances for a builder that hang DOM elements from the
-- current parent element. It uses Haste.DOM from the haste-compiler
--
-----------------------------------------------------------------------------
{-#LANGUAGE TypeSynonymInstances, FlexibleInstances
            , OverloadedStrings, DeriveDataTypeable #-}
module Builder where
import Data.Typeable
import Haste
import Haste.DOM
import Haste.Foreign(ffi)
import Data.Maybe
import Data.Monoid
import Unsafe.Coerce

newtype JSBuilderM a= JSBuilder{build :: Elem -> IO Elem} deriving Typeable

type JSBuilder = JSBuilderM ()

instance Monoid (JSBuilderM a) where
    mappend mx my= JSBuilder $ \e -> do
         build mx e
         build my e
         return e
    mempty  = JSBuilder return

instance Monad JSBuilderM where
   (>>) x y= mappend (unsafeCoerce x) y
   (>>=) = error "bind (>>=) invocation creating DOM elements"
   return  = mempty



class ToElem a where
  toElem :: a -> JSBuilder

instance ToElem String where
   toElem s= JSBuilder $ \e ->do
        e' <- newTextElem s
        addChild e' e
        return e'

--instance Show a => ToElem a where toElem = toElem . show

instance ToElem (JSBuilderM a) where toElem e = unsafeCoerce e

attr tag (n, v)=JSBuilder $ \e -> do
        tag' <- build tag e
        setAttr tag' n v
        return tag'


nelem s= JSBuilder $ \e ->do
        e' <- newElem s
        addChild e' e
        return e'

child :: ToElem a => JSBuilder -> a -> JSBuilder
child me ch= JSBuilder $ \e' -> do
        e <- build me e'
        let t = toElem ch
        r <- build t e
        return e

addEvent :: JSBuilder -> Event IO b -> IO () -> JSBuilder
addEvent be event action= JSBuilder $ \e -> do
        e' <- build be e
        onEvent e event $ unsafeCoerce $ action >> focus e
        return e'

elemsByTagName :: String -> IO [Elem]
elemsByTagName = ffi  "(function(s){document.getElementsByTagName(s);})"

br= nelem "br"

