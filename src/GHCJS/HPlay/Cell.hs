-----------------------------------------------------------------------------
--
-- Module      :  Cell
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
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, CPP #-}
module GHCJS.HPlay.Cell  where
import Transient.Base
import GHCJS.HPlay.View
import Control.Monad.IO.Class
import Data.Typeable
import Unsafe.Coerce
import qualified Data.Map as M hiding ((!))
import System.IO.Unsafe
import Data.IORef
import Control.Monad
import Data.Maybe
import Control.Exception
import Data.List

#ifdef ghcjs_HOST_OS
import qualified Data.JSString as JS
#endif

data Cell  a = Cell { mk :: Maybe a -> Widget a
                    , setter ::  a -> IO ()
                    , getter ::  IO (Maybe a)}

--instance Functor Cell where
--  fmap f cell = cell{setter= \c x ->  c .= f x, getter = \cell -> get cell >>= return . f}



-- a box cell with polimorphic value, identified by a string
boxCell :: (Show a, Read a, Typeable a) => ElemID -> Cell a
boxCell id = Cell{ mk= \mv -> getParam (Just id) "text" mv
                 , setter= \x -> do
                          me <- elemById id
                          case me of
                            Just e -> setProp e "value" (toJSString $ show1 x)
                            Nothing -> return ()

                 , getter= do
                          me <- elemById id
                          case me of
                            Nothing -> return Nothing
                            Just e -> getit}
    where
    getit= withElem id $ \e ->  getProp e "value" >>=  return . read1
    read1 s= if typeOf(typeIO getit) /= typestring
               then case readsPrec 0 $ fromJSString s  of
                   [(v,_)] ->  Just v
                   _  -> Nothing
               else Just $ unsafeCoerce s
    typeIO :: IO(Maybe a) -> a
    typestring= typeOf (undefined :: String)
    typeIO = undefined
    show1 x= if typeOf x== typestring
            then unsafeCoerce x
            else show x





-- | Cell assignment
(.=) :: MonadIO m =>  Cell a -> a -> m ()
(.=) cell x = liftIO $ (setter cell )  x

get cell =  Transient $ liftIO (getter cell)


---- |  a cell value assigned to other cell
--(..=) :: Cell a -> Cell a -> Widget ()
--(..=) cell cell'= get cell' >>= (cell .= )

infixr 0 .=  -- , ..=

-- experimental: to permit cell arithmetic

--instance Num a => Num (Cell a) where
--  c + c'= Cell undefined undefined  $
--            do r1 <- getter c
--               r2 <- getter c'
--               return $  liftA2 (+) r1  r2
--
--  c * c'= Cell undefined undefined $
--            do r1 <- getter c
--               r2 <- getter c'
--               return $ liftA2 (+) r1  r2
--
--  abs c= c{getter=  getter c >>= return . fmap abs}
--
--  signum c= c{getter=  getter c >>= return . fmap signum}
--
--  fromInteger i= Cell  undefined undefined  . return $ Just $ fromInteger i


-- *  Spradsheet type cells
-- Implement a solver that allows circular dependencies . See
-- > http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit

-- The recursive Cell calculation DSL BELOW ------


-- | get a cell for the spreadsheet expression
gcell ::  Num a => String -> M.Map String a -> a
gcell n= \vars -> case M.lookup n vars of
    Just exp -> inc n  exp
    Nothing -> error $ "cell error in: "++n
  where
  inc n exp= unsafePerformIO $ do
     tries <- readIORef rtries
     if tries <= maxtries
       then  do
          writeIORef rtries  (tries+1)
          return exp

       else  error n


-- a parameter is a function of all of the rest parameters
type Expr a = M.Map JS.JSString a -> a

rtries= unsafePerformIO $ newIORef $ (0::Int)
maxtries=  3* (M.size $ unsafePerformIO $ readIORef rexprs)

rexprs :: IORef (M.Map JS.JSString (Expr Float))
rexprs= unsafePerformIO $ newIORef M.empty

rmodified :: IORef (M.Map JS.JSString (Expr Float))
rmodified= unsafePerformIO $ newIORef M.empty



mkscell name val expr= mk (scell name expr) val

scell id  expr= Cell{ mk= \mv-> static $ do
                           liftIO $ do
                             exprs <- readIORef rexprs
                             writeIORef rexprs $ M.insert id expr exprs

                           r <- getParam (Just id) "text" mv `fire` OnKeyUp
                           liftIO $ do
                                mod <- readIORef rmodified
                                writeIORef rmodified  $ M.insert  id (const r)  mod
                           return r
                         `continuePerch`  id



                 , setter= \x -> withElem id $ \e -> setProp e "value" (toJSString $ show1 x)

                 , getter= getit}
    where

    getit= withElem id $ \e -> getProp e "value" >>= return . read1
    read1 s= if typeOf(typeIO getit) /= typeOf (undefined :: String)
               then case readsPrec 0 $ fromJSString s  of
                   [(v,_)] ->  Just v
                   _  -> Nothing
               else unsafeCoerce s
    typeIO :: IO(Maybe a) -> a
    typeIO = undefined
    show1 x= if typeOf x== typeOf (undefined :: String)
            then unsafeCoerce x
            else show x



calc :: Widget ()
calc= do
  nvs <- liftIO $ readIORef rmodified
  when (not $ M.null nvs) $ do
    values <-liftIO $ handle doit calc1
    mapM_ (\(n,v) -> boxCell n .= v)  values
  liftIO $ writeIORef rmodified M.empty
  where
  -- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
  -- loeb ::  Functor f => f (t -> a) -> f a
  loeb :: M.Map JS.JSString (Expr a) -> M.Map JS.JSString a
  loeb x = fmap (\a -> a (loeb  x)) x

  calc1  :: IO [(JS.JSString,Float)]
  calc1= do
    writeIORef rtries 0
    cells <- liftIO $ readIORef rexprs
    nvs   <- liftIO $ readIORef rmodified
    let mvalues = M.union nvs  cells
        evalues = loeb mvalues

    toStrict $ M.toList evalues

  toStrict xs = print xs >> return xs

  circular n= "loop detected in cell: "++ show n  ++ " please fix the error"

  doit :: SomeException -> IO [(JS.JSString,Float)]
  doit e= do
    nvs <- readIORef rmodified
    exprs <- readIORef rexprs
    case  M.keys exprs \\ M.keys nvs of
      [] -> do
         let Just (ErrorCall n)= fromException e
         let err= circular n
         alert $ toJSString err
         error err
      (name:_) -> do
         mv <- getter $ boxCell name
         case mv of
            Nothing -> return []
            Just v -> do
                writeIORef rmodified  $ M.insert name (const v) nvs
                calc1


instance (Num a,Eq a,Fractional a) =>Fractional (x -> a)where
     f / g = \x -> f x / g x
     fromRational = error "fromRational not implemented"


instance (Num a,Eq a) => Num (x -> a) where
     fromInteger = const . fromInteger
     f + g = \x -> f x + g x
     f * g = \x -> f x * g x
     negate = (negate .)
     abs = (abs .)
     signum = (signum .)
