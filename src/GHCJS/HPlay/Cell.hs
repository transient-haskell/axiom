-----------------------------------------------------------------------------
--
-- Module      :  Cell
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverloadedStrings, CPP, ScopedTypeVariables #-}
module GHCJS.HPlay.Cell(Cell(..),boxCell,(.=),get,mkscell,scell, gcell, calc)  where
import Transient.Base
import Transient.Move
import Transient.Internals (runTransState)

import GHCJS.HPlay.View
import Data.Typeable
import Unsafe.Coerce
import qualified Data.Map as M hiding ((!))
import System.IO.Unsafe
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Control.Exception
import Data.List
import GHCJS.Perch
import Control.Exception

#ifdef ghcjs_HOST_OS

import Data.JSString hiding (empty)

#else

type JSString = String

#endif

data Cell  a = Cell { mk :: Maybe a -> Widget a
                    , setter ::  a -> IO ()
                    , getter ::  IO (Maybe a)}

--instance Functor Cell where
--  fmap f cell = cell{setter= \c x ->  c .= f x, getter = \cell -> get cell >>= return . f}



-- | creates a input box cell with polimorphic value, identified by a string.
-- the cell can be updated programatically
boxCell :: (Show a, Read a, Typeable a) => ElemID -> Cell a
boxCell id = Cell{ mk= \mv -> getParam  (Just id) "text" mv
                 , setter= \x -> do
                          me <- elemById id
                          case me of
                            Just e ->  setProp e "value" (toJSString $ show1 x)
                            Nothing -> return ()

                 , getter= getit id}

getit id = withElem id $ \e -> do
  ms <- getValue e
  case ms of
    Nothing -> return Nothing
    Just s  -> return $ read1  s
  where
  read1 s=
      if typeOf(typeIO getit) /= typestring
           then case readsPrec 0  s  of
               [(v,_)] -> v `seq` Just v
               _       -> Nothing
           else Just $ unsafeCoerce s

typeIO :: (ElemID -> IO (Maybe a)) -> a
typeIO = undefined

typestring= typeOf (undefined :: String)

show1 x= if typeOf x== typestring
        then unsafeCoerce x
        else show x

instance Attributable (Cell a) where
 (Cell mk setter getter) ! atr = Cell (\ma -> mk ma ! atr) setter getter



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


-- | within a `mkscell` formula, `gcell` get the the value of another cell using his name.
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
gcell ::   JSString -> TransIO Double
gcell n= do
  vars <- liftIO $ readIORef rvars
  case M.lookup n vars  of
    Just exp -> inc n  exp
    Nothing -> error $ "cell not found: "++ show n
  where
  inc n exp= unsafePerformIO $ do
     tries <- readIORef rtries
     if tries <= maxtries
       then  do
          writeIORef rtries  (tries+1)
          return exp

       else  throw Loop

data Loop= Loop deriving (Show,Typeable)

instance Exception Loop

-- a parameter is a function of all of the rest
type Expr a = TransIO a

rtries= unsafePerformIO $ newIORef $ (0::Int)
maxtries=  3 * (M.size $ unsafePerformIO $ readIORef rexprs)

rexprs :: IORef (M.Map JSString (Expr Double))
rexprs= unsafePerformIO $ newIORef M.empty      -- initial expressions

rvars :: IORef (M.Map JSString (Expr Double))
rvars= unsafePerformIO $ newIORef M.empty        -- expressions actually used for each cell.
                                                -- initially, A mix of reexprs and rmodified
                                                -- and also contains the result of calculation

rmodified :: IORef (M.Map JSString (Expr Double))
rmodified= unsafePerformIO $ newIORef M.empty    -- cells modified by the user or by the loop detection mechanism


-- | make a spreadsheet cell. a spreadsheet cell is an input-output box that takes input values from
-- the user, has an expression associated and display the result value after executing `calc`
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
mkscell :: JSString -> Maybe Double -> Expr Double -> TransIO Double
mkscell name val expr= mk (scell name expr) val

both mx= local $ runCloud mx <** runCloud ( atRemote  $ mx)

scell :: JSString -> Expr Double -> Cell Double
scell id  expr= Cell{ mk= \mv->  runCloud $ do
                           both $ lliftIO $ do
                             exprs <- readIORef rexprs
                             writeIORef rexprs $ M.insert id expr exprs

                           r <- local $ getParam (Just id) "text"  mv `fire` OnKeyUp

                           both $ lliftIO $  do
                               mod <-  readIORef rmodified
                               writeIORef rmodified  $ M.insert  id (return  r)  mod
                           return r
                       --  `continuePerch`  id



                     , setter= \x -> withElem id $ \e -> setProp e "value" (toJSString $ show1 x)

                     , getter= getit id}





-- | executes the spreadsheet adjusting the vaules of the cells created with `mkscell` and solving loops
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
calc :: TransIO ()
calc=  do
  st <- getCont
  liftIO  $ handle (removeVar st) $ run' st $  do
          nvs <- liftIO $ readIORef rmodified

          when (not $ M.null nvs) $ do
            values <-  calc1
            mapM_ (\(n,v) -> boxCell n .= v)  values
          liftIO $ writeIORef rmodified M.empty
--   return ()
  where
  run' st x=  runTransState st x >> return ()


  checktries x= unsafePerformIO $ do
         n <-  readIORef rtries
         if (n> maxtries) then  error "loop"
                          else writeIORef rtries $ n+1


  calc1  :: TransIO [(JSString,Double)]
  calc1= do
    liftIO $ writeIORef rtries 0
    cells <- liftIO $ readIORef rexprs
    nvs   <- liftIO $ readIORef rmodified
    liftIO $ writeIORef rvars $ M.union nvs cells
    solve




  circular n= "loop detected in cell: "++ show n  ++ " please fix the error"

--  removeVar :: EventF -> SomeException -> IO () -- [(JSString,Double)]
  removeVar st  = \(e:: Loop) -> handle (removeVar st) $ do


    nvs <- readIORef rmodified
    exprs <- readIORef rexprs

    case  M.keys exprs \\ M.keys nvs of
      [] -> do

         error "no more input variables"
      (name:_) -> do
         mv <-  getit name

         case mv of
            Nothing -> return ()
            Just v  -> do
                writeIORef rmodified  $ M.insert name ( return v) nvs
                return ()
                runTransState st calc
                return ()

  -- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
  -- loeb ::  Functor f => f (t -> a) -> f a
  --  loeb x = fmap (\a ->  a (loeb  x)) x
  -- loeb :: [([a]-> a)] -> [a]
  -- loeb x=  map (\f ->  f (loeb  x)) x

--loeb :: [([a] -> IO a)] -> IO [a]
--loeb x= mapM (\f -> loeb x >>= f) x -- fail does not terminate




--loeb x=  map (\f ->  f (loeb  x)) x

--solve  :: M.Map JSString (TransIO a) -> TransIO (M.Map JSString a)
solve :: TransIO [(JSString,Double)]
solve = do
 vars <- liftIO $ readIORef rvars
 mapM (solve1 vars) $ M.toList vars
 where
 solve1 vars (k,f)= do
    x <- f

    liftIO $ writeIORef rvars $ M.insert k (return x) vars
    return (k,x)



instance (Num a,Eq a,Fractional a) =>Fractional (TransIO a)where
     mf / mg = do
        f <- mf
        g <- mg
        return $ f  / g
     fromRational = error "fromRational not implemented"


instance (Num a,Eq a) => Num (TransIO a) where
     fromInteger = return . fromInteger
     f + g = f >>= \x -> g >>= \y -> return $ x + y
     f * g = f >>= \x -> g >>= \y -> return $ x * y
     negate f = f >>= return . negate
     abs f =  f >>= return . abs
     signum f =  f >>= return . signum
