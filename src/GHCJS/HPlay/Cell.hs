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
module GHCJS.HPlay.Cell(Cell(..),boxCell,bcell,(.=),get,mkscell,scell, gcell, calc)  where
import Transient.Internals
import Transient.Move --hiding (JSString)
import GHCJS.HPlay.View
import Data.Typeable
import Unsafe.Coerce
import qualified Data.Map as M hiding ((!))

import Control.Monad.State hiding (get)
import Control.Monad
import Data.Monoid
import Data.List
import Control.Exception
import Data.IORef
import System.IO.Unsafe
#ifdef ghcjs_HOST_OS

import Data.JSString hiding (empty)

#else

-- type JSString = String

#endif

data Cell a  = Cell { mk :: Maybe a -> Widget a
                    , setter ::  a -> IO ()
                    , getter ::  IO (Maybe a)}

--instance Functor Cell where
--  fmap f cell = cell{setter= \c x ->  c .= f x, getter = \cell -> get cell >>= return . f}

-- | creates (but not instantiates) an input box that has a setter and a getter. To instantiate it us his method `mk`
bcell :: (Show a, Read a, Typeable a) =>TransIO (Cell a)
bcell= genNewId >>= return . boxCell

-- | creates (but not instantiates) a input box cell with polimorphic value, identified by a string.
-- the cell has a getter and a setter. To instantiate it us his method `mk`
boxCell :: (Show a, Read a, Typeable a) => ElemID -> Cell a
boxCell id = Cell{ mk= \mv -> getParam  (Just id) "text" mv
                 , setter= \x -> do
                          me <- elemById id
                          case me of
                            Just e ->  setProp e "value" (toJSString $ show1 x)
                            Nothing -> return ()

                 , getter= getID id}

getID id = withElem id $ \e -> do
  ms <- getValue e
  case ms of
    Nothing -> return Nothing
    Just s  -> return $ read1  s
  where
  read1 s=
      if typeOf(typeIO getID) /= typestring
           then case readsPrec 0  s  of
               [(v,_)] -> v `seq` Just v
               _       -> Nothing
           else Just $ unsafeCoerce s

typeIO :: (ElemID -> IO (Maybe a)) -> a
typeIO = undefined

typestring :: TypeRep
typestring= typeOf (undefined :: String)

show1 :: (Show a, Typeable a) => a -> String
show1 x= if typeOf x== typestring
        then unsafeCoerce x
        else show x

instance Attributable (Cell a) where
 (Cell mk setter getter) ! atr = Cell (\ma -> mk ma ! atr) setter getter



-- | Cell assignment using the cell setter
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
gcell ::   JSString -> Cloud Double
gcell n= loggedc $ do
  -- onAll $ do
  --    cutExceptions
  --    reportBack
  vars <- getCloudState rvars <|> return M.empty  -- liftIO $ readIORef rvars
  localIO  $ print ("gcell", n)
  case M.lookup n vars of
    Just exp -> do  inc ;  exp  !> "executing exp"
    Nothing -> error $ "cell not found: " ++ show n
  where
  inc  =  do
     Tries tries maxtries <- getCloudState rtries <|> error "no tries" --do
                                      -- Exprs exprs <- getCloudState
                                      -- return . Tries 0 $ 3 * (M.size $  exprs)
     localIO $ print tries
     if tries <= maxtries
       then  localIO $  writeIORef rtries $ Tries (tries+1) maxtries
       else  local $ do
         -- liftIO $ print "back"
         back Loop

data Loop= Loop deriving (Show,Typeable)

instance Exception Loop

-- a parameter is a function of all of the rest
type Expr a = Cloud a

data Tries= Tries Int Int deriving Typeable
rtries= unsafePerformIO $ newIORef $ Tries 0 0
--maxtries=  3 * (M.size $ unsafePerformIO $ readIORef rexprs)

-- newtype Exprs= Exprs (M.Map JSString (Expr Double))
rexprs :: IORef (M.Map JSString (Expr Double))
rexprs= unsafePerformIO $ newIORef M.empty      -- initial expressions

-- newtype Vars= Vars (M.Map JSString (Expr Double))
rvars :: IORef (M.Map JSString (Expr Double))
rvars= unsafePerformIO $ newIORef M.empty       -- expressions actually used for each cell.
                                                -- initially, A mix of reexprs and rmodified
                                                -- and also contains the result of calculation

-- newtype Modified= Modified (M.Map JSString (Expr Double)) deriving Typeable
rmodified :: IORef (M.Map JSString ( Double))
rmodified= unsafePerformIO $ newIORef M.empty    -- cells modified by the user or by the loop detection mechanism


-- | make a spreadsheet cell. a spreadsheet cell is an input-output box that takes input values from
-- the user, has an expression associated and display the result value after executing `calc`
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
mkscell :: JSString -> Expr Double -> Cloud (Cell Double)
mkscell  name  expr= do
  exprs <- onAll $ liftIO (readIORef rexprs) <|> return ( M.empty) -- readIORef rexprs
  onAll $ liftIO $ writeIORef  rexprs $ M.insert name expr exprs
  return $ scell name expr



scell :: JSString -> Expr Double -> Cell Double
scell id  expr= Cell{ mk= \mv -> Widget $  do
                           r <- norender $ getParam (Just id) "text"  mv `fire` OnChange
                           mod <-  liftIO (readIORef rmodified) <|> return( M.empty)
                           liftIO $ writeIORef rmodified $ M.insert id  r mod
                           return r

                    , setter= \x -> withElem id $ \e -> setProp e "value" (toJSString $ show1 x)

                    , getter= getID id}






-- | executes the spreadsheet adjusting the vaules of the cells created with `mkscell` and solving loops
--
-- see http://tryplayg.herokuapp.com/try/spreadsheet.hs/edit
calc :: Cloud ()
calc=  do
  mod <- localIO $ readIORef rmodified
  onAll $ liftIO $ print ("LENGTH MOD", M.size mod)
  onAll $ liftIO $ print "setCloudState modified"
  setCloudState rmodified mod
  exprs <- getCloudState rexprs
  onAll $ liftIO $ print "setCloudState exprs"
  setCloudState rexprs exprs
  onAll $ liftIO $ print "setCloudState rvars"

  setCloudState rvars  M.empty

  onAll $ return() `onBack` (\(e::Loop) -> runCloud'  $ do localIO $ print "REMOVEVAR"; removeVar  e; local (forward Loop) )
  exprs <- getCloudState rexprs <|> error "no exprs"
  onAll $ liftIO $ print "setCloudState rtries"

  setCloudState rtries $ Tries 0 $ 3 * (M.size $  exprs)
  nvs <- getCloudState rmodified <|> error "no modified" -- liftIO $ readIORef rmodified

  onAll $ liftIO $ print ("LENGTH NVS", M.size nvs)
  when (not $ M.null nvs) $ calc1
            --values <-  calc1
            --localIO $ print "NEW CALC"
            --local $ mapM_ (\(n,v) -> boxCell n .= v)  values
  onAll $ liftIO $ print "setCloudState modified"
  setCloudState rmodified M.empty

  where


  --calc1  :: Expr [(JSString,Double)]
  calc1= do
      return () !> "CALC1"
      cells    <- getCloudState rexprs <|> error "no exprs" -- liftIO $ readIORef rexprs
      nvs      <- getCloudState rmodified <|> error "no modified2" -- liftIO $ readIORef rmodified
      onAll $ liftIO $ print "setCloudState vars"

      setCloudState rvars $ M.union (M.map return nvs) cells

      solve

  --solve :: Expr [(JSString,Double)]
  solve =  do
     vars <- getCloudState rvars <|> error "no vars" --  liftIO $ readIORef rvars
     onAll $ liftIO $ print $ ("LENGHT VARS", M.size vars)
     mapM_ (solve1 vars) $ M.toList vars
     where
     solve1 vars (k,f)= do
        localIO $ print ("solve1",k)
        x <- f
        localIO $ print ("setcloudstate var",k,x)
        local $ boxCell k .= x
        setCloudState rvars $ M.insert k (return x) vars
        return () -- (k,x) :: Expr (JSString,Double)


setCloudState r v=  allNodes $  writeIORef r v
getCloudState r= onAll . liftIO $ readIORef r

--  removeVar ::SomeException -> IO () -- [(JSString,Double)]
removeVar = \(e:: Loop) -> do
  nvs <- getCloudState rmodified <|>  error "no modified 3"-- readIORef rmodified
  -- mapM (\n -> snd n >>= \v ->  localIO $ print (fst n,v)) $ M.toList nvs
  exprs  <- getCloudState rexprs <|>  error " no Exprs2" --readIORef rexprs

  case  M.keys exprs \\ M.keys nvs of
    [] -> error "non solvable circularity in cell dependencies"
    (name:_) -> do
      localIO $ print ("removeVar",name)

      mv <- localIO $ getID name

      case mv of
          Nothing -> return ()
          Just v  -> do
              onAll $ liftIO $ print "setCloudState modified"
              setCloudState  rmodified  $ M.insert name v nvs
              return ()

allNodes :: IO () -> Cloud ()
allNodes mx= loggedc $ (localIO mx)  <> (atRemote $ (localIO $ print "UPDATE" >> mx))

--atBrowser mx= if isBrowserInstance then mx else atRemote mx

--atServer mx= if not isBrowserInstance then mx else atRemote mx
  
  -- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
  -- loeb ::  Functor f => f (t -> a) -> f a
  --  loeb x = fmap (\a ->  a (loeb  x)) x
  -- loeb :: [([a]-> a)] -> [a]
  -- loeb x=  map (\f ->  f (loeb  x)) x

--loeb :: [([a] -> IO a)] -> IO [a]
--loeb x= mapM (\f -> loeb x >>= f) x -- fail does not terminate



--loeb x=  map (\f ->  f (loeb  x)) x


