{-# LANGUAGE CPP, ScopedTypeVariables, TypeFamilies, FlexibleContexts, ForeignFunctionInterface, JavaScriptFFI, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FunctionalDependencies, FlexibleInstances, UndecidableInstances, GeneralizedNewtypeDeriving, BangPatterns, RankNTypes, GADTs #-}
module Main (main) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import GHCJS.Types
import Control.Concurrent
import System.Mem
import Data.IORef

main :: IO ()
main = do
  subscribedRef <- newIORef "Test Value"
  putStrLn "Created"
  _ <- mkWeakIORef subscribedRef $ putStrLn "Finalized"
  let eurl = subscribedRef
  e <- do
#ifdef WORKING
    return $ mapE (return . Just . (build undefined undefined :: IO () -> M ())) eurl
#else
    widgetHoldInternal' undefined eurl
#endif
  forever $ do
    putStrLn =<< readIORef e
    performGC
    threadDelay 1000000000

newtype M a = M (ReaderT (IO () -> IO ()) IO a) deriving (Functor, Applicative, Monad, MonadIO)

class Monad m => E e m | e -> m where
  mapE :: (a -> m b) -> e -> e

instance E (IORef String) IO where
  mapE _ x = x

class (Monad h, Monad m) => C h m | m -> h where
  askM :: m (h () -> IO ())

instance C IO M where
  askM = M ask

class (E t IO, MonadIO h, C IO h) => D t h where

instance D (IORef String) M

class Monad m => HasVal m where
  askVal :: m JSVal

instance HasVal IO where
  askVal = return undefined

{-# NOINLINE widgetHoldInternal' #-}
widgetHoldInternal' :: D (IORef String) M => JSVal -> IORef String -> IO (IORef String)
widgetHoldInternal' p newChild = do
  doc <- askVal
  return $ mapE (return . (build doc (id p) :: IO () -> M ())) newChild

{-# NOINLINE [1000000] build #-}
build :: (MonadIO m, C IO m) => JSVal -> JSVal -> IO () -> m ()
build doc self c = do
  Just newChild <- liftIO $ Just <$> js_a doc
  liftIO c
  m <- askM
  liftIO $ m $ return ()
  _ <- liftIO $ js_b self newChild
  return ()

foreign import javascript unsafe "null" js_a :: JSVal -> IO JSVal

foreign import javascript unsafe "null" js_b :: JSVal -> JSVal -> IO ()
