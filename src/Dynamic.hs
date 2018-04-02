{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Control.Monad
import Data.Int
import Foreign.Ptr

import LLVM.Context
import LLVM.Linking (loadLibraryPermanently, getSymbolAddressInProcess)
import LLVM.Module
import LLVM.OrcJIT
import LLVM.Target

foreign import ccall "dynamic"
  mkFun :: FunPtr (IO Int32) -> IO Int32

resolver :: IRCompileLayer l -> SymbolResolver
resolver compileLayer =
  SymbolResolver
    (\s -> findSymbol compileLayer s True)
    (\s ->
       fmap
         (\a -> JITSymbol a (JITSymbolFlags False True))
         (getSymbolAddressInProcess s))

main :: IO ()
main = do
  b <- loadLibraryPermanently Nothing
  unless (not b) (error "Couldn’t load library")
  withContext $ \ctx ->
    withModuleFromLLVMAssembly ctx (File "module.ll") $ \mod' ->
      withHostTargetMachine $ \tm ->
        withObjectLinkingLayer $ \objectLayer ->
          withIRCompileLayer objectLayer tm $ \compileLayer -> do
            withModule
              compileLayer
              mod'
              (resolver compileLayer) $
              \_ -> do
                mainSymbol <- mangleSymbol compileLayer "f"
                (JITSymbol mainFn _) <- findSymbol compileLayer mainSymbol True
                unless (mainFn /= WordPtr 0) (error "Couldn’t find JIT symbol")
                result <- mkFun (castPtrToFunPtr (wordPtrToPtr mainFn))
                print result
