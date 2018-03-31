{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import           Control.Monad
import           Data.Semigroup

import qualified LLVM.Relocation as Reloc
import qualified LLVM.CodeModel as CodeModel
import qualified LLVM.CodeGenOpt as CodeGenOpt

import           LLVM.Context
import           LLVM.Linking (loadLibraryPermanently, getSymbolAddressInProcess)
import           LLVM.Module
import           LLVM.OrcJIT
import           LLVM.Target hiding (withHostTargetMachine)

import           Data.Int
import           Foreign.Ptr

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

withHostTargetMachine :: (TargetMachine -> IO a) -> IO a
withHostTargetMachine f = do
  initializeAllTargets
  triple <- getProcessTargetTriple
  cpu <- getHostCPUName
  features <- getHostCPUFeatures
  (target, _) <- lookupTarget Nothing triple
  withTargetOptions $ \options ->
    withTargetMachine target triple cpu features options Reloc.PIC CodeModel.Default CodeGenOpt.Default f

eagerJit :: IO Int32
eagerJit =
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
                return result

main :: IO ()
main = do
  b <- loadLibraryPermanently Nothing
  unless (not b) (error "Couldn’t load library")
  res <- eagerJit
  putStrLn ("JIT result: " <> show res)
