#!/usr/bin/env runhaskell

> import Distribution.Simple
> import Distribution.Simple.PreProcess
> import Distribution.Simple.Utils
> import Distribution.PackageDescription
> import Distribution.Simple.LocalBuildInfo
> import System.Process (callCommand)

> main = defaultMainWithHooks
>   simpleUserHooks
>     { hookedPreProcessors = [("erb", runErb)] }

> runErb _ _ =
>   PreProcessor
>     { platformIndependent = True
>     , runPreProcessor = mkSimplePreProcessor $ \fin fout verbosity -> do
>       info verbosity $ "erb-processing " ++ fin ++ " to " ++ fout
>       callCommand $ "erb " ++ show fin ++ " > " ++ show fout
>     }
