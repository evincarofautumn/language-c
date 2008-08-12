-----------------------------------------------------------------------------
-- |
-- Module      :  Language.C
-- Copyright   :  (c) 2008 Benedikt Huber
--                [1995..2007]
--                   Manuel M. T. Chakravarty
--                   Duncan Coutts
--                   Betram Felgenhauer
-- License     :  BSD-style
-- Portability :  portable
-- Stability   :  provisional
--
-- The C99 parser and pretty printer library.
-----------------------------------------------------------------------------
module Language.C (
    parseCFile,
    module Language.C.Data,
    module Language.C.Syntax,
    module Language.C.Pretty,
    module Language.C.Parser,
)
where
import Language.C.Data
import Language.C.Syntax
import Language.C.Pretty
import Language.C.Parser
import Language.C.System.Preprocess

-- | preprocess and parse C file
--   Synopsis: @parseFile preprocesssor tmp-dir? cpp-opts file@
parseCFile :: (Preprocessor cpp) => cpp -> (Maybe FilePath) -> [String] -> FilePath -> IO (Either ParseError CTranslUnit)
parseCFile cpp tmp_dir_opt args input_file = do
    input_stream <- if not (isPreprocessed input_file)
                        then  let cpp_args = (rawCppArgs args input_file) { cppTmpDir = tmp_dir_opt }
                              in  runPreprocessor cpp cpp_args >>= handleCppError
                        else  readInputStream input_file
    return$ parseC input_stream (Position input_file 1 1)
    where
    handleCppError (Left exitCode) = fail $ "Preprocessor failed with " ++ show exitCode
    handleCppError (Right ok)      = return ok
