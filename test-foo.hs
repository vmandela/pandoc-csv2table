{-# LANGUAGE QuasiQuotes #-}
module Main where

import Text.Table.Tablify
import System.Exit (exitFailure, exitSuccess)
import Text.RawString.QQ
import Text.Pandoc
import Text.Pandoc.Extensions
import Text.Pandoc.Walk
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad

inp :: String
inp = [r|Hi

~~~{.table}
Header 1,Header 2
Row 1 Col 1,Row 1 Col 2
~~~


  Right     Left     Center     Default
-------     ------ ----------   -------
     12     12        12            12
    123     123       123          123
      1     1          1             1

Table:  Demonstration of simple table syntax.

|]

syntree = runPure $ readMarkdown ropt (T.pack inp)


ropt :: ReaderOptions
ropt = def
    { -- The following option causes pandoc to read smart typography, a nice
      -- and free bonus.
      readerExtensions = foldr enableExtension  pandocExtensions [Ext_smart]
    }


-- pandocCompilerDiagramsWith :: ReaderOptions -> WriterOptions -> Compiler (Item String)
-- pandocCompilerDiagramsWith ropt wopt = pandocCompilerWithTransformM ropt wopt diagramsTransformer

-- diagramsTransformer :: Pandoc -> Compiler Pandoc
-- diagramsTransformer pandoc = unsafeCompiler $ (renderCSV pandoc)

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

renderCSV :: Pandoc -> IO Pandoc
renderCSV = bottomUpM (concatMapM tablifyCsvLinks)

modSyntree = case syntree of
  (Left _) -> error "Syntax error"
  (Right p) -> renderCSV p


-- a = writeMarkdown def

b :: PandocMonad m => IO (m T.Text)
b = liftM (writeMarkdown def ) modSyntree

main :: IO ()
main = do
  result <- runIO $ do
    doc <- readMarkdown ropt (T.pack inp)
    writeMarkdown def doc
  rst <- handleError result
  TIO.putStrLn rst
  exitSuccess
