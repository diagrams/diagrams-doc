import           Development.Shake
import           Development.Shake.FilePath

lhs2TeX, pdflatex, latexmk :: String
lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
latexmk  = "latexmk"

main :: IO ()
main = shake shakeOptions $ do

    want ["abstract.pdf"]

    "*.tex" %> \output -> do
        let input = replaceExtension output "lhs"
        need [input]
        cmd lhs2TeX $ ["--verb", "-o", output] ++ [input]

    "*.pdf" %> \output -> do
        let input = replaceExtension output "tex"
        need [input, output -<.> "bib"]

        -- need ["Diagrams.hs"]  -- for document-specific diagrams

        () <- cmd pdflatex $ ["--enable-write18", input]
--        cmd latexmk $ ["-pdf", input]
        cmd "pdflatex" $ [input]
