import           Development.Shake
import           Development.Shake.FilePath

lhs2TeX  = "lhs2TeX"
pdflatex = "pdflatex"
rubber   = "rubber"
bibtex   = "bibtex"

main = shake shakeOptions $ do

    want ["LGM.pdf"]

    "*.tex" *> \output -> do
        let input = replaceExtension output "lhs"
        need [input]
        system' lhs2TeX $ ["--verb", "-o", output] ++ [input]

    "*.pdf" *> \output -> do
        let input = replaceExtension output "tex"
        need [input]
        system' pdflatex $ ["--enable-write18", input]
--        system' pdflatex $ ["--enable-write18", input]
