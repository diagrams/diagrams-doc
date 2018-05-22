stack build
stack exec diagrams-doc -- +RTS -N4 -RTS buildh
cp -R web/_site/* ../github.io/
cd ../github.io/ && git commit -a && git push
