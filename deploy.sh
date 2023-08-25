stack build
stack exec diagrams-doc -- +RTS -N4 -RTS buildh
cp -R web/_site/* ../github.io/
cd ../github.io/ && git add doc/images/* && git add blog/images* && git commit -a && git push
