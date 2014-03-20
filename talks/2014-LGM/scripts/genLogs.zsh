cd ~/src/diagrams/
for repo in dual-tree monoid-extras core lib contrib cairo svg postscript builder doc gtk haddock palette povray SVGFonts; do
  cd $repo
  git log --format="%aN%n%aD" > ~/src/diagrams/doc/talks/2014-LGM/scripts/logs/$repo.txt
  cd ..
done