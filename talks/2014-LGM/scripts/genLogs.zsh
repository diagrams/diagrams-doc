cd ~/src/diagrams/
for repo in core lib contrib cairo svg postscript builder doc haddock palette povray; do
  cd $repo
  git log --format="%aN%n%at" > ~/src/diagrams/doc/talks/2014-LGM/scripts/logs/$repo.txt
  cd ..
done