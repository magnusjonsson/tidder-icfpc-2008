sh clean.sh &&
ghc -prof -auto-all -O2 -optc-O3 --make AStar.hs AStarTest.hs &&
./AStarTest +RTS -p &&
cat ./AStarTest.prof
