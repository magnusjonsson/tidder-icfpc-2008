sh clean.sh &&
ghc -prof -auto-all -O3 -funbox-strict-fields --make AStar.hs AStarTest.hs &&
./AStarTest +RTS -p &&
cat ./AStarTest.prof
