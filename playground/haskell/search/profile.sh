sh clean.sh &&
#ghc -prof -auto-all -O3 -funbox-strict-fields -package heap --make AStar.hs AStarTest.hs &&
ghc -prof -auto-all -O2 -optc-O3 -ddump-simpl --make AStar.hs AStarTest.hs > core.txt &&
./AStarTest +RTS -p &&
cat ./AStarTest.prof
