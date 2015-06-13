#!/usr/bin/zsh

killall hackage-server
cabal build hackage-server
./dist/build/hackage-server/hackage-server run &

export -f jg
echo "Testing..."
sleep 3
