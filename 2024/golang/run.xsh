#!xonsh
import sys

go build ./src/day@(sys.argv[1]).go ./src/util.go
./day@(sys.argv[1]) @(sys.argv[2])
