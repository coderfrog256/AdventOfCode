#!xonsh
import sys

result = !(go build ./src/day@(sys.argv[1]).go ./src/util.go)
print(result.out)
if result.returncode:
    print("Build failed!")
    sys.exit(result.returncode)
./day@(sys.argv[1]) @(sys.argv[2])
