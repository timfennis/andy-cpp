#!/bin/fish

docker run -v (pwd):/build -w /build -it timfennis/vsce package
