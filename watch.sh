#!/bin/bash
./build.sh
fswatch -o src/ html/ | xargs -n1 ./build.sh
