#!/bin/bash
psc-package build
pulp browserify --no-check-main --to target/game.js
cp html/index.html target/index.html
psc-package sources | xargs purs docs --format ctags src/*.purs src/**/*.purs > tags
