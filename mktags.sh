#!/usr/bin/env bash
psc-package sources | xargs purs docs --format ctags src/*.purs src/**/*.purs > tags
