#!/bin/sh

cd $TRAVIS_BUILD_DIR/examples
elm-make 1-basic/Main.elm
elm-make 2-custom/Main.elm
elm-make 3-effects/Main.elm
elm-make 4-transform/Main.elm
cd $TRAVIS_BUILD_DIR
