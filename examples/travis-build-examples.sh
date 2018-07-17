#!/bin/sh

cd $TRAVIS_BUILD_DIR/examples
elm-make 1-readme_example/Main.elm
elm-make 2-custom_validation/Main.elm
elm-make 3-side_effects/Main.elm
elm-make 4-transform/Main.elm
cd $TRAVIS_BUILD_DIR
