#!/bin/sh

# build examples in the "examples" dir
cd $TRAVIS_BUILD_DIR/examples
elm make --output=/dev/null 1-readme_example/Main.elm
elm make --output=/dev/null 2-custom_validation/Main.elm
elm make --output=/dev/null 3-side_effects/Main.elm
elm make --output=/dev/null 4-transform/Main.elm
