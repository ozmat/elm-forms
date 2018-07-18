#!/bin/sh

# build examples in the "examples" dir
cd $TRAVIS_BUILD_DIR/examples
elm-make --yes 1-readme_example/Main.elm
elm-make --yes 2-custom_validation/Main.elm
elm-make --yes 3-side_effects/Main.elm
elm-make --yes 4-transform/Main.elm
elm-make --yes 5-real_world/Main.elm
rm -f index.html
