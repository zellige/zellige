# zellige

Library for constructing MVT (Mapnik Vector Tiles)

![Build Status](https://circleci.com/gh/sitewisely/zellige/tree/master.svg?style=svg&circle-token=d7f7d0ec0cba1afe0b7f9db162276e976e0e627c)

Example of use:
- `zellige --layer-input ./test/integration/19781.json --layer-output foo.mvt --layer-name foo --layer-zoom 15 --layer-x 28999 --layer-y 19781 --layer-buffer 128 --layer-extent 2048 --layer-quantize-pixels 2`

Tests:
- `ghci --test`
- `hspec [name of spec]`