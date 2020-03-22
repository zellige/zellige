# zellige

Library for constructing MVT (Mapnik Vector Tiles)

![Build Status](https://github.com/zellige/zellige/workflows/CI/badge.svg)

Example of use:
- `zellige --layer-input ./test/integration/19781.json --layer-output foo.mvt --layer-name foo --layer-zoom 15 --layer-x 28999 --layer-y 19781 --layer-buffer 128 --layer-extent 2048 --layer-quantize-pixels 2`

Tests:
- `ghci --test`
- `hspec [name of spec]`

### Other Projects
- [Clipper](http://www.angusj.com/delphi/clipper.php)