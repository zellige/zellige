# zellige
Library for constructing MVT (Mapnik Vector Tiles)

Example of use:
- `zellige --layer-input ./test/integration/19781.json --layer-output foo.mvt --layer-name foo --layer-zoom 15 --layer-x 28999 --layer-y 19781 --layer-buffer 128 --layer-extent 2048`

Tests:
- `ghci --test`
- `hspec [name of spec]`