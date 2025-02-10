## Tridi
Haskell library with 3d opengl building blocks and fps observer boilerplate

### Dependencies
`tridi` depends on `orthori` package which is pinned in nixpkgs and automatically sourced from github when building with nix

### Development

Recommended usage with `nix-build` and running executable for drastic perfomance improvement.

For faster development, use interpreted mode.
Enter nix shell with hls using
```
nix-shell --arg withHLS true
```
Some problematic behaviour with FFI when running in interpreted mode. This is avoided by starting ghci with this flag:
```
ghci -fno-ghci-sandbox -isrc src/Main.hs
```


### Utility

Boilerplate code for getting started with 3d development in haskell and opengl

### Usage

`braketGLFWWin` function wraps all low-level details
Main loop collects keyboard input and manages internal state (Observer), i.e. camera position/orientation.
Observer is controled by wasd (movement), hjkl(rotation) and ui(move up/down).
This allow simple observation of the 3d space for nicer development with opengl.

### Examples

Check Main.hs

