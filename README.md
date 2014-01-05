# phash: Haskell bindings to pHash, the open source perceptual hash library

PHash is a library for generating perceptual hashes of media files. These
bindings currently only support images. You can compare these hashes to detect
visually similar images.

## Installation Notes
Note that this library does not come bundled with the source code for pHash.
You must install that yourself. Your package manager may have it available as
`libphash`. If that is not available, you can install it from source from
http://phash.org.

## Usage

```haskell
import Data.PHash

main = do
  Just h1 = imageHash "somefile.jpg"
  print h1
  print =<< imagesSimilar "somefile.jpg" "similarfile.jpg" reasonableThreshold
  where reasonableThreshold = 15
```

# Credit
All credit goes to the original pHash authors. For more information about pHash
visit http://phash.org
