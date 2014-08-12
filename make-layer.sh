#! /bin/bash

# Create a pseudo layer, i.e. just an empty tarball.
# TODO Use the tar package: http://hackage.haskell.org/package/tar
mkdir etc
tar czf etc.tar.gz etc
rmdir etc
