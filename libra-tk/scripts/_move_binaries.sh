#!/bin/sh
# Script to clean-up and move files around after the oasis build
# process finishes.

# Move binary files, e.g., "foo.native" --> "bin/_libra_foo"
for i in *.native; do
  if [ ! -e "$i" ]; then continue; fi
  mkdir -p bin || exit -1
  cp $i bin/_libra_`basename $i .native` && rm $i
done

# Copy libra script
cp scripts/_libra bin/libra
chmod +x bin/libra
