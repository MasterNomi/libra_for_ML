#!/bin/bash

../bin/_libra_rand -seed 1234 -n 100 > out-rand.txt
diff -n -b out-rand.txt test-rand.txt
if [ $? -ne 0 ]; then echo "The random number generator generates different sequence using the same seed, so the test of algorithms that depend on random numbers may fail"; fi

cd ..;
librapath="`pwd`/bin"
echo $librapath
cd test;
oldpath=$PATH
export PATH=$PATH:$librapath
for i in manual util acve cl acbn acmn mtlearn acopt inference inference/mpe mn dn2mn idspn; do
  (cd $i; ./run.sh)
done

echo "BEGINNING FRAGILE TESTS:"
echo "The following tests may fail due to differences in random number generation"
echo "or floating point precision.  Failure of these tests does not necessarily"
echo "imply a bug, just a difference of some kind."
for i in util acve cl acbn acmn idspn mtlearn acopt inference inference/mpe mn dn2mn manual; do
  (cd $i; if [ -e ./run-fragile.sh ]; then ./run-fragile.sh; fi)
done

export PATH=$oldpath
