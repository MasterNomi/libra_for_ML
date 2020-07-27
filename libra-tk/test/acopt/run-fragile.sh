#!/bin/bash
echo "Running acopt tests with Gibbs sampling."
echo "These tests are sensitive to the sequence of random numbers."

testnum=0

# FRAGILE TEST 1
testnum=$(($testnum+1))
rm -f msweb-out1.ac
../../bin/libra acopt -m msweb-ac.ac -o msweb-out1.ac -ma msweb-cl.xmod -ev msweb.ev -gibbs -gspeed fast -seed 1 -log msweb-out1.log
../../bin/libra mscore -m msweb-out1.ac -i ../../doc/examples/msweb.test -v -log msweb-out1.txt
sig1=`paste -d " " msweb-out1.txt msweb-test1.txt | awk -f sig.awk`
if [ $sig1 -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# FRAGILE TEST 2
testnum=$(($testnum+1))
rm -f msweb-out3.ac
../../bin/libra mconvert -m msweb-cl.xmod -o msweb-cl.mn -ev msweb.ev 
../../bin/libra acopt -m msweb-ac.ac -o msweb-out3.ac -ev msweb.ev -ma msweb-cl.mn -gibbs -gspeed fast -seed 1 -log msweb-out3.log
../../bin/libra mscore -m msweb-out3.ac -i ../../doc/examples/msweb.test -v -log msweb-out3.txt

sig3=`paste -d " " msweb-out3.txt msweb-test3.txt | awk -f sig.awk`
#./diffr msweb-test3.ac msweb-out3.ac > /dev/null
if [ $sig3 -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
