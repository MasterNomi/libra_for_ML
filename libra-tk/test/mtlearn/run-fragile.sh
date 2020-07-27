#!/bin/bash
echo "Running mtlearn tests..."
echo "mtlearn is sensitive to the sequence of random numbers."
testnum=0

# TEST 1
testnum=$(($testnum+1))
# TEST 1-a
../../bin/libra mtlearn -i train1.data -s train1.schema -o out1.spn -k 5 -f -seed 1234 > out1.log
diff -b -q model1.spn/spac.m out1.spn/spac.m > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-a: FAILED"; else echo "Test $testnum-a: Success"; fi

# TEST 1-b
cat out1.log | grep Average > out1.llg 
diff -b -q model1.llg out1.llg > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-b: FAILED"; else echo "Test $testnum-b: Success"; fi


# TEST 2
testnum=$(($testnum+1))
../../bin/libra spquery -q test1.data -m out1.spn -log out1.inf > /dev/null
diff -b -q model1.inf out1.inf > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
