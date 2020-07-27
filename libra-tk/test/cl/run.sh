#!/bin/bash
echo "Running cl tests..."

testnum=0

# TEST 1
testnum=$(($testnum+1))
../../bin/libra cl -v -i test1.data -o out1.bif > out1.log
diff -b -q out1.bif test1.bif > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 2
testnum=$(($testnum+1))
../../bin/libra cl -v -i test2.data -o out2.bif > out2.log
diff -b -q out2.bif test2.bif > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# NOTE: Correctness of tests 3, 4, and 5 is unverified.
# Passing these tests merely indicates consistency with previous results.

# TEST 3
testnum=$(($testnum+1))
../../bin/libra cl -i kdd-10k.data -o out3.bif > out3.log
diff -b -q out3.bif test3.bif > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 4
testnum=$(($testnum+1))
../../bin/libra cl -i kdd-10k.data -o out4.xmod > out4.log
diff -b -q out4.xmod test4.xmod > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 5
testnum=$(($testnum+1))
../../bin/libra cl -i test5.data -o out5.xmod > out5.log
diff -b -q out5.xmod test5.xmod > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
