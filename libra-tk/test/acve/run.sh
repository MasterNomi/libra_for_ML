#!/bin/bash
echo "Running acve tests..."

testnum=0

# TEST 1
testnum=$(($testnum+1))
rm -rf out1.ac out1.log
../../bin/libra acve -m test1.bif -o out1.ac -log out1.log
diff -b -q out1.ac test1.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 2
testnum=$(($testnum+1))
rm -rf out2.ac out2.log
../../bin/libra acve -m alarm.bif -o out2.ac -log out2.log -v
diff -b -q out2.ac test2.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 3
testnum=$(($testnum+1))
rm -rf out3.ac out3.log
../../bin/libra acve -m cold-learned.xmod -o out3.ac -log out3.log -v
diff -b -q out3.ac test3.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 3
testnum=$(($testnum+1))
rm -rf out4.ac out4.log
../../bin/libra acve -m test4.mn -o out4.ac -log out4.log -v
diff -b -q out4.ac test4.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
