#!/bin/bash
echo "Running acbn tests..."

testnum=0

# TEST 1
testnum=$(($testnum+1))
rm -f cold-out1.ac cold-out1.xmod cold-out1.log
../../bin/libra acbn -i cold.data -o cold-out1.ac -mo cold-out1.xmod -debug -v > cold-out1.log 
diff -b -q cold-out1.ac cold-test1.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-a: FAILED"; else echo "Test $testnum-a: Success"; fi
diff -b -q cold-out1.xmod cold-test1.xmod > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-b: FAILED"; else echo "Test $testnum-b: Success"; fi

# TEST 2
testnum=$(($testnum+1))
rm -f alarm-out2.ac alarm-out2.xmod alarm-out2.log
../../bin/libra acbn -i alarm.data -o alarm-out2.ac -mo alarm-out2.xmod -debug -v > alarm-out2.log 
../../bin/libra mscore -m alarm-out2.ac -i alarm.data > alarm-out2.llg
diff -b -q alarm-out2.llg alarm-test2.llg > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
#diff -b -q alarm-out2.ac alarm-test2.ac > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum-a: FAILED"; else echo "Test $testnum-a: Success"; fi
#diff -b -q alarm-out2.xmod alarm-test2.xmod > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum-b: FAILED"; else echo "Test $testnum-b: Success"; fi
