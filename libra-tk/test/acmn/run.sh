#!/bin/bash
echo "Running acmn tests..."

testnum=0

# TEST 1
testnum=$(($testnum+1))
../../bin/libra acmn -i cold.data -o cold-out1.ac -mo cold-out1.mn -v > /dev/null
diff -b -q cold-out1.ac cold.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-a: FAILED"; else echo "Test $testnum-a: Success"; fi
diff -b -q cold-out1.mn cold.mn > /dev/null
../../bin/libra mscore -m cold-out1.mn -i cold.data -v  > cold-out1.llg
#tail -n 1001 cold-out1.llg | head -n 1000 > cold-out2.llg
sig=`paste -d " " cold.llg cold-out1.llg | awk -f sig.awk`
if [ $sig -ne 0 ]; then echo "Test $testnum-b: FAILED"; else echo "Test $testnum-b: Success"; fi

# TEST 2
#testnum=$(($testnum+1))
#../../bin/libra acmn -i alarm.data -o alarm-out1.ac -mo alarm-out1.mn -v > /dev/null 
#../../bin/libra acquery -q alarm.data -m alarm-out1.ac > alarm-inf-ac-out.txt
#sig1=`paste -d " " alarm-inf-ac-out.txt alarm-inf-ac-test.txt | awk -f sig.awk`
#diff -b -q alarm-inf-ac-out.txt alarm-inf-ac-test.txt > /dev/null
#if [ $sig1 -ne 0 ]; then echo "Test $testnum-a: FAILED"; else echo "Test $testnum-a: Success"; fi
#../../bin/libra mscore -i alarm.data -m alarm-out1.mn -v -log alarm-inf-mn-out.txt
#tail -n 1000 alarm-inf-mn-out.txt > alarm-inf2.out
#sig2=`paste -d " " alarm-inf-ac-out.txt alarm-inf-ac-test.txt | awk -f sig.awk`
#diff -b -q alarm-inf2.out alarm-inf2.test > /dev/null
#if [ $sig2 -ne 0 ]; then echo "Test $testnum-b: FAILED"; else echo "Test $testnum-b: Success"; fi
