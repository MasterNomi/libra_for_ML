#!/bin/bash
cd ..;
cd ..;
librapath="`pwd`/bin"
echo $librapath
cd test;
cd idspn;
oldpath=$PATH
export PATH=$PATH:$librapath

echo "Running idspn tests..."
echo "Idspn's tests may take several minutes"
testnum=0

# TEST 1
testnum=$(($testnum+1))
# TEST 1-a
../../bin/libra idspn -i train1.data -s train1.schema -o out1.spn -f -seed 1234 > out1.log
diff -b -q model1.spn/spac.m out1.spn/spac.m > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 1-b
#cat out1.log | grep Average > out1.llg 
#sig1=`paste -d " " model1.llg out1.llg | awk -f sig.awk`
#diff -b -q model1.llg out1.llg > /dev/null
#if [ $sig1 -ne 0 ]; then echo "Test $testnum-b: FAILED"; else echo "Test $testnum-b: Success"; fi


# TEST 2
testnum=$(($testnum+1))
../../bin/libra spquery -q test1.data -m out1.spn -log out1.inf > /dev/null
sig2=`paste -d " " model1.inf out1.inf | awk -f sig.awk`
diff -b -q model1.inf out1.inf > /dev/null
if [ $sig2 -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


export PATH=$oldpath
