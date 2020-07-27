#!/../bin/bash
echo "Running MPE tests..."

testnum=0

echo "Testing Max-Product..."

# Testing BP on simple chain network
# TEST 1
testnum=$(($testnum+1))
../../../bin/libra maxprod -m test1.bif -log out1-bn.log
./diffr out1-bn.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../../bin/libra maxprod -m test1.mn -log out1-mn.log
./diffr out1-mn.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../../bin/libra maxprod -m test1-f.mn -log out1-mnf.log
./diffr out1-mnf.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

../../../bin/libra acquery -mpe -m test1.ac -log out1-ac.log
./diffr out1-ac.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum AC: FAILED"; else echo "Test $testnum AC: Success"; fi

# TEST 2
testnum=$(($testnum+1))
../../../bin/libra maxprod -m test1.bif -ev test1.ev -log out2-bn.log
./diffr out2-bn.log test2.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../../bin/libra maxprod -m test1.mn -ev test1.ev -log out2-mn.log
./diffr out2-mn.log test2.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../../bin/libra maxprod -m test1-f.mn -ev test1.ev -log out2-mnf.log
./diffr out2-mnf.log test2.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

../../../bin/libra acquery -mpe -m test1.ac -ev test1.ev -log out2-ac.log
./diffr out2-ac.log test2.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum AC: FAILED"; else echo "Test $testnum AC: Success"; fi

# TEST 3
testnum=$(($testnum+1))
../../../bin/libra maxprod -m test1.bif -ev test1.ev -q test1.q -log out3-bn.log
./diffr out3-bn.log test3.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../../bin/libra maxprod -m test1.mn -ev test1.ev -q test1.q -log out3-mn.log
./diffr out3-mn.log test3.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../../bin/libra maxprod -m test1-f.mn -ev test1.ev -q test1.q -log out3-mnf.log
./diffr out3-mnf.log test3.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi


# TEST 4
testnum=$(($testnum+1))
../../../bin/libra maxprod -m test1-ev1.mn -log out4.log
./diffr out4.log test4.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 5
testnum=$(($testnum+1))
../../../bin/libra maxprod -m test1-ev1.mn -ev test1.ev1 -log out5.log
./diffr out5.log test5.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 6
testnum=$(($testnum+1))
../../../bin/libra maxprod -m test1-ev1.mn -ev test1.ev1 -q test1.q1 -log out6.log
./diffr out6.log test6.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# Testing MP on Alarm network

# TEST 7
testnum=$(($testnum+1))
../../../bin/libra maxprod -m alarm.bif -log out7-bn.log
./diffr out7-bn.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../../bin/libra maxprod -m alarm.mn -log out7-mn.log
./diffr out7-mn.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../../bin/libra maxprod -m alarm-f.mn -log out7-mnf.log
./diffr out7-mnf.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

../../../bin/libra acquery -mpe -m alarm.ac -log out7-ac.log
./diffr out7-ac.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum AC: FAILED"; else echo "Test $testnum AC: Success"; fi

rm *.log-r

# TODO: Testing maxproduct on learned MSWeb network with decision tree CPDs
