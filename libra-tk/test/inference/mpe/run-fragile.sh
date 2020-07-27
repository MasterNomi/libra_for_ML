#!/../bin/bash
echo "Running MPE tests..."

testnum=0

echo "These tests are sensitive to differences in initialization and floating point operations."

# TEST 8
testnum=$(($testnum+1))
../../../bin/libra maxprod -m alarm.bif -ev alarm50.ev -log out8-bn.log
./diffr out8-bn.log test8-bn.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../../bin/libra maxprod -m alarm.mn -ev alarm50.ev -log out8-mn.log
./diffr out8-mn.log test8-mn.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../../bin/libra maxprod -m alarm-f.mn -ev alarm50.ev -log out8-mnf.log
./diffr out8-mnf.log test8-mnf.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

../../../bin/libra acquery -mpe -m alarm.ac -ev alarm50.ev -log out8-ac.log
./diffr out8-ac.log test8-ac.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum AC: FAILED"; else echo "Test $testnum AC: Success"; fi


# TEST 9
testnum=$(($testnum+1))
../../../bin/libra maxprod -m alarm.bif -ev alarm50.ev -q alarm.data -log out9-bn.log
./diffr out9-bn.log test9-bn.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../../bin/libra maxprod -m alarm.mn -ev alarm50.ev -q alarm.data -log out9-mn.log
./diffr out9-mn.log test9-mn.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../../bin/libra maxprod -m alarm-f.mn -ev alarm50.ev -q alarm.data -log out9-mnf.log
./diffr out9-mnf.log test9-mnf.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

../../../bin/libra maxprod -m alarm.ac -ev alarm50.ev -q alarm.data -log out9-ac.log
./diffr out9-ac.log test9-ac.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum AC: FAILED"; else echo "Test $testnum AC: Success"; fi

rm *.log-r
