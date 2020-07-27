#!/bin/bash
echo "Running MN tests..."

testnum=0

#
# Convert models to Markov networks
#

# TEST 1
testnum=$(($testnum+1))
../../bin/libra mconvert -m msweb-cl.bn -o out1.mn
./diffr out1.mn msweb-cl.mn > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 2
testnum=$(($testnum+1))
../../bin/libra mconvert -m msweb.xmod -o out2.mn
./diffr out2.mn msweb.mn > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# Score them (using PLL)
#

# TEST 3
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb-cl.mn -pll -i ../../doc/examples/msweb.test -v -log out3.log
./diffr out3.log test3.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 4
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb.mn -pll -i ../../doc/examples/msweb.test -v -log out4.log
./diffr out4.log test4.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


#
# MF
#

# TEST 5
testnum=$(($testnum+1))
../../bin/libra mf -m msweb-cl.mn -log out5.log -thresh 0.001 -roundrobin
./diffr out5.log test5.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 6
testnum=$(($testnum+1))
../../bin/libra mf -m msweb-cl.mn -q msweb.q -log out6.log -thresh 0.001 -roundrobin
./diffr out6.log test6.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 7
testnum=$(($testnum+1))
../../bin/libra mf -m msweb.mn -q msweb.q -ev msweb.ev1 -log out7.log -thresh 0.001 -roundrobin
./diffr out7.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 8
testnum=$(($testnum+1))
../../bin/libra mf -m msweb.mn -q msweb.q -ev msweb.ev1 -log out8.log -sameev -thresh 0.001 -roundrobin
grep -v "time" out8.log > out8a.log 
./diffr out8a.log test8.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# BP
#

# TEST 9
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-cl.mn -log out9.log
./diffr out9.log test9.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 10
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-cl.mn -q msweb.q -log out10.log 
./diffr out10.log test10.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 11 
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.mn -q msweb.q -ev msweb.ev1 -log out11.log 
./diffr out11.log test11.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 12 
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.mn -q msweb.q -ev msweb.ev1 -log out12.log -sameev
./diffr out12.log test12.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# Gibbs
#

# TEST 13
testnum=$(($testnum+1))
time ../../bin/libra gibbs -seed 1234 -m msweb.mn -mo out13.marg
./diffr out13.marg test13.marg > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 14 
testnum=$(($testnum+1))
time ../../bin/libra gibbs -seed 1234 -m msweb.mn -q msweb.q -ev msweb.ev1 -log out14.log
./diffr out14.log test14.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 15
testnum=$(($testnum+1))
time ../../bin/libra gibbs -seed 1234 -m msweb.mn -q msweb.q -ev msweb.ev1 -log out15.log -speed medium -sameev
./diffr out15.log test15.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

rm *.log-r
rm *.marg-r
rm *.mn-r
