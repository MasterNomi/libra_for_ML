#!/bin/bash
echo "Running inference tests..."

testnum=0

echo "Testing BP..."

# Testing BP on simple chain network

# TEST 1
testnum=$(($testnum+1))
../../bin/libra bp -m test1.bif -log out1-bn.log
./diffr -b -q out1-bn.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m test1.mn -log out1-mn.log
./diffr -b -q out1-mn.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m test1-f.mn -log out1-mnf.log
./diffr -b -q out1-mnf.log test1.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 2
testnum=$(($testnum+1))
../../bin/libra bp -m test1.bif -ev test1.ev -log out2-bn.log
./diffr -b -q out2-bn.log test2.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m test1.mn -ev test1.ev -log out2-mn.log
./diffr -b -q out2-mn.log test2b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m test1-f.mn -ev test1.ev -log out2-mnf.log
./diffr -b -q out2-mnf.log test2b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 3
testnum=$(($testnum+1))
../../bin/libra bp -m test1.bif -ev test1.ev -q test1.q -log out3-bn.log
./diffr -b -q out3-bn.log test3.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m test1.mn -ev test1.ev -q test1.q -log out3-mn.log
./diffr -b -q out3-mn.log test3b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m test1-f.mn -ev test1.ev -q test1.q -log out3-mnf.log
./diffr -b -q out3-mnf.log test3b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 4
testnum=$(($testnum+1))
../../bin/libra bp -m test1-ev1.mn -log out4.log
./diffr -b -q out4.log test4.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 5
testnum=$(($testnum+1))
../../bin/libra bp -m test1-ev1.mn -ev test1.ev1 -log out5.log
./diffr -b -q out5.log test5.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 6
testnum=$(($testnum+1))
../../bin/libra bp -m test1-ev1.mn -ev test1.ev1 -q test1.q1 -log out6.log
./diffr -b -q out6.log test6.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# Testing BP on Alarm network

# TEST 7
testnum=$(($testnum+1))
../../bin/libra bp -m alarm.bif -log out7-bn.log
./diffr -b -q out7-bn.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m alarm.mn -log out7-mn.log
./diffr -b -q out7-mn.log test7b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m alarm-f.mn -log out7-mnf.log
./diffr -b -q out7-mnf.log test7b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 8
testnum=$(($testnum+1))
../../bin/libra bp -m alarm.bif -ev alarm.ev -log out8-bn.log
./diffr -b -q out8-bn.log test8.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m alarm.mn -ev alarm.ev -log out8-mn.log
./diffr -b -q out8-mn.log test8b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m alarm-f.mn -ev alarm.ev -log out8-mnf.log
./diffr -b -q out8-mnf.log test8b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 9
testnum=$(($testnum+1))
../../bin/libra bp -m alarm.bif -ev alarm.ev -q alarm.q -log out9-bn.log
./diffr -b -q out9-bn.log test9.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m alarm.mn -ev alarm.ev -q alarm.q -log out9-mn.log
./diffr -b -q out9-mn.log test9b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m alarm-f.mn -ev alarm.ev -q alarm.q -log out9-mnf.log
./diffr -b -q out9-mnf.log test9b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 10
testnum=$(($testnum+1))
../../bin/libra bp -m alarm-ev1.mn -log out10.log
./diffr -b -q out10.log test10.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 11
testnum=$(($testnum+1))
../../bin/libra bp -m alarm-ev1.mn -ev alarm.ev1 -log out11.log
./diffr -b -q out11.log test11.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 12
testnum=$(($testnum+1))
../../bin/libra bp -m alarm-ev1.mn -ev alarm.ev1 -q alarm.q1 -log out12.log
./diffr -b -q out12.log test12.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# Testing BP on learned MSWeb network with decision tree CPDs

# TEST 13
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.xmod -log out13-bn.log
./diffr -b -q out13-bn.log test13.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m msweb.mn -log out13-mn.log
./diffr -b -q out13-mn.log test13b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m msweb-f.mn -log out13-mnf.log
./diffr -b -q out13-mnf.log test13c.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 14
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.xmod -ev msweb.ev -log out14-bn.log
./diffr -b -q out14-bn.log test14.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m msweb.mn -ev msweb.ev -log out14-mn.log
./diffr -b -q out14-mn.log test14b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m msweb-f.mn -ev msweb.ev -log out14-mnf.log
./diffr -b -q out14-mnf.log test14c.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 15
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.xmod -ev msweb.ev -q msweb.q -log out15-bn.log
./diffr -b -q out15-bn.log test15.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum BN: FAILED"; else echo "Test $testnum BN: Success"; fi

../../bin/libra bp -m msweb.mn -ev msweb.ev -q msweb.q -log out15-mn.log
./diffr -b -q out15-mn.log test15b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN: FAILED"; else echo "Test $testnum MN: Success"; fi

../../bin/libra bp -m msweb-f.mn -ev msweb.ev -q msweb.q -log out15-mnf.log
./diffr -b -q out15-mnf.log test15b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum MN-F: FAILED"; else echo "Test $testnum MN-F: Success"; fi

# TEST 16
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-ev1.mn -log out16.log
./diffr -b -q out16.log test16.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 17
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-ev1.mn -ev msweb.ev1 -log out17.log
./diffr -b -q out17.log test17.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 18
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-ev1.mn -ev msweb.ev1 -q msweb.q -log out18.log
./diffr -b -q out18.log test18.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

echo "Testing spquery..."
# TEST 19
testnum=$(($testnum+1))
../../bin/libra spquery -q ./test19.data -ev test19.ev -m model19.spn -log out19.inf > /dev/null
res=`cat out19.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 20
testnum=$(($testnum+1))
../../bin/libra spquery -q ./test20.data -ev test20.ev -m model19.spn -log out20.inf > /dev/null
res=`cat out20.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi



# TEST 21
testnum=$(($testnum+1))
../../bin/libra spquery -q ./test21.data -ev test21.ev -m model19.spn -log out21.inf > /dev/null
res=`cat out21.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# Test 22
echo "Testing acquery"
testnum=$(($testnum+1))
../../bin/libra acquery -q ./test19.data -ev test19.ev -m model22.ac -log out22.inf > /dev/null
res=`cat out22.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 23
testnum=$(($testnum+1))
../../bin/libra acquery -q ./test20.data -ev test20.ev -m model22.ac -log out23.inf > /dev/null
res=`cat out23.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 24
testnum=$(($testnum+1))
../../bin/libra acquery -q ./test21.data -ev test21.ev -m model22.ac -log out24.inf > /dev/null
res=`cat out24.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 25  (Same as TEST 24, but with -sameev)
testnum=$(($testnum+1))
../../bin/libra acquery -q ./test21.data -ev test21.ev -m model22.ac -log out25.inf -sameev > /dev/null
res=`cat out25.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 26  (Same as TEST 24, but with -sameev -preprune)
testnum=$(($testnum+1))
../../bin/libra acquery -q ./test21.data -ev test21.ev -m model22.ac -log out26.inf -preprune -sameev > /dev/null
res=`cat out25.inf | awk 'BEGIN{s=0.0} {s = s + exp($1);} END{ if (s >= 1.99999 && s <= 2.00001 ){ print 1 } else {print 0}}'`
if [ $res -ne 1 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

rm -f *.log-r
# TODO: Add tests for mf, gibbs, etc.
