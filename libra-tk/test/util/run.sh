#!/bin/bash
echo "Running util tests..."

testnum=0

# TEST 1
testnum=$(($testnum+1))
../../bin/libra bnsample -m test1.bif -n 10000 -seed 1 -o out1.data 
diff -b -q out1.data test1.data > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 2
testnum=$(($testnum+1))
../../bin/libra mscore -m test1.bif -i test1.data -v -log out2.log
diff -b -q out2.log test2.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 3: Sampling from a slightly more complex BIF file
testnum=$(($testnum+1))
../../bin/libra bnsample -m cold.bif -n 10000 -seed 1 > out3.data 
diff -b -q out3.data test3.data > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 4: Sampling from an XMOD file that's equivalent to the BIF file
testnum=$(($testnum+1))
../../bin/libra bnsample -m cold.xmod -n 10000 -seed 1 -o out4.data 
diff -b -q out4.data test3.data > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 5: Sampling from a more complex XMOD file, with tree CPDs
testnum=$(($testnum+1))
../../bin/libra bnsample -m msweb.xmod -n 1000 -seed 1 -o out5.data 
diff -b -q out5.data test5.data > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


# TEST 6
echo "Testing spn2ac"
testnum=$(($testnum+1))
../../bin/libra spn2ac -m model1.spn -o out1.ac  > /dev/null
diff -b -q model1.ac out1.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 7
testnum=$(($testnum+1))
../../bin/libra spn2ac -m model2.spn -o out2.ac > /dev/null
diff -b -q model2.ac out2.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 8
testnum=$(($testnum+1))
../../bin/libra spn2ac -m model3.spn -o out3.ac > /dev/null
diff -b -q model3.ac out3.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
