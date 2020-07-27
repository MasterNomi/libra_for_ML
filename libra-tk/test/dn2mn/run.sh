#!/bin/bash
echo "Running dn2mn tests..."

testnum=0

for base in a3 a1 alarm; do
testnum=$(($testnum+1))
../../bin/libra dn2mn -m $base.dn -i $base.data -o $base-l-out.mn -rev -linear -marg
../../bin/libra dn2mn -m $base.dn -i $base.data -order $base.order -o $base-o-out.mn -norev -single -marg
../../bin/libra dn2mn -m $base.dn -i $base.data -maxlen 3 -all -o $base-all-out.mn -marg
../../bin/libra mscore -m $base-o-out.mn -i $base.data -pll -log $base-o-out.log
../../bin/libra mscore -m $base-l-out.mn -i $base.data -pll -log $base-l-out.log
../../bin/libra mscore -m $base-all-out.mn -i $base.data -pll -log $base-all-out.log

diff -b -q $base-o-out.log $base-o-test.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum -linear: FAILED"; else echo "Test $testnum -linear: Success"; fi
diff -b -q $base-l-out.log $base-l-test.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum -order: FAILED"; else echo "Test $testnum -order: Success"; fi
diff -b -q $base-all-out.log $base-all-test.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum -all: FAILED"; else echo "Test $testnum -all: Success"; fi

done
