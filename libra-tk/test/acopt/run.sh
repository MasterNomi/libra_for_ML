#!/bin/bash
echo "Running acopt tests..."

testnum=0


# TEST 1
testnum=$(($testnum+1))
rm -f msweb-out2.ac
../../bin/libra acopt -m msweb-ac.ac -o msweb-out2.ac -ma msweb.xmod -ev msweb.ev -maxiter 1 -log msweb-out2.log
../../bin/libra mscore -m msweb-out2.ac -i ../../doc/examples/msweb.test -v -log msweb-out2.txt
sig2=`paste -d " " msweb-out2.txt msweb-test2.txt | awk -f sig.awk`
./diffr msweb-test2.ac msweb-out2.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 2
testnum=$(($testnum+1))
rm -f msweb-out4.ac
../../bin/libra mconvert -m msweb.xmod -o msweb.mn -ev msweb.ev 
../../bin/libra acopt -m msweb-ac.ac -o msweb-out4.ac -ev msweb.ev -ma msweb.mn -ev msweb.ev -maxiter 1 -log msweb-out4.log


../../bin/libra mscore -m msweb-out4.ac -i ../../doc/examples/msweb.test -v -log msweb-out4.txt

sig4=`paste -d " " msweb-out4.txt msweb-test4.txt | awk -f sig.awk`
#./diffr msweb-test4.ac msweb-out4.ac > /dev/null

if [ $sig4 -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#testnum=$(($testnum+1))
#rm -f msweb-out1.ac
#../../bin/libra acopt -c msweb-ac.ac -o msweb-out1.ac -b msweb-cl.xmod -ev msweb.ev -gibbs -gspeed medium -seed 1 -log msweb-out1.log
#./diffr msweb-test1.ac msweb-out1.ac > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 5
#testnum=$(($testnum+1))
#rm -f baudio-out1.ac
#../../bin/libra acopt -c baudio-e100k.ac -o baudio-out1.ac -m baudio.xmod -ev baudio1.tev -gibbs -gspeed fast -seed 1 -log baudio-out1.log
#./diffr baudio-test1.ac baudio-out1.ac > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 6
#testnum=$(($testnum+1))
#rm -f baudio-out2.ac
#../../bin/libra acopt -c baudio-e100k.ac -o baudio-out2.ac -m baudio.xmod -ev baudio1.tev -maxiter 1 -log baudio-out2.log
#./diffr baudio-test2.ac baudio-out2.ac > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


rm *.ac-r
