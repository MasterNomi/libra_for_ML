#!/bin/bash
echo "Running manual tests..."

testnum=0

if true; then
# TEST 1
testnum=$(($testnum+1))
../../bin/libra cl -i ../../doc/examples/msweb.data -s ../../doc/examples/msweb.schema -o out1.xmod -prior 1 -v -log out1.log
./diffr out1.xmod test1.xmod > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 2
testnum=$(($testnum+1))
../../bin/libra acve -m msweb-cl.xmod -o out2.ac -v -log out2.log
./diffr out2.ac test2.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 3
testnum=$(($testnum+1))
../../bin/libra fstats -i ../../doc/examples/msweb.data -log out3a.log
./diffr out3a.log test3a.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-A: FAILED"; else echo "Test $testnum-A: Success"; fi
../../bin/libra fstats -i msweb-cl.xmod -log out3b.log
./diffr out3b.log test3b.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-B: FAILED"; else echo "Test $testnum-B: Success"; fi
../../bin/libra fstats -i msweb-cl.ac -log out3c.log
./diffr out3c.log test3c.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-C: FAILED"; else echo "Test $testnum-C: Success"; fi

# TEST 4
testnum=$(($testnum+1))
../../bin/libra acbn -i ../../doc/examples/msweb.data -s ../../doc/examples/msweb.schema -o out4.ac -mo out4.xmod -v -log out4.log
./diffr out4.ac test4.ac > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-A: FAILED"; else echo "Test $testnum-A: Success"; fi
./diffr out4.xmod test4.xmod > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum-B: FAILED"; else echo "Test $testnum-B: Success"; fi

# TEST 5
testnum=$(($testnum+1))
#../../bin/libra aclearnstruct -i ../../doc/examples/msweb.data -s ../../doc/examples/msweb.schema -o out5.ac -ob out5.xmod -v -log out5.log -pe 100 -ps 0 -maxe 10000 -shrink
#./diffr out5.ac msweb-ac2.ac > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum-A: Success"; fi
#./diffr out5.xmod msweb-ac2.xmod > /dev/null
#if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum-B: Success"; fi

# TEST 6
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb-cl.xmod -i ../../doc/examples/msweb.test -v -log out6.log
./diffr out6.log test6.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 7
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb-cl.ac -i ../../doc/examples/msweb.test -v -log out7.log
./diffr out7.log test7.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 8
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb-ac.xmod -i ../../doc/examples/msweb.test -v -log out8.log
./diffr out8.log test8.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 9
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb-ac.ac -i ../../doc/examples/msweb.test -v -log out9.log
./diffr out9.log test9.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 10
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -marg -v -log out10.log
grep -v " time:" out10.log > out10a.log
./diffr out10a.log test10.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 11
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -v -mo out11.marg -log out11.log
./diffr out11.marg test11.marg > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 12
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -v -q ./msweb.q -log out12.log
grep -v "time" out12.log | tail -1 > out12a.log
./diffr out12a.log test12.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 13
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -v -q ./msweb.q -ev ./msweb.ev -log out13.log
grep -v "time" out13.log | tail -1 > out13a.log
./diffr out13a.log test13.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 14
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -v -q ./msweb.q -ev ./msweb.ev -marg -log out14.log
grep -v "time" out14.log | tail -1 > out14a.log
./diffr out14a.log test14.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 15
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -ev msweb.ev -mpe -log out15.log
./diffr out15.log test15.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 16
testnum=$(($testnum+1))
../../bin/libra acquery -m msweb-cl.ac -q msweb.q -ev msweb.ev -mpe -log out16.log
./diffr out16.log test16.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# MF
#

# TEST 17
testnum=$(($testnum+1))
../../bin/libra mf -m msweb-cl.xmod -log out17.log -thresh 0.001
./diffr out17.log test17.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 18
testnum=$(($testnum+1))
../../bin/libra mf -m msweb-cl.xmod -q msweb.q -log out18.log -thresh 0.001
./diffr out18.log test18.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 19
testnum=$(($testnum+1))
../../bin/libra mf -m msweb.xmod -q msweb.q -ev msweb.ev1 -log out19.log -thresh 0.001
./diffr out19.log test19.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 20
testnum=$(($testnum+1))
../../bin/libra mf -m msweb.xmod -q msweb.q -ev msweb.ev1 -log out20.log -sameev -thresh 0.001
grep -v "time" out20.log > out20a.log
./diffr out20a.log test20.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# BP
#

# TEST 21
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-cl.xmod -log out21.log
./diffr out21.log test21.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 22
testnum=$(($testnum+1))
../../bin/libra bp -m msweb-cl.xmod -q msweb.q -log out22.log 
./diffr out22.log test22.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 23 
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.xmod -q msweb.q -ev msweb.ev1 -log out23.log
./diffr out23.log test23.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 24 
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.xmod -q msweb.q -ev msweb.ev1 -log out24.log -sameev
./diffr out24.log test24.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi


#
# Gibbs
#

# TEST 25
testnum=$(($testnum+1))
../../bin/libra gibbs -seed 1234 -m msweb.xmod -mo out25.marg
./diffr out25.marg test25.marg > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 26 
testnum=$(($testnum+1))
../../bin/libra gibbs -seed 1234 -m msweb.xmod -q msweb.q -ev msweb.ev1 -log out26.log
./diffr out26.log test26.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 27
testnum=$(($testnum+1))
../../bin/libra gibbs -seed 1234 -m msweb.xmod -q msweb.q -ev msweb.ev1 -log out27.log -speed medium -sameev
./diffr out27.log test27.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# Max Product
#

# TEST 28
testnum=$(($testnum+1))
../../bin/libra maxprod -m msweb-cl.xmod -ev msweb.ev -mo msweb-cl.mpe -log out28.log
./diffr out28.log test28.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 29
testnum=$(($testnum+1))
../../bin/libra maxprod -m msweb.xmod -q msweb.q -ev msweb.ev -log out29.log
./diffr out29.log test29.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

#
# Markov networks and misc.
#

# TEST 30
testnum=$(($testnum+1))
../../bin/libra mconvert -m msweb.xmod -o out30.mn -v -log out30.log
./diffr out30.mn msweb.mn > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 31
testnum=$(($testnum+1))
../../bin/libra bp -m msweb.mn -log out31.log
./diffr out31.log test31.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 32
testnum=$(($testnum+1))
../../bin/libra mf -m msweb.mn -ev msweb.ev -log out32.log
./diffr out32.log test32.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 33
testnum=$(($testnum+1))
../../bin/libra gibbs -seed 0 -m msweb.mn -q msweb.q -ev msweb.ev -log out33.log
./diffr out33.log test33.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 34
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb.mn -pll -i ../../doc/examples/msweb.test -v -log out34.log
./diffr out34.log test34.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 35
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb.xmod -i ../../doc/examples/msweb.test -pll -v -log out35.log
./diffr  out35.log test35.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 36
testnum=$(($testnum+1))
../../bin/libra mconvert -m msweb-cl.xmod -o out36.bif -v -log out36.log
./diffr out36.bif test36.bif > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi
fi

#
# Dependency networks
#
# TEST 37
testnum=$(($testnum+1))
../../bin/libra dnlearn -i ../../doc/examples/msweb.data -s ../../doc/examples/msweb.schema -o out37.xmod -prior 2 -log out37.log -mincount 1
./diffr out37.xmod test37.xmod > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 38
testnum=$(($testnum+1))
../../bin/libra mscore -m msweb-dn.xmod -i ../../doc/examples/msweb.test -depnet -log out38.log
./diffr out38.log test38.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 39

testnum=$(($testnum+1))
../../bin/libra gibbs -m msweb-dn.xmod -seed 1234 -mo out39.marg -depnet -log out39.log
./diffr out39.marg test39.marg > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

# TEST 40
testnum=$(($testnum+1))
../../bin/libra mf -m msweb-dn.xmod -ev msweb.ev -q msweb.q -depnet -log out40.log
./diffr out40.log test40.log > /dev/null
if [ $? -ne 0 ]; then echo "Test $testnum: FAILED"; else echo "Test $testnum: Success"; fi

rm *.log-r
rm *.marg-r
rm *.bif-r
rm *.ac-r
rm *.xmod-r
rm *.mn-r
