#!/bin/sh
# When called from the Makefile, the arguments are $bindir and $docdir
if [ $# -ne 2 ]; then
    echo "You must specify binary and documentation paths on the command line."
    exit 1
fi

BIN_DIRECTORY=$1
DOC_DIRECTORY=$2

# Remove all installed files.
echo "Remove Libra binaries from $BIN_DIRECTORY."
[ -f $BIN_DIRECTORY/libra ] && rm $BIN_DIRECTORY/libra 
for f in acmn bp idspn gibbs acbn condacmn condacbn acopt acquery condAcquery bnlearn acve bnsample cl dn2mn dnboost dnlearn fstats icm maxprod mconvert mf mnlearnw mnsetw mscore spn2ac mtlearn spnlearn crflearn spquery rand; do
  [ -f $BIN_DIRECTORY/_libra_$f ] && rm $BIN_DIRECTORY/_libra_$f
done

echo "Remove Libra documentation from $DOC_DIRECTORY."
[ -f $DOC_DIRECTORY/devguide.pdf ] && rm $DOC_DIRECTORY/manual.pdf 
[ -f $DOC_DIRECTORY/devguide.pdf ] && rm $DOC_DIRECTORY/devguide.pdf
[ -d $DOC_DIRECTORY/examples ] && rm -rf $DOC_DIRECTORY/examples/msweb*
[ -d $DOC_DIRECTORY/examples ] && rmdir $DOC_DIRECTORY/examples
[ -d $DOC_DIRECTORY ] && rmdir $DOC_DIRECTORY

exit 0
