# When called from the Makefile, the arguments are $bindir and $docdir
if [ $# -ne 2 ]; then
    echo "You must specify binary and documentation paths on the command line."
    exit 1
fi

BIN_DIRECTORY=$1
DOC_DIRECTORY=$2

echo "Installing Libra binaries in $BIN_DIRECTORY."
mkdir -p $BIN_DIRECTORY || exit -1
chmod +x bin/_libra_*
chmod +x bin/libra
cp bin/_libra_* $BIN_DIRECTORY
cp bin/libra $BIN_DIRECTORY

echo "Installing Libra documentation in $DOC_DIRECTORY."
mkdir -p $DOC_DIRECTORY || exit -1
cp doc/manual.pdf doc/devguide.pdf $DOC_DIRECTORY
mkdir -p $DOC_DIRECTORY/examples || exit -1
cp doc/examples/msweb-ac.ac doc/examples/msweb-cl.bn doc/examples/msweb.bn doc/examples/msweb.q doc/examples/msweb.xmod doc/examples/msweb-ac.xmod doc/examples/msweb-mn.ac doc/examples/msweb.data doc/examples/msweb.schema doc/examples/msweb-bn.bn doc/examples/msweb.ev doc/examples/msweb-cl.ac doc/examples/msweb.ac doc/examples/msweb.mn doc/examples/msweb.test $DOC_DIRECTORY/examples
cp -r doc/examples/msweb-mt.spn doc/examples/msweb.spn $DOC_DIRECTORY/examples
