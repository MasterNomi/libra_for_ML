# When called from the Makefile, TARGET_DIRECTORY defaults to /usr/local/bin.
# This can be changed by running: "make install INSTALL_PATH=/some/other/path"
if [ -z "$TARGET_DIRECTORY" ]; then
if [ -z "$INSTALL_PATH" ]; then
    echo "You must set INSTALL_PATH to a path before running install.sh."
    exit 1
fi
TARGET_DIRECTORY=$INSTALL_PATH
fi

# The Makefile saves the user home directory to .user when "make all"
# is run.  That way, the correct home directory is used, even if 
# "make install" is called using sudo.
if [ ! -f .user ]; then
    echo 'Please run "make all" first.'
    exit 1
fi
HOME=`cat .user`
libra_home=`pwd`;
echo "Installing Libra ...."

if [ "`uname`" = "Darwin" ]; then

echo "Setting LIBRA_HOME variable"
if [ "`grep 'LIBRA_HOME' $HOME/.profile`" = "" ]; then \
echo "export LIBRA_HOME=$libra_home" >> $HOME/.profile; 
export LIBRA_HOME=$libra_home
else
echo "LIBRA_HOME exists in $HOME/.profile; please make sure that it is set correctly" 
fi

else

if [ "`grep 'LIBRA_HOME' $HOME/.bashrc`" = "" ]; then \
echo "export LIBRA_HOME=$libra_home" >> $HOME/.bashrc; 
export LIBRA_HOME=$libra_home
else
echo "LIBRA_HOME exists in $HOME/.bashrc; please make sure that it is set correctly" 
fi

fi

echo "Copying libra run-file into $TARGET_DIRECTORY"
chmod +x scripts/libra
mkdir -p $TARGET_DIRECTORY
cp scripts/libra $TARGET_DIRECTORY

echo ""
echo "To use Libra, open another terminal or copy and paste the following command:"
echo "export LIBRA_HOME=$libra_home" 
