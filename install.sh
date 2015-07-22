#!/bin/sh

LIB=`pwd`/lib

PREFIX=/usr/local
SASH=sash

usage()
{
    echo 'install.sh [OPTIONS] [target]'
    echo ' OPTIONS:'
    echo '  --prefix $prefix    the prefix of where to install Pegasus'
    echo '  --sash   $sash      specifies which Sagittarius is used'
    echo '  --config-dir $dir   specifies where to put local repository'
    echo ' Targets:'
    echo '  install   installs Pegasus'
    echo '  remove    removes Pegasus'
    echo '  help      shows this message'
    exit 255;
}

while [ $# -gt 1 ] ; do
    arg="$1"
    shift
    case $arg in
	--prefix) 	PREFIX="$1"       ; shift ;;
	--sash)   	SASH="$1"         ; shift ;;
	--config-dir)   CONFIG_DIR="$1"   ; shift ;;
    esac
done

if [ x"$1" = x ]; then
    TARGET="install"
else
    case "$1" in
	install) TARGET="install" ;;
	remove)  TARGET="remove"  ;;
	help)    usage; exit 0;;
	*)       echo "unknown target: $1"; usage; exit 255;;
    esac
fi

# The library will be installed into the Sagittarius sitelib
$SASH -L$LIB ./bin/install.scm -l lib -w . -t $TARGET

# the script
PEGASUS="$PREFIX/bin/pegasus.scm"

# create the bin file
echo "-- Creating pegasus file"
echo '#!/bin/sh' > pegasus
if [ x"$CONFIG_DIR" = x ]; then
    echo "$SASH $PEGASUS \"\$@\"" >> pegasus
else
    echo "env PEGASUS_CONFIG_DIR=\"$CONFIG_DIR\" $SASH $PEGASUS \"\$@\"" >> pegasus
fi
chmod +x pegasus
# install pegasus
echo "-- Installing: $PREFIX/bin/pegasus"
cp ./pegasus     $PREFIX/bin/
echo "-- Installing: $PREFIX/bin/pegasus.scm"
cp ./bin/pegasus.scm $PREFIX/bin/
# clean up
rm ./pegasus

# initialise repository
$PREFIX/bin/pegasus init
# init now clone and update will pull so not needed
# $PREFIX/bin/pegasus update

echo "Done!"

