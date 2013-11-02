#!/bin/sh

LIB=`pwd`/lib

PREFIX=/usr/local
SASH=sash

while [ $# -gt 1 ] ; do
    arg="$1"
    shift
    case $arg in
	--prefix) PREFIX="$1" ; shift ;;
	--sash)   SASH="$1"   ; shift ;;
    esac
done

if [ x"$1" = x ]; then
    TARGET="install"
else
    case "$1" in
	install) TARGET="install" ;;
	remove)  TARGET="remove"  ;;
	*)
	    echo "unknown target: $1" ; exit -1 ;;
    esac
fi

# The library will be installed into the Sagittarius sitelib
$SASH -L$LIB ./bin/install.scm -l lib -w . -t $TARGET

# create the bin file
echo "-- Creating pegasus file"
echo '#!/bin/sh' > pegasus
echo "$SASH $PREFIX/bin/pegasus.scm \"\$@\"" >> pegasus
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
$PREFIX/bin/pegasus update

echo "Done!"

