#!/bin/sh

if [ x`which ghc` = x -a x`which runhugs` = x ]; then
    echo "Neither ghc nor runhugs found on search path."
    exit 1
fi

mkdir -p bin

if [ x`which ghc` = x -o ! x$USE_HUGS = x ]; then
    # create scripts to run with Hugs
    cat >bin/robinri <<'EOF'
#!/bin/sh
THIS=`realpath $0`
DIR=`dirname $THIS`/../src
cp $DIR/mains/robinri/Main.lhs $DIR/Main.lhs
runhugs $DIR/Main.lhs $*
EOF
    chmod 755 bin/robinri
    cat >bin/whitecap <<'EOF'
#!/bin/sh
THIS=`realpath $0`
DIR=`dirname $THIS`/../src
cp $DIR/mains/whitecap/Main.lhs $DIR/Main.lhs
runhugs $DIR/Main.lhs $*
EOF
    chmod 755 bin/whitecap
else
    rm -f src/Main.lhs
    ghc -isrc --make src/mains/robinri/Main.lhs -o bin/robinri || exit 1
    ghc -isrc --make src/mains/whitecap/Main.lhs -o bin/whitecap || exit 1
fi

./build-packages.sh
