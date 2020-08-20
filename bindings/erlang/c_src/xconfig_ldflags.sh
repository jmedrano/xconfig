#!/bin/sh

# Try to use the pkg-config configuration, provide some default options if pkg-config is not installed
XCONFIG_LIBS=`pkg-config --libs xconfig 2>/dev/null || echo "-L/usr/lib/ -lxconfig"`
# Only add references to libraries that are really used
XCONFIG_LIBS="$XCONFIG_LIBS -Wl,--as-needed -lboost_thread -lpthread"

# Get the compiler library search path
line=`c++ -xc++ -E -v - </dev/null 2>&1 | grep "^LIBRARY_PATH="`
if [ $? -eq 0 ]
then
    # Load the LIBRARY_PATH variable
    eval "$line"

    # If the libboost_system library is found, is must be included (it depends on the Boost version)
    FOUND=$(IFS=":"; find $LIBRARY_PATH -name "libboost_system*.so" -print -quit)

    if [ -n "$FOUND" ]
    then
        XCONFIG_LIBS="$XCONFIG_LIBS -lboost_system"
    fi
fi

echo "$XCONFIG_LIBS -lstdc++"
