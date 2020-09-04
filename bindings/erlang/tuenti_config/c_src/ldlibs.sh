#!/bin/sh

# erl_interface doesn't exist anymore since OTP 23, but it's needed in OTP <= 20

ERL_INTERFACE_LIB_DIR=$(erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")


LDLIBS=""
if [ -f "$ERL_INTERFACE_LIB_DIR/liberl_interface.a" ]; then
	LDLIBS="$LDLIBS -lerl_interface"
fi

if [ -f "$ERL_INTERFACE_LIB_DIR/libei.a" ]; then
	LDLIBS="$LDLIBS -lei"
fi

echo "$LDLIBS"
