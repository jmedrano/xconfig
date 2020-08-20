#!/bin/sh
# This script tries to compile a very basic program that calls ei_init() function. That function must be used to
# initiliaze the EI C Erlang interface, but it's only available since OTP 21 and the old ERL interface is deprecated
# since OTP 22

TEST_FILE=_test_has_ei_init.c

# Get Erlang paths (same as in Makefile generated by rebar3)
ERTS_INCLUDE_DIR=$(erl -noshell -s init stop -eval "io:format(\"~ts/erts-~ts/include/\", [code:root_dir(), erlang:system_info(version)]).")
ERL_INTERFACE_INCLUDE_DIR=$(erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, include)]).")
ERL_INTERFACE_LIB_DIR=$(erl -noshell -s init stop -eval "io:format(\"~ts\", [code:lib_dir(erl_interface, lib)]).")

rm -rf "$TEST_FILE"

if [ -f "$TEST_FILE" ]; then
    echo "Can't delete $TEST_FILE" 2>&1
    echo "-invalid_option_cant_delete_file"
    exit 1
fi

cat > _test_has_ei_init.c <<EOF
#include <ei.h>

int main(void) {
  ei_init();
  return 0;
}
EOF

if [ ! -f "$TEST_FILE" ]; then
    echo "Can't create $TEST_FILE" 2>&1
    echo "-invalid_option_cant_create_file"
    exit 2
fi

gcc "$TEST_FILE" -Wimplicit-function-declaration -Werror -o /dev/null -L "$ERL_INTERFACE_LIB_DIR" -I "$ERTS_INCLUDE_DIR" -I "$ERL_INTERFACE_INCLUDE_DIR" -lei -pthread 2>/dev/null
if [ $? -eq 0 ]; then
  echo "-DHAS_EI_INIT=1"
fi

rm _test_has_ei_init.c

