#!/bin/bash

export TEST_ENV="TEST_VALUE"
set -eu

function run_test {
	ret=0
	dir=`mktemp -d --tmpdir xconfig.test.XXXXXXXX`
	cd "$dir"

	read path
	path=$PWD/${path//:/:$PWD\/}
	path=${path//;/;$PWD\/}

	IFS=`/bin/echo -ne "\n"`
	expected_filename="true"
	while read line; do
		if [ "$expected_filename" ]; then
			filename="$line"
			mkdir -p `dirname $filename`
			expected_filename=
		else
			echo "$line" >> "$filename"
		fi
		if [ "$line" = "..." ]; then
			expected_filename="true"
		fi
	done

	xconfig -p "$path" k "" > outcome
	if ! diff -q expected outcome; then
		echo "### mismatch on $1"
		diff -u -U 100 expected outcome
		ret=1
	fi

	cd - > /dev/null
	rm -rf "$dir"

	return $ret;
}

num_tests=0
num_failed=0
echo "LAUNCHING xconfigd"
xconfigd &

for testfile in *.test; do
	if ! run_test $testfile < $testfile; then
		num_failed=$((num_failed + 1))
	fi
	num_tests=$((num_tests + 1))
done

echo "Failed $num_failed of $num_tests tests"
killall xconfigd
