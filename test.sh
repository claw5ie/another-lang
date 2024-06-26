#!/bin/bash

set -eu

parser_tests="tests/parser/*.test"
typechecker_tests="tests/typechecker/*.test"
tests="${parser_tests} ${typechecker_tests}"
total_count=0
failed_count=0

for file in $tests
do
    ((total_count += 1))

    printf "Testing \"%s\"... " $file

    base_file_path=${file%.*}
    if [ -e "${base_file_path}.expect" ]; then
        set +e
        output=$(./compiler "${base_file_path}.test" 2>&1 > /dev/null)
        set -e
        expected=$(cat "${base_file_path}.expect")

        if [ "$output" == "$expected" ]; then
            echo -e "\e[32mOK\e[0m"
        else
            echo -e "\e[31mFAILED\e[0m"
            ((failed_count += 1))
        fi
    else
        echo "missing expected output"
        ((failed_count += 1))
    fi
done

echo ""
echo "Failed ${failed_count}/${total_count} tests"

if [ $failed_count -gt 0 ]; then
    exit 1
fi
