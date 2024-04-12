#!/bin/bash

make all
all_test_files="test_add test_div test_mul test_sub test_other"
for test_file in ${all_test_files}; do
    echo "Running ${test_file}..."
    ./${test_file}
    echo "==========================="
done

echo "All tests completed."
make clean
