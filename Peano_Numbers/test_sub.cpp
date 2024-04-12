#include <iostream>
#include "peano.h"

using namespace std;
using namespace Peano_Numbers;

int main() {
    // Test subtraction
    using Result1 = Sub<Zero, Zero>;
    static_assert(Result1::value == 0, "Test 1 failed, expected 0");
    using Result2 = Sub<Succ<Zero>, Zero>;
    static_assert(Result2::value == 1, "Test 2 failed, expected 1");
    using Result3 = Sub<Succ<Succ<Zero>>, Succ<Zero>>;
    static_assert(Result3::value == 1, "Test 3 failed, expected 0");

    // Test subtraction with negative result
    // This test should trigger a static_assertion failure
    // A < B
    // using Result4 = Sub<Zero, Succ<Succ<Zero>>>; // 0 - 2
    // printf("Result4::value = %d\n", Result4::value);
    cout << "All subtraction tests passed!" << endl;
    return 0;
}
