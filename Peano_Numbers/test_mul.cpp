#include <iostream>
#include "peano.h"

using namespace std;
using namespace Peano_Numbers;

int main() {
    // Test multiplication
    using Result1 = Mul<Zero, Zero>;
    static_assert(Result1::value == 0, "Test 1 failed, expected 0");

    using Result2 = Mul<Zero, Succ<Zero>>;
    static_assert(Result2::value == 0, "Test 2 failed, expected 0");

    using Result3 = Mul<Succ<Zero>, Zero>;
    static_assert(Result3::value == 0, "Test 3 failed, expected 0");

    using Result4 = Mul<Succ<Zero>, Succ<Zero>>;
    static_assert(Result4::value == 1, "Test 4 failed, expected 1");

    using Result5 = Mul<Succ<Succ<Zero>>, Succ<Succ<Zero>>>;
    static_assert(Result5::value == 4, "Test 5 failed, expected 4");

    cout << "All multiplication tests passed!" << endl;
    return 0;
}
