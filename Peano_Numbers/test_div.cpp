#include <iostream>
#include "peano.h"

using namespace std;
using namespace Peano_Numbers;

int main() {
    // Test division
    using Result1 = Div<Zero, Succ<Zero>>;
    static_assert(Result1::value == 0, "Test 1 failed, expected 0");
    
    using Result2 = Div<Succ<Succ<Zero>>, Succ<Zero>>;
    static_assert(Result2::value == 2, "Test 2 failed, expected 2");
    
    // Test division by zero
    // This test should trigger a static_assertion failure
    // A divided by 0
    // using Result3 = Div<Zero, Zero>;
    // printf("Result3::value = %d\n", Result3::value); // Uncommenting this line will cause a static_assertion failure

    cout << "All division tests passed!" << endl;
    
    return 0;
}
