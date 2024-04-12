#include <iostream>
#include "peano.h" // 替换为你的文件名

using namespace std;
using namespace Peano_Numbers;

int main() {
    // Test addition
    using Result1 = Add<Zero, Zero>;
    static_assert(Result1::value == 0, "Test 1 failed, expected 0");
    using Result2 = Add<Zero, Succ<Zero>>;
    static_assert(Result2::value == 1, "Test 2 failed, expected 1");
    using Result3 = Add<Succ<Zero>, Zero>;
    static_assert(Result3::value == 1, "Test 3 failed, expected 1");
    using Result4 = Add<Succ<Zero>, Succ<Zero>>;
    static_assert(Result4::value == 2, "Test 4 failed, expected 2");
    cout << "All addition tests passed!" << endl;
    return 0;
}
