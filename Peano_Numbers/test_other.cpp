#include <iostream>
#include "peano.h" // 替换为你的文件名

using namespace std;
using namespace Peano_Numbers;

int main() {
    // Test Even operation
    {
        // Test Even1
        using Even1 = Even<Zero>;
        static_assert(Even1::value == true, "Test Even1 failed");
        
        // Test Even2
        using Even2 = Even<Succ<Zero>>;
        static_assert(Even2::value == false, "Test Even2 failed");
        
        // Test Even3
        using Even3 = Even<Succ<Succ<Zero>>>;
        static_assert(Even3::value == true, "Test Even3 failed");
        
        cout << "All Even tests passed!" << endl;
    }

    // Test Odd operation
    {
        // Test Odd1
        using Odd1 = Odd<Zero>;
        static_assert(Odd1::value == false, "Test Odd1 failed");
        
        // Test Odd2
        using Odd2 = Odd<Succ<Zero>>;
        static_assert(Odd2::value == true, "Test Odd2 failed");
        
        // Test Odd3
        using Odd3 = Odd<Succ<Succ<Zero>>>;
        static_assert(Odd3::value == false, "Test Odd3 failed");
        
        cout << "All Odd tests passed!" << endl;
    }

    // Test Less than operation
    {
        // Test LT1
        using LT1 = LT<Zero, Zero>;
        static_assert(LT1::value == false, "Test LT1 failed");
        
        // Test LT2
        using LT2 = LT<Zero, Succ<Zero>>;
        static_assert(LT2::value == true, "Test LT2 failed");
        
        // Test LT3
        using LT3 = LT<Succ<Zero>, Zero>;
        static_assert(LT3::value == false, "Test LT3 failed");
        
        // Test LT4
        using LT4 = LT<Succ<Zero>, Succ<Zero>>;
        static_assert(LT4::value == false, "Test LT4 failed");
        
        cout << "All Less than tests passed!" << endl;
    }

    // Test Greater than operation
    {
        // Test GT1
        using GT1 = GT<Zero, Zero>;
        static_assert(GT1::value == false, "Test GT1 failed");
        
        // Test GT2
        using GT2 = GT<Zero, Succ<Zero>>;
        static_assert(GT2::value == false, "Test GT2 failed");
        
        // Test GT3
        using GT3 = GT<Succ<Zero>, Zero>;
        static_assert(GT3::value == true, "Test GT3 failed");
        
        // Test GT4
        using GT4 = GT<Succ<Zero>, Succ<Zero>>;
        static_assert(GT4::value == false, "Test GT4 failed");
        
        cout << "All Greater than tests passed!" << endl;
    }

    // Test Equal operation
    {
        // Test EQ1
        using EQ1 = EQ<Zero, Zero>;
        static_assert(EQ1::value == true, "Test EQ1 failed");
        
        // Test EQ2
        using EQ2 = EQ<Zero, Succ<Zero>>;
        static_assert(EQ2::value == false, "Test EQ2 failed");
        
        // Test EQ3
        using EQ3 = EQ<Succ<Zero>, Zero>;
        static_assert(EQ3::value == false, "Test EQ3 failed");
        
        // Test EQ4
        using EQ4 = EQ<Succ<Zero>, Succ<Zero>>;
        static_assert(EQ4::value == true, "Test EQ4 failed");
        
        cout << "All Equal tests passed!" << endl;
    }
    return 0;
}
