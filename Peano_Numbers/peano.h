namespace Peano_Numbers {
    struct Peano{        
    };
    struct Zero : Peano{
        static constexpr int value = 0;
    };

    template <class T>
    struct Succ : Peano{
        static constexpr int value = T::value + 1;
    };

    // Add operation
    template <class A, class B>
    struct Add : Peano{
        static constexpr int value = A::value + B::value;
    };

    // Subtract operation
    template <class A, class B>
    struct Sub : Peano{
        // The result of subtraction is always non-negative, print error message if the result is negative
        static_assert(A::value >= B::value, "Subtraction result is negative");
        static constexpr int value = A::value - B::value;
    };

    // Multiply operation
    template <class A, class B>
    struct Mul : Peano{
        static constexpr int value = A::value * B::value;
    };

    // Divide operation
    template <class A, class B>
    struct Div : Peano{
        static_assert(B::value != 0, "Division by zero");
        static constexpr int value = A::value / B::value;
    };

    // Even operation
    template <class A>
    struct Even : Peano{
        static constexpr bool value = A::value % 2 == 0;
    };

    // Odd operation
    template <class A>
    struct Odd : Peano{
        static constexpr bool value = A::value % 2 != 0;
    };
    // LT, Less than operation
    template <class A, class B>
    struct LT : Peano{
        static constexpr bool value = A::value < B::value;
    };
    // GT, Greater than operation
    template <class A, class B>
    struct GT : Peano{
        static constexpr bool value = A::value > B::value;
    };
    // EQ, Equal operation
    template <class A, class B>
    struct EQ : Peano{
        static constexpr bool value = A::value == B::value;
    };
}