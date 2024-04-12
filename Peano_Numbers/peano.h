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
}