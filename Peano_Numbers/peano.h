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
}