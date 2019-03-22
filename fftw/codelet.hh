// ------ language="C++" file="codelet.hh"
#include <cstddef>

// ------ begin <<real-types>>[0]
using R = float;
using E = R;

#define K(x) ((E) x)
#define DK(name, value) const E name = K(value)
// ------ end
// ------ begin <<integer-types>>[0]
using INT = ptrdiff_t;
// ------ end
// ------ begin <<fused-arithmetic>>[0]
inline E FMA(E a, E b, E c) { return a * b + c; }
inline E FMS(E a, E b, E c) { return a * b - c; }
inline E FNMA(E a, E b, E c) { return -a * b - c; }
inline E FNMS(E a, E b, E c) { return -a * b + c; }
// ------ end
// ------ begin <<array-indices>>[0]
using stride = INT;

template <typename T>
inline INT WS(stride s, T i) { return s * i; }

template <typename NPtr, typename X>
inline constexpr INT MAKE_VOLATILE_STRIDE(NPtr nptr, X x) { return 0; }
// ------ end
// ------ end
