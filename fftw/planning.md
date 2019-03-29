# Planning a composite FFT

``` {.python #function-stubs}
```

``` {.python file=genfft/plan.py}
import noodles
from typing import NamedTuple


class PointerOffset(NamedTuple):
    name: str
    offset: int

    def __str__(self):
        return "{} + {}".format(self.name, self.offset)


def calc_stride(shape, s=1):
    if len(shape) == 0:
        return tuple()
    else:
        return calc_stride(shape[:-1], s*[shape[-1]]) + (s,)

def tails(x):
    return (x[i:] for i in range(len(x)))

def remove(x, i):
    return (x[j] for j in range(len(x)) if j != i)

def replace(x, i, v):
    return ((x[j], v)[j==i] for j in range(len(x)))


class Array:
    def __init__(self, name, dtype, shape, offset=0, stride=None):
        self.name = name
        self.dtype = dtype
        self.shape = shape
        self.offset = offset
        self.stride = stride or calc_stride(shape)

    @property
    def ndim(self):
        return len(shape)

    @property
    def real(self):
        if self.dtype == 'complex64':
            subtype = 'float32'
        elif self.dtype == 'complex128':
            subtype = 'float64'
        else:
            raise ValueError("dtype is not complex")

        return Array(
            self.name, subtype,
            shape=self.shape,
            offset=self.offset * 2,
            stride=tuple(x*2 for x in self.stride))

    @property
    def imag(self):
        if self.dtype == 'complex64':
            subtype = 'float32'
        elif self.dtype == 'complex128':
            subtype = 'float64'
        else:
            raise ValueError("dtype is not complex")

        return Array(
            self.name, subtype,
            shape=self.shape,
            offset=self.offset * 2 + 1,
            stride=tuple(x*2 for x in self.stride))

    @property
    def T(self):
        return Array(self.name, self.dtype, self.shape[::-1], self.offset, self.stride[::-1])

    def reshape(self, s):
        if np.prod(s) != np.prod(self.shape):
            raise ValueError(
                "Cannot reshape shape {} to {}, sizes don't match.".format(self.shape, s))
        if self.ndim > 1:
            raise ValueError(
                "Cannot reshape, since strides would become non-trivial.")
        newstride = calc_stride(s, self.stride[0])
        return Array(self.name, self.dtype, shape=s, offset=self.offset, stride=newstride)

    def _select(self, d, i):
        return Array(
            self.name, self.dtype,
            shape=tuple(remove(self.shape, d)),
            offset=self.offset + self.stride[d]*i,
            stride=tuple(remove(self.stride, d)))

    def _slice(self, d, s):
        a, b, s = s.indices(self.shape[d])
        return Array(
            self.name, self.dtype,
            shape=tuple(replace(self.shape, d, (b - a)//s)),
            offset=self.offset + self.stride[d]*a,
            stride=tuple(replace(self.stride, d, s * self.stride[d])))

    def __getitem__(self, slices):
        if isinstance(slices, tuple):
            slices = (slices,)
        
        x = self
        for i, s in enumerate(slices):
            if isinstance(s, int):
                x = x._select(i, s)
            elif isinstance(s, slice):
                x = x._slice(i, s)
            else:
                raise TypeError("Index needs to be integer or slice")

        return x

    @property
    def ptr(self):
        return PointerOffset(self.name, self.offset)


class Codelet:
    def __init__(self, prefix, radix):
        self.prefix = prefix
        self.radix = radix

    @noodles.schedule
    def __call__(self, args):
        return "{}_{}({})".format(
            self.prefix, self.radix, ", ".join(map(str, args)))


def plan_notw(dtype, radix):
    def notw(input_array, output_array):
        <<notw-assertions>>

        if input_array.ndim == 1:
            n = 1
        else:
            n = input_array.shape[0]

        return Codelet("notw", radix)(
            input_array.real.ptr, input_array.imag.ptr,
            output_array.real.ptr, output_array.imag.ptr,
            input_array.real.stride[1], output_array.real.stride[1], n,
            input_array.real.stride[0], output_array.real.stride[0])
    return notw

def plan_twiddle(dtype, radix):
    def twiddle(input_array, twiddle_factors):
        if dtype == 'float32':
            assert(input_array.dtype == 'complex64')
        if dtype == 'float64':
            assert(input_array.dtype == 'complex128')
        <<shape-assertions>>

        n_w = radix - 1
        assert(input_array.dtype == twiddle_factors.dtype)
        if input_array.ndim == 1:
            assert(twiddle_factors.shape == (n_w,))
        else:
            assert(twiddle_factors.shape == (input_array.shape[0], n_w))

        if input_array.ndim == 1:
            n = 1
        else:
            n = input_array.shape[0]

        fun(input_array.real.ptr, input_array.imag.ptr,
            twiddle_factors.ptr,
            input_array.real.stride[-1], 0, n, input_array.real.stride[0])
    return twiddle


def n_factor_fft(ns):
    if len(ns) == 1:
        return plan_notw("float32", n=ns[0])

    n = np.prod(ns[1:])
    m = ns[0]
    fft_n = n_factor_fft(config, ns[1:])
    fft_m = plan_twiddle("float32", n=m)
    W = Array("twiddle_{}_{}".format(n, m), shape=(n, m-1), dtype='complex64')
    
    def fft(x, y):
        for i in range(x.shape[0]):
            z = y[i].reshape([m, n])
            fft_n(x[i].reshape([n, m]).T, z)
            fft_m(z.T, W)

    return fft


def full_factor_fft(config, n):
    from sympy.ntheory import factorint
    factors = sum(([k] * v for k, v in factorint(n).items()), [])

    _fft = n_factor_fft(config, factors)    
    def fft(x):
        y = np.zeros_like(x)
        _fft(x[None,:], y[None,:])
        return y
    
    return fft
```

