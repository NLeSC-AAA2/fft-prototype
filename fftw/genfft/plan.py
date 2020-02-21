## ------ language="Python" file="genfft/plan.py"
from typing import NamedTuple
from functools import reduce
import operator

class PointerOffset(NamedTuple):
    name: str
    offset: int

    def __str__(self):
        if self.offset == 0:
            return self.name
        else:
            return "{} + {}".format(self.name, self.offset)


def calc_stride(shape, s=1):
    if len(shape) == 0:
        return tuple()
    else:
        return calc_stride(shape[:-1], s*shape[-1]) + (s,)

def tails(x):
    return (x[i:] for i in range(len(x)))

def remove(x, i):
    return x[:i] + x[i+1:]

def replace(x, i, v):
    return x[:i] + (v,) + x[i+1:]

def insert(x, i, v):
    return x[:i] + (v,) + x[i:]

def product(x):
    return reduce(operator.mul, x, 1)

class Array:
    def __init__(self, name, dtype, shape, offset=0, stride=None):
        self.name = name
        self.dtype = dtype
        self.shape = shape
        self.offset = offset
        self.stride = stride or calc_stride(shape)

    @property
    def ndim(self):
        return len(self.shape)

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
        if product(s) != product(self.shape):
            raise ValueError(
                "Cannot reshape shape {} to {}, sizes don't match.".format(self.shape, s))
        if not self.contiguous:
            raise ValueError(
                "Cannot reshape, since strides would become non-trivial.")
        newstride = calc_stride(s, min(self.stride))
        return Array(self.name, self.dtype, shape=s, offset=self.offset, stride=newstride)

    @property
    def contiguous(self):
        x = calc_stride(self.shape)
        return self.stride == x or self.stride == x[::-1]
            
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
            shape=replace(self.shape, d, (b - a)//s),
            offset=self.offset + self.stride[d]*a,
            stride=replace(self.stride, d, s * self.stride[d]))

    def _extrude(self, d):
        return Array(
            self.name, self.dtype,
            shape=insert(self.shape, d, 1),
            offset=self.offset,
            stride=insert(self.stride, d, self.stride[d] * self.shape[d]))

    def __getitem__(self, slices):
        if not isinstance(slices, tuple):
            slices = (slices,)
        
        x = self
        for i, s in enumerate(slices):
            if s is None:
                x = x._extrude(i)
            elif isinstance(s, int):
                x = x._select(i, s)
            elif isinstance(s, slice):
                x = x._slice(i, s)
            else:
                raise TypeError("Index needs to be integer or slice: {}, type: {}".format(s, type(s)))

        return x

    @property
    def ptr(self):
        return PointerOffset(self.name, self.offset)


class Codelet:
    def __init__(self, prefix, radix):
        self.prefix = prefix
        self.radix = radix

    def __call__(self, *args):
        return "{}_{}({})".format(
            self.prefix, self.radix, ", ".join(map(str, args)))


def plan_notw(dtype, radix):
    def notw(input_array, output_array):
        ## ------ begin <<notw-assertions>>[0]
        assert(input_array.shape == output_array.shape)
        if dtype == 'float32':
            assert(input_array.dtype == 'complex64')
            assert(output_array.dtype == 'complex64')
        if dtype == 'float64':
            assert(input_array.dtype == 'complex128')
            assert(output_array.dtype == 'complex128')
        ## ------ end

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
        ## ------ begin <<shape-assertions>>[0]
        if input_array.ndim == 1:
            assert(input_array.size == radix)
        elif input_array.ndim == 2:
            assert(input_array.shape[1] == radix)
        else:
            raise ValueError("Expecting array of dimension 1 or 2.")
        ## ------ end

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

        return Codelet("twiddle", radix)(
            input_array.real.ptr, input_array.imag.ptr,
            twiddle_factors.ptr,
            input_array.real.stride[-1], 0, n, input_array.real.stride[0])
    return twiddle


def n_factor_fft(ns):
    if len(ns) == 1:
        return plan_notw("float32", radix=ns[0])

    n = product(ns[1:])
    m = ns[0]
    fft_n = n_factor_fft(ns[1:])
    fft_m = plan_twiddle("float32", radix=m)
    W = Array("twiddle_{}_{}".format(n, m), shape=(n, m-1), dtype='complex64')
    
    def fft(x, y):
        expr = ['loop',]
        for i in range(x.shape[0]):
            z = y[i].reshape([m, n])
            expr.append([fft_n(x[i].reshape([n, m]).T, z), fft_m(z.T, W)])
        return tuple(expr)

    return fft


def full_factor_fft(n):
    from sympy.ntheory import factorint
    factors = sum(([k] * v for k, v in factorint(n).items()), [])

    _fft = n_factor_fft(factors)    
    def fft(x):
        y = Array("y", shape=x.shape, dtype=x.dtype)
        return _fft(x[None,:], y[None,:])
    
    return fft
## ------ end
