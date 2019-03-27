# Cooley-Tukey algorithm

(sampled from Frigo 1999. I changed array indices to $k, l, m$, $n$ denoting the length of the array.)

The (forward) discrete Fourier transform of $X$ is the array $Y$ given by

$$Y[l] = \sum_{k=0}^{n-1} X[k] w_n^{-kl},$$ {#eq:dft}

where 

$$w_n = \exp\left(\frac{2\pi i}{n}\right).$$ {#eq:wn}

So $w_n^{-kl}$ denotes $w_n$ *to the power of* $-kl$.

In the case that $X$ is real valued, $Y$ will have *hermitian symmetry*

$$Y[n - k] = Y^*[k].$$ {#eq:hermitian-symmetry}

The backward DFT flips the sign at the exponent of $w_n$, giving

$$Y[l] = \sum_{k=0}^{n-1} X[k] w_n^{kl}.$$ {#eq:idft}

Now suppose that $n$ can be factored into $n = n_1 n_2$. We may now view the arrays $X$ and $Y$ in a rectangular shape, where

$$X[k] = X[k_1 n_2 + k_2] = X[k_1, k_2].$$

Also, let $Y$ take the transposed shape of $X$,

$$Y[l] = Y[l_1 + l_2 n_1] = Y[l_1, l_2].$$

Then +@eq:dft can be written as

$$Y[l_1, l_2] = \sum_{k_2 = 0}^{n_2 - 1} \left[\left(\sum_{k_1 = 0}^{n_1-1} X[k_1, k_2] w_{n_1}^{-k_1 l_1}\right) w_n^{l_1 k_2}\right] w_{n_2}^{-l_2 k_2}.$$

Also known as the **Cooley-Tukey fast Fourier transform**.

This separates the DFT in an inner and outer transform, of sizes $n_1$ and $n_2$. The output of the inner transform is multiplied by **twiddle factors** $w_n^{-l_1 k_2}$, before the outer transform is done.

## The twiddle codelet

FFTW comes with special codelets to facilitate this multiplication, and performing the outer transform in one go. The `twiddle` codelet performs the FFT **in place**.

### Using twiddles

We know that the input values need to be multiplied with twiddle factors, but the twiddle for $w_n^0$ is always 1. This is why the `twiddle` codelet expects `n_w = radix - 1` number of complex twiddle factors. If we study the generated codelet carefully, we also find that the input values are multiplied by the complex conjugate of the given twiddle factors.

### Wrapping a twiddle codelet

``` {.python #load-twiddle-codelet}
def load_twiddle_codelet(shared_object, function_name, dtype, radix):
    signature = codelet_signatures["twiddle"]
    <<load-function>>

    def fft_twiddle(input_array, twiddle_factors):
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

        <<input-strides>>
        fun(input_array.ctypes.data, input_array.ctypes.data + float_size,
            twiddle_factors.ctypes.data,
            input_strides[-1], 0, n, input_strides[0])

    return fft_twiddle
```

### Twiddle-complex codelet

``` {.python #load-twiddle-complex-codelet}
def load_twiddle_complex_codelet(shared_object, function_name, dtype, radix):
    signature = codelet_signatures["twiddle_complex"]
    <<load-function>>

    def fft_twiddle(input_array, twiddle_factors):
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

        <<input-strides>>
        print("twiddle {} {} {}".format(radix, n, input_strides))
        fun(input_array.ctypes.data,
            twiddle_factors.ctypes.data,
            input_strides[-1], 0, n, input_strides[0])

    return fft_twiddle
```

## Making a larger transform

The basis of Cooley-Tukey algorithm is to split the computation of the full Fourier transform into two parts. Say the length of the array is $r = nm$. We then first perform $n$ radix $m$ transforms, multiply the output by twiddle factors $w_r^{kl}$ and then do $m$ radix $n$ transforms. Here

$$w_n = \exp\left(\frac{2\pi i}{n}\right).$$

This is easily visualised by putting the values $x_k$ for $k = 0 .. r$ on a rectangular grid of size $n \times m$.

``` {.python #twiddle-factors}
def w(k, n):
    return np.exp(2j * np.pi * k / n)

def make_twiddle(n1, n2):
    I1 = np.arange(n1)
    I2 = np.arange(n2)
    return w(I1[:,None] * I2[None,:], n1*n2).astype('complex64')
```

We can make use of the fact that in NumPy we can transpose and reshape arrays as much as we like, and the underlying data array is the same object by reference.

``` {.python #fft-two-factor}
def fft_two_factor(config, n, m):
    fft_n = generate_fft(config, "notw", n=n)
    fft_m = generate_fft(config, "notw", n=m)
    W = make_twiddle(m, n).conj()

    def fft(x):
        y = np.zeros_like(x).reshape([m, n])
        z = np.zeros_like(x)
        fft_n(x.reshape([n, m]).T, y)
        y *= W
        fft_m(y.T, z.reshape([m, n]).T)
        return z

    return fft
```

## Using the twiddle codelet

This referential quality to NumPy arrays can become tricky when we don't actually want it. Since we don't need the first column of the twiddle factor array, we can just leave it out, but then we need to copy the result, because the codelet expects a contiguous array. After this is done, the resulting code is much cleaner.

``` {.python #fft-two-factor-twiddle}
def fft_two_factor_twiddle(config, n, m):
    fft_n = generate_fft(config, "notw", n=n)
    fft_m = generate_fft(config, "twiddle", n=m)
    W = make_twiddle(n, m)[:,1:].copy()

    def fft(x):
        y = np.zeros_like(x).reshape([m, n])
        fft_n(x.reshape([n, m]).T, y)
        fft_m(y.T, W)
        return y.flatten()

    return fft
```

## Creating arbitrary FFT

Using the basic algorithm of a two stage FFT we can expand to any list of factors, and construct an arbitary length FFT algorithm. This has a for-loop in Python and $log(n)$ recursion.

``` {.python #fft-full-factor}
def n_factor_fft(config, ns):
    if len(ns) == 1:
        return generate_fft(config, "notw", n=ns[0])

    n = np.prod(ns[1:])
    m = ns[0]
    fft_n = n_factor_fft(config, ns[1:])
    fft_m = generate_fft(config, "twiddle", n=m)
    W = make_twiddle(n, m)[:,1:].copy()
    
    def fft(x, y):
        for i in range(x.shape[0]):
            z = y[i].reshape([m, n])
            fft_n(x[i].reshape([n, m]).T, z)
            fft_m(z.T, W)

    return fft
```

Note that the for-loop could be executed in parallel without problem. Using NumPy arrays we can log every call to the codelets and generate a series of commands that generate the correctly strided calls, including annotation on what can be done in parallel.

Using the `n_factor_fft` function we can create an easy wrapper that creates an arbitrary length FFT.

``` {.python #fft-full-factor}
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


## Module

``` {.python file=genfft/fft.py}
from .codelets import (
    generate_fft, default_config)
import numpy as np

<<twiddle-factors>>
<<fft-two-factor>>
<<fft-two-factor-twiddle>>
<<fft-full-factor>>
```
