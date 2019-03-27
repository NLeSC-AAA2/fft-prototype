## ------ language="Python" file="genfft/fft.py"
from .codelets import (
    generate_fft, default_config)
import numpy as np

## ------ begin <<twiddle-factors>>[0]
def w(k, n):
    return np.exp(2j * np.pi * k / n)

def make_twiddle(n1, n2):
    I1 = np.arange(n1)
    I2 = np.arange(n2)
    return w(I1[:,None] * I2[None,:], n1*n2).astype('complex64')
## ------ end
## ------ begin <<fft-two-factor>>[0]
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
## ------ end
## ------ begin <<fft-two-factor-twiddle>>[0]
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
## ------ end
## ------ begin <<fft-full-factor>>[0]
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
## ------ end
## ------ begin <<fft-full-factor>>[1]
def full_factor_fft(config, n):
    from sympy.ntheory import factorint
    factors = sum(([k] * v for k, v in factorint(n).items()), [])

    _fft = n_factor_fft(config, factors)    
    def fft(x):
        y = np.zeros_like(x)
        _fft(x[None,:], y[None,:])
        return y
    
    return fft
## ------ end
## ------ end
