## ------ language="Python" file="fft.py"
from codelets import (
    generate_fft, default_config)
import numpy as np


def w(k, n):
    return np.exp(2j * np.pi * k / n)


def make_twiddle(n1, n2):
    I1 = np.arange(n1)
    I2 = np.arange(n2)
    return w(I1[:,None] * I2[None,:], n1*n2).astype('complex64')


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
