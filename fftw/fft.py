## ------ language="Python" file="fft.py"
from codelets import (
    generate_fft, default_config)
import numpy as np


fft16 = generate_fft(default_config, "notw", n=16)
fft4 = generate_fft(default_config, "notw", n=4)
fft5 = generate_fft(default_config, "notw", n=5)
fft3 = generate_fft(default_config, "notw", n=3)
fft2 = generate_fft(default_config, "notw", n=2)
twiddle4 = generate_fft(default_config, "twiddle", n=4)
twiddle3 = generate_fft(default_config, "twiddle", n=3)
twiddle5 = generate_fft(default_config, "twiddle", n=5)

def w(k, n):
    return np.exp(-2j * np.pi * k / n)


def make_twiddle(n1, n2):
    I1 = np.arange(n1)
    I2 = np.arange(n2)
    return w(I1[:,None] * I2[None,:], n1*n2).astype('complex64')


def fft_p16(x):
    yA = np.zeros_like(x).reshape([4,4])
    yB = yA.T
    z = np.zeros_like(x)
    fft4(x.reshape([4,4]).T, yA)
    W = make_twiddle(4, 4)
    yA *= W
    fft4(yB, z.reshape([4,4]).T)
    return z


def fft_p12(x):
    y = np.zeros_like(x).reshape([3, 4])
    z = np.zeros_like(x)

    fft4(x.reshape([4, 3]).T, y)
    W = make_twiddle(3, 4)
    y *= W
    fft3(y.T, z.reshape([3, 4]).T)
    return z


def fft_p15(x):
    y = np.zeros_like(x).reshape([5, 3])
    z = np.zeros_like(x)

    fft3(x.reshape([3, 5]).T, y)
    W = make_twiddle(5, 3)
    y *= W
    fft5(y.T, z.reshape([5, 3]).T)
    return z


def fft_t15(x):
    y = np.zeros_like(x).reshape([5, 3])
    fft3(x.reshape([3, 5]).T, y)
    W = make_twiddle(3, 5)
    twiddle5(y.T, W[:,1:])
    return y.flatten()


def fft_t12(x):
    y = np.zeros_like(x).reshape([4, 3])
    fft4(x.reshape([4, 3]).T, y.T)
    W = make_twiddle(4, 3)
    twiddle3(y, W[:,1:])
    return y.flatten()


x = np.arange(12, dtype='complex64')
x15 = np.arange(15, dtype='complex64')

np.set_printoptions(precision=3)
print(np.fft.fft(x))
print(fft_p12(x))
print(fft_t12(x))
print()
print(make_twiddle(4, 3))
print(make_twiddle(2,2))
print()
print("FFT 5 x 3")
print(np.fft.fft(x15))
print(fft_p15(x15))
print(fft_t15(x15))


## ------ end
