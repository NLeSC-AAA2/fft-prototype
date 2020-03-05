from dataclasses import dataclass
import math
import numpy as np

from itertools import groupby
from functools import partial
from .util import parity


def channels(N, radix):
    parity_r = partial(parity, radix)
    return groupby(sorted(range(N), key=parity_r), parity_r)


@dataclass
class MultiChannel:
    N: int
    radix: int
    
    @property
    def depth(self):
        return int(math.log(self.N, self.radix))
    
    @property
    def M(self):
        return self.N//self.radix
    
    @property
    def L(self):
        return self.M//self.radix
    
    @property
    def factors(self):
        return [self.radix] * self.depth
    
    @property
    def channels(self):
        return channels(self.N, self.radix)
    
    @property
    def channel_loc(self):
        x = np.zeros(shape=(self.N,), dtype=int)
        for g, i in self.channels:
            x[list(i)] = g
        return x.reshape(self.factors)
    
    @property
    def index_loc(self):
        x = np.zeros(shape=(self.N,), dtype=int)
        for g, i in self.channels:
            x[list(i)] = np.arange(self.M, dtype=int)
        return x.reshape(self.factors)
    
    def mix(self, x):
        return tuple(x[list(i)].copy() for g, i in self.channels)

    def unmix(self, s):
        x = np.zeros(shape=(self.N,), dtype='complex64')
        for g, i in self.channels:
            x[list(i)] = s[g]
        return x

