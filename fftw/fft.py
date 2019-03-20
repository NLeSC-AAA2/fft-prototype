## ------ language="Python" file="fft.py"
from fft_codelets import (
    generate_fft)
import numpy as np


if __name__ == "__main__":
    fft12 = generate_fft(default_config, "notw", n=12)
    twid4 = generate_fft(default_config, "twiddle", n=4)
    fft4 = generate_fft(default_config, "notw", n=4)
    fft3 = generate_fft(default_config, "notw", n=3)

    x = np.arange(12, dtype='complex64')
    y = np.zeros_like(x)
    z = np.zeros_like(x)
    fft3(x.reshape([4, 3]), y.reshape([4,3]))
    fft4(y.reshape([4, 3]).T, z.reshape([4,3]).T)
    print(z)

## ------ end
