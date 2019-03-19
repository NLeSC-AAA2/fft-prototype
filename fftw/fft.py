## ------ language="Python" file="fft.py"
from fft_codelets import (
    generate_codelet, indent_code, default_config, build_shared_object, declare_extern_c, load_notw_codelet, load_twiddle_codelet)
from noodles.run.threading.sqlite3 import run_parallel
from noodles import serial
import noodles
import numpy as np


def run(wf):
    return run_parallel(wf, registry=serial.base, n_threads=4, db_file="lib/db", echo_log=False)


def generate_fft(config, variant, **kwargs):
    kwargs["name"] = "fft"
    code_p = indent_code(
        declare_extern_c(
            generate_codelet(config, variant, **kwargs)))
    so_p = build_shared_object(config, code_p)
    shared_object = run(so_p)

    if variant == "notw":
        return load_notw_codelet(
            shared_object, kwargs["name"], "float32", kwargs["n"])
    elif variant == "twiddle":
        return load_twiddle_codelet(
            shared_object, kwargs["name"], "float32", kwargs["n"])
    else:
        raise ValueError("Unknown FFT variant: " + variant)


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
