# Running codelets on the fly

``` {.py file=fft_codelets.py}
import noodles
import ctypes
import subprocess
import re

import tempfile
import uuid

import numpy as np

from pathlib import (Path)
from ctypes import (cdll, c_void_p, c_ssize_t)
from collections import (namedtuple)

CodeletSignature = namedtuple("CodeletSignature", ["return_type", "arg_types"])

# JSON compatible config dictionary
default_config = {
    "compiler":       "g++",
    "compile_args":   ["-x", "c++", "-O3", "-include", "codelet.hh", "-shared"],
    "build_path":     "lib",
    "generator_path": "../genfft",
    "generator_name": "gen_{variant}.native"
}

codelet_signatures = {
    "twiddle": CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_ssize_t, c_ssize_t, c_ssize_t]),
    "notw":    CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_void_p, c_ssize_t, c_ssize_t,
               c_ssize_t, c_ssize_t, c_ssize_t])
}


def compile_command(config, target):
    return [config["compiler"]] + config["compile_args"] \
        + ["-o", str(target), "-"]


@noodles.schedule(store=True)
def generate_codelet(
        config, variant, *,
        n, compact=True, standalone=True,
        **kwargs):
    """Generate a codelet using one of the FFTW3 codelet generators.
    """
    kwargs["n"] = n
    kwargs["compact"] = compact
    kwargs["standalone"] = standalone

    def make_arg(name, value):
        if isinstance(value, bool):
            if value:
                return ["-" + name]
            else:
                return []
        else:
            return ["-" + name, str(value)]

    exe = Path(config["generator_path"]) \
        / config["generator_name"].format(variant=variant)
    command = sum(map(make_arg, kwargs.keys(), kwargs.values()), [str(exe)])
    result = subprocess.run(
        command, check=True, text=True, capture_output=True)
    return result.stdout


@noodles.schedule
def indent_code(source):
    result = subprocess.run(
        ["indent", "-nut"], check=True, text=True, input=source,
        capture_output=True)
    return result.stdout


@noodles.schedule
def declare_extern_c(source):
    return "\n".join(re.sub("^void", "extern \"C\" void", line)
                     for line in source.splitlines())


@noodles.schedule(store=True)
def build_shared_object(config, source):
    """Compile a C source into a shared object."""
    target = Path(config["build_path"]) / (uuid.uuid4().hex + ".so")
    target.parent.mkdir(exist_ok=True)
    subprocess.run(
        compile_command(config, target),
        check=True, text=True, input=source)
    return Path(target)

<<load-notw-codelet>>
<<load-twiddle-codelet>>
```

## Testing

``` {.py file=fft.py}
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

```
