## ------ language="Python" file="fft_codelets.py"
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

## ------ begin <<load-notw-codelet>>[0]
def load_notw_codelet(shared_object, function_name, dtype, radix):
    signature = codelet_signatures["notw"]
    ## ------ begin <<load-function>>[0]
    lib = cdll.LoadLibrary(shared_object)
    fun = getattr(lib, function_name)
    fun.argtypes = signature.arg_types
    dtype = np.dtype(dtype)
    ## ------ end
    ## ------ begin <<make-wrapper>>[0]
    def fft_notw(input_array, output_array):
        ## ------ begin <<notw-assertions>>[0]
        assert(input_array.shape == output_array.shape)
        if dtype == 'float32':
            assert(input_array.dtype == 'complex64')
            assert(output_array.dtype == 'complex64')
        if dtype == 'float64':
            assert(input_array.dtype == 'complex128')
            assert(output_array.dtype == 'complex128')
        ## ------ end
        ## ------ begin <<input-strides>>[0]
        float_size = dtype.itemsize
        input_strides = [s // float_size for s in input_array.strides]
        if input_array.ndim == 1:
            n = 1
        else:
            n = input_array.shape[0]
        ## ------ end
        output_strides = [s // float_size for s in output_array.strides]
    
        fun(input_array.ctypes.data, input_array.ctypes.data + float_size,
            output_array.ctypes.data, output_array.ctypes.data + float_size,
            input_strides[-1], output_strides[-1], n,
            input_strides[0], output_strides[0])
    ## ------ end
    return fft_notw
## ------ end
## ------ begin <<load-twiddle-codelet>>[0]
def load_twiddle_codelet(shared_object, function_name, dtype, radix):
    signature = codelet_signatures["twiddle"]
    ## ------ begin <<load-function>>[0]
    lib = cdll.LoadLibrary(shared_object)
    fun = getattr(lib, function_name)
    fun.argtypes = signature.arg_types
    dtype = np.dtype(dtype)
    ## ------ end

    def fft_twiddle(input_array, twiddle_factors):
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

        n_w = 2 * (radix - 1)    
        if input_array.ndim == 1:
            assert(twiddle_factors.shape == (n_w,))
        else:
            assert(twiddle_factors.shape == (input_array.shape[0], n_w))

        ## ------ begin <<input-strides>>[0]
        float_size = dtype.itemsize
        input_strides = [s // float_size for s in input_array.strides]
        if input_array.ndim == 1:
            n = 1
        else:
            n = input_array.shape[0]
        ## ------ end
        fun(input_array.ctypes.data, input_array.ctypes.data + float_size,
            twiddle_factors.ctypes.data,
            input_strides[-1], 0, n, input_strides[0])

    return fft_twiddle
## ------ end
## ------ end
