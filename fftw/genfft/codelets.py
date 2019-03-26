## ------ language="Python" file="genfft/codelets.py"
import subprocess
import re
import uuid

import numpy as np

from pathlib import (Path)
from ctypes import (cdll, c_void_p, c_ssize_t)
from collections import (namedtuple)

import noodles

## ------ begin <<codelet-build-config>>[0]
default_config = {
    "compiler":       "g++",
    "compile_args":   ["-x", "c++", "-O3", "-include", "genfft/codelet.hh", "-shared"],
    "build_path":     "lib",
    "generator_path": "../genfft",
    "generator_name": "gen_{variant}.native"
}
## ------ end
## ------ begin <<codelet-signatures>>[0]
CodeletSignature = namedtuple(
    "CodeletSignature", ["return_type", "arg_types"])

codelet_signatures = {
    "notw":    CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_void_p,
               c_ssize_t, c_ssize_t, c_ssize_t, c_ssize_t, c_ssize_t]),
    "notw_complex": CodeletSignature(
        None, [c_void_p, c_void_p,
               c_ssize_t, c_ssize_t, c_ssize_t, c_ssize_t, c_ssize_t]),
    "twiddle": CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_ssize_t,
               c_ssize_t, c_ssize_t, c_ssize_t]),
    "twiddle_complex": CodeletSignature(
        None, [c_void_p, c_void_p, c_ssize_t,
               c_ssize_t, c_ssize_t, c_ssize_t])
}
## ------ end
## ------ begin <<codelet-generator>>[0]
@noodles.schedule(store=True)
def generate_codelet(
        config, variant, *,
        n, compact=True, standalone=True,
        **kwargs):
    """Generate a codelet using one of the FFTW3 codelet generators.

    :param config: Configuration, see `default_config`
    :type config: `dict`
    :param variant: "notw" or "twiddle"
    :type variant: `str`
    :returns: code generated by the `genfft` program
    :rtype: `str`

    All other arguments are passed on to `genfft`. If the argument value has
    type `bool`, it is passed as a flag, otherwise the value is converted to
    a string and passed as argument.

    :param n: FFT radix
    :param compact: (flag) group variable declarations
    :param standalone: (flag) don't include FFTW3 boilerplate
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
## ------ end
## ------ begin <<codelet-extern-c>>[0]
@noodles.schedule
def declare_extern_c(source):
    """Change the function declaration to extern "C".

    :param source: C code
    :type source: `str`
    :returns: modified C code
    :rtype: `str`
    """
    return "\n".join(re.sub("^void", "extern \"C\" void", line)
                     for line in source.splitlines())
## ------ end
## ------ begin <<codelet-indent>>[0]
@noodles.schedule
def indent_code(source):
    """Call the `indent` utility to pretty-print C-code.

    :param source: C code
    :type source: `str`
    :returns: indented C code
    :rtype: `str`
    """
    result = subprocess.run(
        ["indent", "-nut"], check=True, text=True, input=source,
        capture_output=True)
    return result.stdout
## ------ end
## ------ begin <<codelet-compile-command>>[0]
def compile_command(config, target):
    """Generate compiler command.

    :param config: Configuration, see `default_config`
    :type config: `dict`
    :param target: Path to target
    :type target: `str` or `Path`
    :returns: command suitable for `subprocess.run`
    :rtype: list of `str`
    """
    return [config["compiler"]] + config["compile_args"] \
        + ["-o", str(target), "-"]
## ------ end
## ------ begin <<codelet-build>>[0]
@noodles.schedule(store=True)
def build_shared_object(config, source):
    """Compile a C source into a shared object."""
    target = Path(config["build_path"]) / (uuid.uuid4().hex + ".so")
    target.parent.mkdir(exist_ok=True)
    subprocess.run(
        compile_command(config, target),
        check=True, text=True, input=source, stderr=subprocess.PIPE)
    return Path(target)
## ------ end

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

        n_w = radix - 1
        assert(input_array.dtype == twiddle_factors.dtype)
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
## ------ begin <<load-notw-complex-codelet>>[0]
def load_notw_complex_codelet(shared_object, function_name, dtype, radix):
    signature = codelet_signatures["notw_complex"]
    ## ------ begin <<load-function>>[0]
    lib = cdll.LoadLibrary(shared_object)
    fun = getattr(lib, function_name)
    fun.argtypes = signature.arg_types
    dtype = np.dtype(dtype)
    ## ------ end
    ## ------ begin <<make-notw-complex-wrapper>>[0]
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
        print("notw {} {} {} {}".format(radix, n, input_strides, output_strides))
        fun(input_array.ctypes.data,
            output_array.ctypes.data,
            input_strides[-1], output_strides[-1], n,
            input_strides[0], output_strides[0])
    ## ------ end
    return fft_notw
## ------ end
## ------ begin <<load-twiddle-complex-codelet>>[0]
def load_twiddle_complex_codelet(shared_object, function_name, dtype, radix):
    signature = codelet_signatures["twiddle_complex"]
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

        n_w = radix - 1
        assert(input_array.dtype == twiddle_factors.dtype)
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
        print("twiddle {} {} {}".format(radix, n, input_strides))
        fun(input_array.ctypes.data,
            twiddle_factors.ctypes.data,
            input_strides[-1], 0, n, input_strides[0])

    return fft_twiddle
## ------ end

## ------ begin <<noodles-run>>[0]
from noodles.run.threading.sqlite3 import run_parallel
from noodles import serial

def run(wf):
    return run_parallel(
        wf, registry=serial.base, n_threads=4,
        db_file="lib/db", echo_log=False)
## ------ end

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
    if variant == "notw_complex":
        return load_notw_complex_codelet(
            shared_object, kwargs["name"], "float32", kwargs["n"])
    if variant == "twiddle":
        return load_twiddle_codelet(
            shared_object, kwargs["name"], "float32", kwargs["n"])
    if variant == "twiddle_complex":
        return load_twiddle_complex_codelet(
            shared_object, kwargs["name"], "float32", kwargs["n"])
    raise ValueError("Unknown FFT variant: " + variant)
## ------ end
