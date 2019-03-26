## ------ language="Python" file="genfft/opencl.py"
import pyopencl as cl
import os
from copy import copy

from codelets import (generate_codelet, indent_code, default_config)
from noodles.run.single.vanilla import run_single
from parse_c_header import (parse_c_function_decl, opencl_fun_decl, ParseException)
import numpy as np

cfg = copy(default_config)
ctx = cl.create_some_context()
queue = cl.CommandQueue(ctx)
print("Running on: ", ctx.devices[0].name)


macros = {
    "R": "float",
    "E": "R",
    "stride": "int",
    "INT": "int",
    "K(x)": "((E) x)",
    "DK(name,value)": "const E name = K(value)",
    "WS(s,i)": "s*i",
    "MAKE_VOLATILE_STRIDE(x,y)": "0",
    "FMA(a,b,c)": "a * b + c",
    "FMS(a,b,c)": "a * b - c",
    "FNMA(a,b,c)": "-a * b - c",
    "FNMS(a,b,c)": "-a * b + c"
}

def macros_to_options(m):
    return sum([["-D", k + "=" + v] for k, v in macros.items()], [])

def macros_to_code(m):
    return "\n".join("#define {} {}".format(k, v) for k, v in macros.items())

c16 = run_single(generate_codelet(
    cfg, "notw_complex", n=16, name="notw16", opencl=True))
c24 = run_single(generate_codelet(
    cfg, "twiddle_complex", n=24, name="twiddle24", opencl=True))

ct384 = """
__kernel void fft384
    ( __global const float *ci
    , __global float *co )
{
    notw16(ci, co, 48, 2, 24, 2, 32);
    twiddle24(co, twiddle_16_24, 32, 0, 16, 2);
}
"""

def w(k, n):
    return np.exp(2j * np.pi * k / n)

def make_twiddle(n1, n2):
    I1 = np.arange(n1)
    I2 = np.arange(n2)
    return w(I1[:,None] * I2[None,:], n1*n2).astype('complex64')


def twiddle_const(n1, n2):
    twiddles = make_twiddle(n1, n2)[:,1:].copy()
    return "__constant float twiddle_{n1}_{n2}[{n}] = {{\n    {values}\n}};".format(
        n1=n1, n2=n2, n=2*n1*(n2-1), values=", ".join(map(str, twiddles.view('float32').flatten())))

opencl_code = "\n".join([
    macros_to_code(macros),
    twiddle_const(16, 24),
    c16,
    c24,
    ct384])

print(opencl_code)

prg = cl.Program(ctx, opencl_code).build()

import numpy as np

mf = cl.mem_flags
x = np.arange(384, dtype='complex64')
x_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
y_g = cl.Buffer(ctx, mf.WRITE_ONLY, x.nbytes)

prg.fft384(
    queue, (1,), (1,), x_g, y_g)

y = np.zeros_like(x)
cl.enqueue_copy(queue, y, y_g)

print(y)
print(np.abs(y - np.fft.fft(x)).max())
## ------ end
