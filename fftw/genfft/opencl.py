## ------ language="Python" file="genfft/opencl.py"
import pyopencl as cl
import os
from copy import copy

from codelets import (generate_codelet, indent_code, default_config)
from noodles.run.single.vanilla import run_single
from parse_c_header import (parse_c_function_decl, opencl_fun_decl, ParseException)

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
     
opencl_code = macros_to_code(macros) + "\n" + c16 + "\n" + c24

prg = cl.Program(ctx, opencl_code).build()

import numpy as np

mf = cl.mem_flags
x = np.arange((16, 24), dtype='complex64')
x_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
y_g = cl.Buffer(ctx, mf.WRITE_ONLY, x.nbytes)

prg.notw16(
    queue, (1,), (1,), x_g, y_g,
    np.int32(2), np.int32(2*16), np.int32(1), np.int32(24), np.int32(2))
prg.twiddle24(
    queue, (1,), (1,), ...
y = np.zeros_like(x)
cl.enqueue_copy(queue, y, y_g)

print(y)
## ------ end
