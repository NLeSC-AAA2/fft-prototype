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

def openclize(code):
    def tr(line):
        try:
            decl = parse_c_function_decl(line)
            return opencl_fun_decl(decl)
        except ValueError:
            return line

    return "\n".join(tr(line) for line in code.splitlines())


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

c_code = run_single(generate_codelet(cfg, "notw", n=16, name="fft"))
opencl_code = macros_to_code(macros) + "\n" + openclize(c_code)

prg = cl.Program(ctx, opencl_code).build()

import numpy as np

mf = cl.mem_flags
xr = np.arange(16, dtype='float32')
xi = np.zeros(16, dtype='float32')
xr_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=xr)
xi_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=xi)
yr_g = cl.Buffer(ctx, mf.WRITE_ONLY, xr.nbytes)
yi_g = cl.Buffer(ctx, mf.WRITE_ONLY, xi.nbytes)

prg.fft(
    queue, (1,), None, xr_g, xi_g, yr_g, yi_g,
    np.int32(1), np.int32(1), np.int32(1), np.int32(32), np.int32(32))
    
yr = np.zeros(16, dtype='float32')
yi = np.zeros(16, dtype='float32')
cl.enqueue_copy(queue, yr, yr_g)
cl.enqueue_copy(queue, yi, yi_g)

print(yr + 1j*yi)
## ------ end
