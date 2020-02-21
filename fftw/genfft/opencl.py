## ------ language="Python" file="genfft/opencl.py"
import pyopencl as cl
import os
from copy import copy

from .codelets import (generate_codelet, indent_code, default_config, run)
from .parse_c_header import (parse_c_function_decl, opencl_fun_decl, ParseException)
from .fft import make_twiddle
import noodles

## ------ begin <<opencl-macros>>[0]
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


def macros_to_code(m):
    return "\n".join("#define {} {}".format(k, v) for k, v in m.items())
## ------ end
## ------ begin <<opencl-macros>>[1]
int_macros = copy(macros)
int_macros.update({
    "R": "int16",
    "E": "int32" })
## ------ end
## ------ begin <<opencl-twiddle-const>>[0]
def twiddle_const(n1, n2):
    template = "__constant float twiddle_{n1}_{n2}[{n}] = {{\n{values}\n}};"
    twiddles = make_twiddle(n1, n2)[:,1:].copy()
    return template.format(
        n1=n1, n2=n2, n=2*n1*(n2-1),
        values=", ".join(map(str, twiddles.view('float32').flatten())))
## ------ end
## ------ begin <<opencl-twiddle-const>>[1]
def twiddle_const_int(n1, n2):
    template = "__constant int16 twiddle_{n1}_{n2}[{n}] = {{\n{values}\n}};"
    twiddles = make_twiddle(n1, n2)[:,1:].copy().view('float32')
    twiddles_int = (twiddles * (2**10)).astype('int16')
    return template.format(
        n1=n1, n2=n2, n=2*n1*(n2-1),
        values=", ".join(map(str, twiddles_int.flatten())))
## ------ end
## ------ begin <<opencl-two-stage>>[0]
## ------ begin <<opencl-two-stage-template>>[0]
two_stage_template = """
__kernel void fft{n}
    ( __global const float *ci
    , __global float *co )
{{
    notw{n1}(ci, co, {n2s}, 2, {n2}, 2, {n1s});
    twiddle{n2}(co, twiddle_{n1}_{n2}, {n1s}, 0, {n1}, 2);
}}
"""
## ------ end

@noodles.schedule
def two_stage_kernel(cfg, n1, n2):
    k1_p = indent_code(generate_codelet(
        cfg, "notw_complex", n=n1, name="notw{}".format(n1), opencl=True))
    k2_p = indent_code(generate_codelet(
        cfg, "twiddle_complex", n=n2, name="twiddle{}".format(n2), opencl=True))
    k3 = two_stage_template.format(n=n1*n2, n1=n1, n2=n2, n1s=2*n1, n2s=2*n2)
    return noodles.schedule("\n\n".join)(noodles.gather(
        macros_to_code(macros),
        indent_code(twiddle_const(n1, n2)),
        k1_p,
        k2_p,
        k3))
## ------ end


## ------ begin <<opencl-rtc>>[0]
@noodles.schedule
def multi_stage_int(cfg, n1, n2, **args):
    k1_p = indent_code(generate_codelet(
        cfg, "notw", n=n1,
        name="notw{}".format(n1),
        opencl=True, **args))
    k = indent_code(generate_codelet(
        cfg, "twiddle", n=n2, name="twiddle{}".format(n2), opencl=True,
        **args))
    return noodles.schedule("\n\n".join)(noodles.gather(
        macros_to_code(int_macros),
        indent_code(twiddle_const_int(n1, n2)),
        k1_p, k))
## ------ end
## ------ begin <<opencl-rtc>>[1]
@noodles.schedule
def single_stage_r2c(cfg, n1, direction='f', **args):
    k1_p = indent_code(generate_codelet(
        cfg, "r2c" + direction, n=n1,
        name="r2c{}_{}".format(direction, n1),
        opencl=True, **args))
    return noodles.schedule("\n\n".join)(noodles.gather(
        macros_to_code(macros),
        k1_p))
## ------ end
## ------ begin <<opencl-rtc>>[2]
rtc_two_stage_template = """
__kernel void fft{n}
    ( __global const float *ri
    , __global float *co )
{{
    notw{n1}(ri, ri+1, co, co+1, {n2s}, 2, 2, {n2}, 2, {n1s});
    twiddle{n2}(co, twiddle_{n1}_{n2}, {n1s}, 0, {n1}, 2);
}}
"""
## ------ end
## ------ begin <<opencl-rtc>>[3]
@noodles.schedule
def multi_stage_hc2hc(cfg, n1, n2, **args):
    k1_p = indent_code(generate_codelet(
        cfg, "r2cf", n=n1,
        name="notw{}".format(n1),
        opencl=True, **args))
    k = indent_code(generate_codelet(
        cfg, "hc2hc", n=n2, name="twiddle{}".format(n2), opencl=True,
        **args))
    return noodles.schedule("\n\n".join)(noodles.gather(
        macros_to_code(macros),
        indent_code(twiddle_const(n1, n2)),
        k1_p, k))
## ------ end
## ------ end
