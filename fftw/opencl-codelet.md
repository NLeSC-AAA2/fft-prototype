# Generating OpenCL codelets

The structure of C is almost identical to OpenCL. Where a C function looks a bit like

``` {.c}
void do_something
    ( float const *input
    , float *output
    , size_t n )
{
    <<function-body>>
}
```

an OpenCL kernel looks like

``` {.opencl}
__kernel void do_something
    ( __global float const *input
    , __global float *output
    , size_t n )
{
    <<function-body>>
}
```

To have FFTW codelets work with OpenCL, and I say *just work* nothing about efficiency, all we should have to change is this function header.

## Parsing function declaration

I will use `pyparsing` to parse the C function declaration. This will only parse a subset that doesn't involve arguments passed by reference or numbers in arguments or types.

If you've never seen `pyparsing` before, it is reasonably powerful as parser combinators go (still, I'd rather implement this in Haskell if I had the choice ;)

``` {.python #parse-c-function-decl}
from pyparsing import (
    Word, alphas, OneOrMore, ZeroOrMore, Optional, delimitedList,
    Literal, Suppress, Group, FollowedBy, ParseException )

def parse_c_function_decl(line):
    <<parse-c-function-decl-body>>
```

The allowed characters in identifiers are all alphabetical ones and the underscore.

``` {.python #parse-c-function-decl-body}
id_chars = alphas + '_'
```

A pointer symbol may appear somewhere, with or without white-space.

``` {.python #parse-c-function-decl-body}
pointer_sym = ZeroOrMore(' ') + Literal('*') + ZeroOrMore(' ')
```

A variable declaration is a series of words, or words with a pointer symbol suffix.
The tokenized output of this parser is passed onto a function `make_var_decl` that interprets it.

``` {.python #parse-c-function-decl-body}
var_decl = OneOrMore(
    Word(id_chars) | pointer_sym + Word(id_chars)) \
    .setParseAction(make_var_decl)
```

Once we know how to extract a variable declaration, a function declaration follows.

``` {.python #parse-c-function-decl-body}
arguments = Group(delimitedList(var_decl))
function_decl = var_decl + Suppress('(') + arguments + Suppress(')')
try:
    result = function_decl.parseString(line)
except (ParseException, AssertionError):
    raise ValueError("Not a valid C function declaration", line)
return result
```

The declaration is stored in a sequence of `VarDecl`. More precisely, it looks something like:

``` {.python}
[VarDecl, [VarDecl, ...]]
```

where the first declaration is the return-type and the name of the function and the second list of `VarDecl` lists all arguments. The type `VarDecl` is defined as follows:

``` {.python #vardecl-type}
from typing import NamedTuple

class VarDecl(NamedTuple):
    decl_type: str
    is_const: bool
    is_pointer: bool
    name: str
```

From the tokens of a variable declaration, we can construct a `VarDecl`, using `make_var_decl`:

``` {.python #make-var-decl}
def make_var_decl(tokens):
    tokens = list(tokens)
    is_pointer = '*' in tokens
    is_const = 'const' in tokens

    try:
        tokens.remove('*')
        tokens.remove('const')
    except ValueError:
        pass

    assert(len(tokens) == 2)
    var_type = tokens[0]
    name = tokens[1]

    return VarDecl(var_type, is_const, is_pointer, name)
```

From the return value of `parse_c_function_decl` we can reconstruct the original declaration.

``` {.python #c-fun-decl}
def c_var_decl(var):
    decl = var.decl_type + " "
    if var.is_const:
        decl += "const "

    if var.is_pointer:
        decl += "* "

    decl += var.name
    return decl

def c_fun_decl(fun):
    return c_var_decl(fun[0]) + "(" + \
        ", ".join(map(c_var_decl, fun[1])) + ")"
```

And also generate a OpenCL declaration, assuming all given pointers are global.

``` {.python #opencl-fun-decl}
def opencl_var_decl(var):
    decl = c_var_decl(var)
    if var.is_pointer:
        return "__global " + decl
    else:
        return decl

def opencl_fun_decl(fun):
    return "__kernel " + c_var_decl(fun[0]) + "(" + \
        ", ".join(map(opencl_var_decl, fun[1])) + ")"
```

## Example

``` {.python file=genfft/parse_c_header.py}
<<vardecl-type>>
<<make-var-decl>>
<<parse-c-function-decl>>
<<c-fun-decl>>
<<opencl-fun-decl>>

if __name__ == "__main__":
    x = parse_c_function_decl(
        "void fft_notw(const R * ri, const R * ii, R * ro, R * io, " + 
        "stride is, stride os, INT v, INT ivs, INT ovs)")
    print("C declaration:\n    ", c_fun_decl(x))
    print("OpenCL declaration:\n    ", opencl_fun_decl(x))
```

# Running an OpenCL codelet

The declaration of the OpenCL `notw` kernel looks like

``` {.opencl}
__kernel void fft
    ( __global const R * ri
    , __global R * ro
    , stride is, stride os
    , INT v, INT ivs, INT ovs)
```

The same for the `twiddle` kernel

``` {.opencl}
__kernel void fft
    ( __global R * ri
    , __constant R * W
    , stride rs
    , INT mb, INT me, INT ms)
```

To call this kernel (and the twiddle variant) we write another kernel that calls them with the correct strides.

``` {.python file=genfft/opencl.py}
import pyopencl as cl
import os
from copy import copy

from .codelets import (generate_codelet, indent_code, default_config, run)
from .parse_c_header import (parse_c_function_decl, opencl_fun_decl, ParseException)
from .fft import make_twiddle
import noodles

<<opencl-macros>>
<<opencl-twiddle-const>>
<<opencl-two-stage>>
<<opencl-test-program>>

<<opencl-rtc>>
```

## OpenCL Macros

In stead of including the header file that we defined for the C version, we'll revert to the use of some simple macros to make the code compile.

``` {.python #opencl-macros}
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
```

## Twiddle factors

Twiddle factors are expanded into a one-dimensional array of floats. This array resides in the `__constant` namespace.

``` {.python #opencl-twiddle-const}
def twiddle_const(n1, n2):
    template = "__constant float twiddle_{n1}_{n2}[{n}] = {{\n{values}\n}};"
    twiddles = make_twiddle(n1, n2)[:,1:].copy()
    return template.format(
        n1=n1, n2=n2, n=2*n1*(n2-1),
        values=", ".join(map(str, twiddles.view('float32').flatten())))
```

## Two stage FFT

The two-stage Cooley-Tukey FFT kernel has the following template:

``` {.python #opencl-two-stage-template}
two_stage_template = """
__kernel void fft{n}
    ( __global const float *ci
    , __global float *co )
{{
    notw{n1}(ci, co, {n2s}, 2, {n2}, 2, {n1s});
    twiddle{n2}(co, twiddle_{n1}_{n2}, {n1s}, 0, {n1}, 2);
}}
"""
```

Here `n1` and `n2` are the respective factors of the total size $n = n_1 n_2$. The strides are given for pointers to `float`, so there we have to multiply the numbers by 2.

The `two_stage_kernel` function returns a Noodles promise, scheduling the use of GenFFT.

``` {.python #opencl-two-stage}
<<opencl-two-stage-template>>

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
```

## Test 2-stage fft

This test program runs the two-stage kernel for a single array.

``` {.python file=test/test_ocl_two_stage.py}
import numpy as np
import pyopencl as cl

from genfft import generate_fft, default_config
from genfft.opencl import two_stage_kernel, run

from copy import copy

def test_ocl_two_stage_3_4():
    cfg = copy(default_config)
    ctx = cl.create_some_context()
    queue = cl.CommandQueue(ctx)
    code = run(two_stage_kernel(cfg, 3, 4))
    prg = cl.Program(ctx, code).build()

    mf = cl.mem_flags
    x = np.arange(12, dtype='complex64')
    x_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(ctx, mf.WRITE_ONLY, x.nbytes)

    prg.fft12(
        queue, (1,), (1,), x_g, y_g)

    y = np.zeros_like(x)
    cl.enqueue_copy(queue, y, y_g)

    print("// fft([0..12]) = \n// ", "\n//  ".join(str(y).splitlines()))
    print("// abserr = ", np.abs(y - np.fft.fft(np.arange(12))).max())
    assert np.abs(y - np.fft.fft(np.arange(12))).max() < 1e-5
```

# Integer transforms

The goal is to create an integer transform of real size 1024 to half-complex.

A radix-4 transform, we can write without floating-point factors.

``` {.python #opencl-macros}
int_macros = copy(macros)
int_macros.update({
    "R": "int16",
    "E": "int32" })
```

``` {.python #opencl-rtc}
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
```

``` {.python #opencl-twiddle-const}
def twiddle_const_int(n1, n2):
    template = "__constant int16 twiddle_{n1}_{n2}[{n}] = {{\n{values}\n}};"
    twiddles = make_twiddle(n1, n2)[:,1:].copy().view('float32')
    twiddles_int = (twiddles * (2**10)).astype('int16')
    return template.format(
        n1=n1, n2=n2, n=2*n1*(n2-1),
        values=", ".join(map(str, twiddles_int.flatten())))
```

# Real-to-complex FFT

The real-to-complex FFT takes real input of size `N` and returns complex output of size `N//2`. We first need to test the `gen_r2cf` and `gen_r2cb` codelet generators (`f` and `b` standing for forward and backward).

``` {.python #opencl-rtc}
@noodles.schedule
def single_stage_r2c(cfg, n1, direction='f', **args):
    k1_p = indent_code(generate_codelet(
        cfg, "r2c" + direction, n=n1,
        name="r2c{}_{}".format(direction, n1),
        opencl=True, **args))
    return noodles.schedule("\n\n".join)(noodles.gather(
        macros_to_code(macros),
        k1_p))
```

## Half-complex transform

To stage a multi-tier transform on the half-complex domain, we need to understand how to twiddle on the half-complex domain.

``` {.python #opencl-rtc}
rtc_two_stage_template = """
__kernel void fft{n}
    ( __global const float *ri
    , __global float *co )
{{
    notw{n1}(ri, ri+1, co, co+1, {n2s}, 2, 2, {n2}, 2, {n1s});
    twiddle{n2}(co, twiddle_{n1}_{n2}, {n1s}, 0, {n1}, 2);
}}
"""
```

``` {.python #opencl-rtc}
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
```

``` {.python #test-rtc-one}
# cfg = copy(default_config)
# ctx = cl.create_some_context()
# queue = cl.CommandQueue(ctx)

code = run(single_stage_rtc(cfg, 16))
prg = cl.Program(ctx, code).build()
```

## Test

``` {.python file=test/test_ocl_rtc.py}
import numpy as np
import pyopencl as cl

from genfft import generate_fft, default_config
from genfft.opencl import two_stage_kernel, run

from copy import copy

def test_ocl_rtc():
    cfg = copy(default_config)
    ctx = cl.create_some_context()
    queue = cl.CommandQueue(ctx)
    code = run(two_stage_kernel(cfg, 3, 4))
    prg = cl.Program(ctx, code).build()

    mf = cl.mem_flags
    x = np.arange(12, dtype='complex64')
    x_g = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(ctx, mf.WRITE_ONLY, x.nbytes)

    prg.fft12(
        queue, (1,), (1,), x_g, y_g)

    y = np.zeros_like(x)
    cl.enqueue_copy(queue, y, y_g)

    print("// fft([0..12]) = \n// ", "\n//  ".join(str(y).splitlines()))
    print("// abserr = ", np.abs(y - np.fft.fft(np.arange(12))).max())
    assert np.abs(y - np.fft.fft(np.arange(12))).max() < 1e-5
```
