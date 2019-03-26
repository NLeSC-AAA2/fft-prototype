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

``` {.python file=genfft/opencl.py}
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
```


