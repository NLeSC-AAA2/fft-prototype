## ------ language="Python" file="genfft/parse_c_header.py"
## ------ begin <<vardecl-type>>[0]
from typing import NamedTuple

class VarDecl(NamedTuple):
    decl_type: str
    is_const: bool
    is_pointer: bool
    name: str
## ------ end
## ------ begin <<make-var-decl>>[0]
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
## ------ end
## ------ begin <<parse-c-function-decl>>[0]
from pyparsing import (
    Word, alphas, OneOrMore, ZeroOrMore, Optional, delimitedList,
    Literal, Suppress, Group, FollowedBy, ParseException )

def parse_c_function_decl(line):
    ## ------ begin <<parse-c-function-decl-body>>[0]
    id_chars = alphas + '_'
    ## ------ end
    ## ------ begin <<parse-c-function-decl-body>>[1]
    pointer_sym = ZeroOrMore(' ') + Literal('*') + ZeroOrMore(' ')
    ## ------ end
    ## ------ begin <<parse-c-function-decl-body>>[2]
    var_decl = OneOrMore(
        Word(id_chars) | pointer_sym + Word(id_chars)) \
        .setParseAction(make_var_decl)
    ## ------ end
    ## ------ begin <<parse-c-function-decl-body>>[3]
    arguments = Group(delimitedList(var_decl))
    function_decl = var_decl + Suppress('(') + arguments + Suppress(')')
    try:
        result = function_decl.parseString(line)
    except (ParseException, AssertionError):
        raise ValueError("Not a valid C function declaration", line)
    return result
    ## ------ end
## ------ end
## ------ begin <<c-fun-decl>>[0]
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
## ------ end
## ------ begin <<opencl-fun-decl>>[0]
def opencl_var_decl(var):
    decl = c_var_decl(var)
    if var.is_pointer:
        return "__global " + decl
    else:
        return decl

def opencl_fun_decl(fun):
    return "__kernel " + c_var_decl(fun[0]) + "(" + \
        ", ".join(map(opencl_var_decl, fun[1])) + ")"
## ------ end

if __name__ == "__main__":
    x = parse_c_function_decl(
        "void fft_notw(const R * ri, const R * ii, R * ro, R * io, " + 
        "stride is, stride os, INT v, INT ivs, INT ovs)")
    print("C declaration:\n    ", c_fun_decl(x))
    print("OpenCL declaration:\n    ", opencl_fun_decl(x))
## ------ end
