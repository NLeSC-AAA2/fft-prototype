## ------ language="Python" file="test/codelets.py"
import numpy as np
from ctypes import (cdll, c_void_p, c_ssize_t)
from collections import (namedtuple)

## ------ begin <<codelet-signatures>>[0]
CodeletSignature = namedtuple(
    "CodeletSignature", ["return_type", "arg_types"])

codelet_signatures = {
    "twiddle": CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_ssize_t,
               c_ssize_t, c_ssize_t, c_ssize_t]),
    "notw":    CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_void_p,
               c_ssize_t, c_ssize_t, c_ssize_t, c_ssize_t, c_ssize_t])
}
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

if __name__ == "__main__":
    ## ------ begin <<run-main>>[0]
    fft7 = load_notw_codelet("build/notw-7.gen.so", "fft", "float32", 7)
    x = np.arange(7, dtype='complex64')
    y = np.zeros_like(x)
    fft7(x, y)
    print(y)
    print(np.fft.fft(x))
    ## ------ end
## ------ end
