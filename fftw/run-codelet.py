## ------ language="Python" file="run-codelet.py"
import numpy as np
from ctypes import (cdll, c_void_p, c_ssize_t)
from collections import (namedtuple)

## ------ begin <<codelet-signatures>>[0]
CodeletSignature = namedtuple("CodeletSignature", ["return_type", "arg_types"])

codelet_signatures = {
    "twiddle": CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_ssize_t, c_ssize_t, c_ssize_t]),
    "notw":    CodeletSignature(
        None, [c_void_p, c_void_p, c_void_p, c_void_p, c_ssize_t, c_ssize_t,
               c_ssize_t, c_ssize_t, c_ssize_t])
}
## ------ end
## ------ begin <<load-codelet>>[0]
def load_codelet(shared_object, function_name, signature, dtype, radix):
    ## ------ begin <<load-function>>[0]
    lib = cdll.LoadLibrary(shared_object)
    fun = getattr(lib, function_name)
    fun.argtypes = codelet_signatures[signature].arg_types
    dtype = np.dtype(dtype)
    ## ------ end
    ## ------ begin <<make-wrapper>>[0]
    def fft_notw(input_array, output_array):
        assert(input_array.ndim == 1)
        assert(output_array.ndim == 1)
        assert(input_array.size == radix)
        assert(output_array.size == radix)
        if dtype == 'float32':
            assert(input_array.dtype == 'complex64')
            assert(output_array.dtype == 'complex64')
        if dtype == 'float64':
            assert(input_array.dtype == 'complex128')
            assert(output_array.dtype == 'complex128')
    
        float_size = dtype.itemsize
        input_stride = input_array.strides[0] / float_size
        output_stride = output_array.strides[0] / float_size
        fun(input_array.ctypes.data, input_array.ctypes.data + float_size,
            output_array.ctypes.data, output_array.ctypes.data + float_size,
            input_stride, output_stride, 1, 0, 0)
    ## ------ end
    return fft_notw
## ------ end

if __name__ == "__main__":
    ## ------ begin <<run-main>>[0]
    fft7 = load_codelet("build/notw-7.gen.so", "fft", "notw", "float32", 7)
    x = np.arange(7, dtype='complex64')
    y = np.zeros_like(x)
    fft7(x, y)
    print(y)
    print(np.fft.fft(x))
    ## ------ end
## ------ end
