import pytest
import pyopencl as cl
import pyopencl.cltypes
import numpy as np

from fft.util import parity
from fft.multi_channel import MultiChannel


N = 1024
radix = 4
mc = MultiChannel(N, radix)
mf = cl.mem_flags


@pytest.fixture
def program(cl_context):
    kernel = open("test/fft1024.cl", "r").read()
    prog = cl.Program(cl_context, kernel).build(["-DTESTING"])
    return prog


def test_parity_4(cl_context, program):
    queue = cl.CommandQueue(cl_context)
    x = np.arange(N, dtype=cl.cltypes.int)
    y = np.zeros_like(x)
    x_g = cl.Buffer(cl_context, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(cl_context, mf.WRITE_ONLY, x.nbytes)
    program.test_parity_4(queue, (N,), None, x_g, y_g)
    cl.enqueue_copy(queue, y, y_g)

    y_ref = np.array([parity(radix, i) for i in range(N)]) 
    assert np.all(y == y_ref)

