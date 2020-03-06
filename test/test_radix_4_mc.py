import pytest
import pyopencl as cl
import pyopencl.cltypes
import numpy as np

from fft.util import parity
from fft.multi_channel import MultiChannel, comp_idx, comp_perm


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


def test_comp_idx_4(cl_context, program):
    queue = cl.CommandQueue(cl_context)
    x = np.arange(mc.L, dtype=cl.cltypes.int)
    y = np.zeros_like(x)
    x_g = cl.Buffer(cl_context, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(cl_context, mf.WRITE_ONLY, x.nbytes)

    for k in range(1, 5):
        for j in range(4):
            program.test_comp_idx_4(
                queue, (mc.L,), None,
                cl.cltypes.int(k), cl.cltypes.int(j), x_g, y_g)
            cl.enqueue_copy(queue, y, y_g)

            y_ref = mc.index_loc \
                .reshape([-1, 4, 4**k]) \
                .transpose([0, 2, 1]) \
                .reshape([-1, 4])[::4, j]
            y_ref_2 = np.array([comp_idx(4, i, j, k-1) for i in range(mc.L)])
            assert np.all(y == y_ref)
            assert np.all(y == y_ref_2)


def test_comp_perm_4(cl_context, program):
    queue = cl.CommandQueue(cl_context)
    x = np.arange(mc.L, dtype=cl.cltypes.int)
    y = np.zeros_like(x)
    x_g = cl.Buffer(cl_context, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(cl_context, mf.WRITE_ONLY, x.nbytes)

    for r in range(4):
        program.test_comp_perm_4(queue, (mc.L,), None, cl.cltypes.int(r), x_g, y_g)
        cl.enqueue_copy(queue, y, y_g)

        y_ref = np.array([comp_perm(radix, i*radix + r) for i in range(mc.L)])
        assert np.all(y == y_ref)


def test_transpose_4(cl_context, program):
    queue = cl.CommandQueue(cl_context)
    x = np.arange(N, dtype=cl.cltypes.int)
    y = np.zeros_like(x)
    x_g = cl.Buffer(cl_context, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(cl_context, mf.WRITE_ONLY, x.nbytes)
    program.test_transpose_4(queue, (N,), None, x_g, y_g)
    cl.enqueue_copy(queue, y, y_g)

    y_ref = x.reshape(mc.factors).T.flatten()
    assert np.all(y == y_ref)


def test_fft4_4(cl_context, program):
    queue = cl.CommandQueue(cl_context)
    x = np.random.normal(size=(1024, 4, 2)).astype(cl.cltypes.float)
    y = np.zeros_like(x)
    x_g = cl.Buffer(cl_context, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(cl_context, mf.WRITE_ONLY, x.nbytes)

    for cycle in range(4):
        y_ref = np.fft.fft(np.roll(x[...,0]+1j*x[...,1], -cycle, axis=1))
        program.test_fft_4(queue, (N,), None, cl.cltypes.int(cycle), x_g, y_g)
        cl.enqueue_copy(queue, y, y_g)
        y_Z = np.roll(y[...,0]+1j*y[...,1], -cycle, axis=1)

        assert abs(y_Z - y_ref).max() < 1e-4


def test_fft_1024(cl_context, program):
    queue = cl.CommandQueue(cl_context)

    x = np.random.normal(size=(1024, 2)).astype(cl.cltypes.float)
    y = np.zeros_like(x)
    x_g = cl.Buffer(cl_context, mf.READ_ONLY | mf.COPY_HOST_PTR, hostbuf=x)
    y_g = cl.Buffer(cl_context, mf.WRITE_ONLY, x.nbytes)
    program.fft_1024(queue, (1,), None, x_g, y_g)
    cl.enqueue_copy(queue, y, y_g)

    y_Z = y[...,0]+1j*y[...,1]
    y_ref = np.fft.fft(x[...,0]+1j*x[...,1])
    assert abs(y_Z - y_ref).max() < 1e-3

