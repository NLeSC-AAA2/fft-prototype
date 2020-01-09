## ------ language="Python" file="test/test_ocl_rtc.py"
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
## ------ end
