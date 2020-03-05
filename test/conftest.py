import pytest
import pyopencl as cl

@pytest.fixture
def cl_context():
    ctx = cl.create_some_context()
    return ctx

