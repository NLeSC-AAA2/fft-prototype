#!/usr/bin/env python

from setuptools import setup, find_packages

test_deps = [
      "pytest>=5,<6"
    , "pytest-cov>=2.8.1,<3"
    , "pytest-mypy>=0.4.2,<1"
    , "flake8>=3,<4" ]

setup(
    name="fftsynth-prototype",
    version="0.1",
    packages=find_packages(),

    install_requires=[
          "numpy"
        , "pyopencl"
        , "pyparsing"
        , "noodles"
    ],
    tests_require=test_deps,
    extras_require={
        "test": test_deps
    },
    # include_package_data=True,
    # package_data={
    #     # If any package contains *.txt or *.rst files, include them:
    #     '': ['*.txt', '*.rst'],
    #     # And include any *.msg files found in the 'hello' package, too:
    #     'hello': ['*.msg'],
    # },

    # metadata to display on PyPI
    author="Johan Hidding",
    author_email="j.hidding@esciencecenter.nl",
    description="Python prototype for FFT synthesis in OpenCL",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    keywords="literate programming",
    url="https://nlesc-aaa2.github.io/",   # project home page, if any

    # project_urls={
    #       "Bug Tracker": "https://github.com/entangled/filters/issues/"
    #     , "Documentation": "https://entangled.github.io/filters/"
    #     , "Source Code": "https://github.com/entangled/filters/"
    # },

    classifiers=[
          'License :: OSI Approved :: Apache Software License'
        , 'Development Status :: 3 - Alpha'
        , 'Intended Audience :: Science/Research'
    ]
    # could also include long_description, download_url, etc.
)

