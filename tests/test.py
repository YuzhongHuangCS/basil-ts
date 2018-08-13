#
#   Run basic tests that ensure things function without completely breaking stuff
#

import os

# R unit tests
os.system("Rscript 'basil-ts/tests/testthat.R'")

# Python API tests
os.system("python3 tests/test_api.py")
