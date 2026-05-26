# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

if (!requireNamespace("testthat", quietly = TRUE)) {
  message("testthat is not installed; skipping package tests.")
  quit(save = "no", status = 0)
}

library(testthat)
library(oglcnac)

test_check("oglcnac")
