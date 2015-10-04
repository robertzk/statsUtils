Miscellaneous statistical utilities for syberiaMungebits [![Build Status](https://travis-ci.org/robertzk/statsUtils.svg?branch=master)](https://travis-ci.org/robertzk/statsUtils) [![Coverage Status](https://coveralls.io/repos/robertzk/statsUtils/badge.png)](https://coveralls.io/r/robertzk/statsUtils)
------------

Intended to be used in conjunction with [syberiaMungebits](https://github.com/robertzk/syberiaMungebits).

Currently, the only real statistical meat here is [sure independence screening](R/sure_independence_screening.r)
and some [extraction of caret helpers](R/caret.R).

# Installation

This package is not yet available from CRAN (as of October 5, 2015).
To install the latest development builds directly from GitHub, run this instead:

```R
if (!require("devtools")) install.packages("devtools")
devtools::install_github("robertzk/statsUtils")
```

