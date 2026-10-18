# Run once when the app starts, before ui.R and server.R: the packages and the
# options. The functions of the app are in R/, which Shiny loads after this file.

# shiny and bslib are attached because the interface uses their functions (div,
# card, observeEvent...) some 240 times; every other package is called with its
# name, e.g. dplyr::mutate().
library(shiny)
library(bslib)

# upload size limit, checked in the browser; Shiny refuses larger requests
# before the server sees them, hence the margin for the multipart overhead
MAX_UPLOAD_MB <- 100
options(shiny.maxRequestSize = (MAX_UPLOAD_MB + 1) * 1024^2)
