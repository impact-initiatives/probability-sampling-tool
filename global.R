# Run once when the app starts, before ui.R and server.R: the packages and the
# options. The functions of the app are in R/, which Shiny loads after this file.

require(data.table)
require(dplyr)
library(shiny)
library(shinyjs)
library(markdown)
library(bslib)

# upload size limit, checked in the browser; Shiny refuses larger requests
# before the server sees them, hence the margin for the multipart overhead
MAX_UPLOAD_MB <- 100
options(shiny.maxRequestSize = (MAX_UPLOAD_MB + 1) * 1024^2)
