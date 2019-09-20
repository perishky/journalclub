#' The steps below are needed to regenerate
#' the data objects and documentation files
#' included with the package and then
#' run all tests.

#' install.packages("devtools")
#' devtools::install_github("klutometis/roxygen")
library(devtools)
library(roxygen2)

document("journalclub")

system("R CMD INSTALL journalclub")
reload(inst("journalclub"))

system("R CMD Rd2pdf journalclub")
system("mv journalclub.pdf journalclub/docs")

