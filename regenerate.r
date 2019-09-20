#' install.packages("devtools")
#' devtools::install_github("klutometis/roxygen")
library(devtools)
library(roxygen2)

document("journalclub")

system("R CMD INSTALL journalclub")
system("R CMD Rd2pdf journalclub")
system("mv journalclub.pdf journalclub/docs/")

