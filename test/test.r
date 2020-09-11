library(journalclub)

recent <- 360*5 ## 5 years

candidates <- journalclub.candidates("input-test", recent=recent)

stopifnot(is.null(candidates))

writeLines("\"DNA methylation\" AND \"BMI\" AND \"smoking\"",
           con="input-test/pubmed-query.txt")

candidates <- journalclub.candidates("input-test", recent=recent)

presented <- candidates$pmid[1:floor(nrow(candidates)/2)]

journalclub.update("input-test", presented)

stopifnot(identical(sort(presented),
                    sort(readLines("input-test/presented.txt"))))

stopifnot(identical(sort(union(readLines("input-test/presented.txt"), 
                               readLines("input-test/ignore.txt"))),
                    sort(candidates$pmid)))

candidates.rep <- journalclub.candidates("input-test", recent=recent)

stopifnot(!any(candidates.rep$pmid %in% candidates$pmid))

candidates.rep2 <- journalclub.candidates("input-test", recent=recent)

stopifnot(!any(candidates.rep2$pmid %in% c(candidates.rep2$pmid, candidates$pmid)))

writeLines("\"DNA methylation\" AND \"smoking\" AND \"alcohol\" AND \"gender\"",
           con="input-test/pubmed-query.txt")

candidates.mod <- journalclub.candidates("input-test", recent=recent)

presented.mod <- candidates.mod$pmid[1:floor(nrow(candidates.mod)/2)]

journalclub.update("input-test", presented.mod)

stopifnot(identical(sort(union(presented,presented.mod)),
                    sort(readLines("input-test/presented.txt"))))

stopifnot(identical(sort(unique(c(candidates$pmid,
                                  candidates.rep$pmid,
                                  candidates.rep2$pmid,
                                  candidates.mod$pmid))),
                    sort(readLines("input-test/ignore.txt"))))
