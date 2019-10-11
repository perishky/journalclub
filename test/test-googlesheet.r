library(googlesheets)

gs <- gs_title("epiepi")

library(journalclub)

candidates <- journalclub.candidates("input-test", recent=60)
candidates.gs <- journalclub.candidates(gs, recent=60)

stopifnot(is.null(candidates))
stopifnot(is.null(candidates.gs))

query <- "\"DNA methylation\" AND \"BMI\" AND \"smoking\""
writeLines(query,
           con="input-test/pubmed-query.txt")

candidates <- journalclub.candidates("input-test", recent=60)
candidates.gs <- journalclub.candidates(gs, query=query, recent=60)

stopifnot(identical(candidates, candidates.gs))

presented <- candidates$pmid[1:floor(nrow(candidates)/2)]

journalclub.update("input-test", presented)
journalclub.update(gs, presented)

candidates.rep <- journalclub.candidates("input-test", recent=60)
candidates.rep.gs <- journalclub.candidates(gs, query=query, recent=60)

stopifnot(identical(candidates.rep$pmid, candidates.rep.gs$pmid))

candidates.rep2 <- journalclub.candidates("input-test", recent=60)
candidates.rep2.gs <- journalclub.candidates(gs, query=query, recent=60)

stopifnot(identical(candidates.rep2$pmid, candidates.rep2.gs$pmid))

query <- "\"DNA methylation\" AND \"smoking\" AND \"alcohol\" AND \"gender\""
writeLines(query,
           con="input-test/pubmed-query.txt")

candidates.mod <- journalclub.candidates("input-test", recent=60)
candidates.mod.gs <- journalclub.candidates(gs, query=query, recent=60)

presented.mod <- candidates.mod$pmid[1:floor(nrow(candidates.mod)/2)]

journalclub.update("input-test", presented.mod)
journalclub.update(gs, presented.mod)

## stopifnot(identical(journalclub.presented("input-test"), journalclub.presented(gs)))

