load.input <- function(filename) {
    if (!file.exists(filename)) {
        warning("Creating", filename)
        return(NULL)
    }
    readLines(filename)
}

#' Retrieve new papers for a journal club
#'
#' @param dir Directory where the journal club history is stored.
#' @param query PubMed query for identifying new papers (Default: NULL).
#' @param recent Number of days to look back for publications from the current day (Default: 30).
#' @return A data frame listing recently published papers
#' that have not been considered for previous journal clubs
#' and either match the input query or cite a previously presented paper.
#' @export
retrieve.journal.club.candidates <- function(dir, query=NULL, recent=30) {    
    presented <- load.input(file.path(dir, "presented.txt")) 
    ignore <- load.input(file.path(dir, "ignore.txt"))
    if (is.null(query))
        query <- load.input(file.path(dir, "pubmed-query.txt"))

    if (is.null(query)) {
        warning("No query has been submitted")
        query.pmids <- NULL
    }
    else
        query.pmids <- retrieve.pmids.by.query(query, days=days)

    if (length(pmids) == 0) {
        warning("There are no previously presented papers to check for citations")
        cite.pmids <- NULL
    }
    else
        cite.pmids <- retrieve.citing.pmids(presented, days=days)

    new.pmids <- setdiff(c(query.pmids, cite.pmids), c(presented, ignore))
    if (length(new.pmids) > 0)
        NULL
    else {
        writeLines(union(ignore, new.pmids), file.path(dir, "ignore.txt"))
        retrieve.papers(new.pmids, mc.cores=mc.cores)
    }
}

#' Update journal club history with newly presented papers
#'
#' @param dir Directory where the journal club history is stored.
#' @param presented List of PMIDs for the papers that were presented.
#' 
#' @export
update.journal.club.history <- function(dir, presented) {
    old <- load.input(file.path(dir, "presented.txt")) 
    presented <- union(old, presented)
    writeLines(file.path(dir, "presented.txt"))
}


#' Retrieve information (title, authors, abstract, etc)
#' for the given paper pmids.
#' `retmax` limits the number of papers to be retrieved
#' at a time (but does not limit the total number of papers
#' to retrieve).
retrieve.papers <- function(pmids, retmax=100) {
    pmids <- na.omit(as.integer(gsub(" ", "", pmids)))
    
    papers <- lapply(seq(1,length(pmids),retmax), function(start) {
        query <- paste0(paste(na.omit(pmids[start:(start+retmax-1)]),
                              collapse=" "), "[uid]")
        object <- get_pubmed_ids(query)
        papers <- fetch_pubmed_data(object, retmax=retmax)
    
        papers <- articles_to_list(papers)
        papers <- lapply(1:length(papers), function(i) {
            cat(date(), "retrieving paper", start+i-1, "\n", file=stderr())
            paper <- papers[[i]]
            df <- article_to_df(paper, max_chars=-1, getKeywords=T)
            if (is.null(df)) {
                warning(paste(pmids[[i]], "could not be accessed"))
                return(NULL)
            }
            df$lastname[1] <- paste(df$lastname, collapse=";")
            df$firstname[1] <- paste(df$firstname, collapse=";")
            df[1,]
        })
        do.call(rbind, papers)
    })
    papers <- do.call(rbind, papers)
    papers[match(pmids, papers$pmid),]
}


#' retrieve pmids for papers
#' that match the input query
#' within the few (default: 30) days
retrieve.pmids.by.query <- function(query,days=30,retmax=1e5) {
    pubmed.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed"
    query <- gsub(" ", "+", as.character(query))
    query <- gsub("\"", "%22", query)
    
    mindate <- format(Sys.Date()-days, "%Y/%m/%d")
    
    query.url <- paste(c(pubmed.url,
                         paste("term", query, sep="="),
                         paste("retmax", format(retmax,scientific=F), sep="="),
                         paste("mindate",
                               paste("", mindate, "", sep=""),
                               sep="="),
                         "usehistory=n"),
                           collapse="&")
    print(query.url) 
    results <- xmlTreeParse(getURL(query.url))
    results <- xmlRoot(results)
    sapply(1:length(results[["IdList"]]),
           function(i) xmlValue(results[["IdList"]][[i]]))
}


#' retrieve pmids for papers that cite
#' the papers with input pmids within the last
#' few (default: 30) days
retrieve.citing.papers <- function(pmids, days=30, retmax=100, verbose=T) {
    pubmed.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin"
    
    mindate <- format(Sys.Date()-days, "%Y/%m/%d")
    
    pmids <- na.omit(as.integer(gsub(" ", "", pmids)))

    retrieve.citing.papers.0 <- function(pmids) {
        query.url <- paste(c(pubmed.url,
                             paste("id", pmids, sep="="),
                             paste("retmax",length(pmids),sep="="),
                             paste("mindate",
                                   paste("", mindate, "", sep=""),
                                   sep="="),
                             "usehistory=y"),
                           collapse="&")
    
        results <- xmlTreeParse(getURL(query.url))
        
        results <- xmlRoot(results)
        
        citing.pmids <- lapply(1:length(results), function(i) {
            result <- results[[i]]
            if ("LinkSetDb" %in% names(result)) {
                result <- result[["LinkSetDb"]]
                idx <- which(names(result) == "Link")
                sapply(idx, function(i) xmlValue(result[[i]][["Id"]]))
            }
            else NULL
        })
        unname(unlist(citing.pmids))
    }

    starts <- seq(1,length(pmids),retmax)
    ends <- c(tail(starts,-1)-1, length(pmids))
    cpmids <- unique(unlist(lapply(1:length(starts), function(i) {
        if (verbose)
            cat(date(), "retrieving citations for pmids",
                starts[i], "-", ends[i], "\n", file=stdout())
        retrieve.citing.papers.0(pmids[starts[i]:ends[i]])
    })))
    setdiff(cpmids, pmids)
}