load.input <- function(filename) {
    if (!file.exists(filename)) {
        warning("Creating ", filename)
        file.create(filename)
    }
    readLines(filename)
}

save.output <- function(filename, lines) {
    stopifnot(file.exists(filename))
    con <- file(filename, "a")
    writeLines(as.character(lines), con=con)
    close(con)
    return(TRUE)
}

#' Retrieve new publications for a journal club
#'
#' @param dir Directory where the journal club history is stored.
#' @param query PubMed query for identifying new publications (Default: NULL).
#' @param recent Number of days to look back for publications from the current day (Default: 30).
#' @return A data frame listing recent publications
#' that have not been considered for previous journal clubs
#' and either match the input query or cite a previously presented publication.
#' @export
journalclub.candidates <- function(dir, query=NULL, recent=30, debug=F) {
    if (debug)
        browser()
    if (!file.exists(dir)) {
        warning("Creating directory ", dir)
        dir.create(dir, recursive=T)
    }
    presented <- load.input(file.path(dir, "presented.txt")) 
    ignore <- load.input(file.path(dir, "ignore.txt"))
    if (is.null(query))
        query <- load.input(file.path(dir, "pubmed-query.txt"))

    if (length(query) == 0) {
        warning("No query has been submitted")
        query.pmids <- NULL
    }
    else
        query.pmids <- journalclub.query(query, days=recent)

    if (length(presented) == 0) {
        warning("There are no previously presented publications to check for citations")
        cite.pmids <- NULL
    }
    else
        cite.pmids <- journalclub.citing(presented, days=recent)

    new.pmids <- setdiff(c(query.pmids, cite.pmids), c(presented, ignore))
    if (length(new.pmids) == 0)
        NULL
    else {
        new.papers <- journalclub.annotate(new.pmids)
        save.output(file.path(dir, "ignore.txt"), new.pmids)
        new.papers
    }
}

#' Update journal club history with newly presented publications
#'
#' @param dir Directory where the journal club history is stored.
#' @param presented List of PMIDs for the publications that were presented.
#' @return TRUE
#' 
#' @export
journalclub.update <- function(dir, presented) {
    save.output(file.path(dir, "presented.txt"), presented)
}


#' Retrieve information for a selection of publications
#'
#' Generates a data frame providing information (abstract, title, authors, journal, publication date)
#' for a list of PMIDs.
#'
#' @param pmids A vector of PMIDs.
#' @return A data frame with one row per publication.
#' 
#' @export
journalclub.annotate <- function(pmids, retmax=100) {
    pmids <- na.omit(as.integer(gsub(" ", "", pmids)))
    
    papers <- lapply(seq(1,length(pmids),retmax), function(start) {
        query <- paste0(paste(na.omit(pmids[start:(start+retmax-1)]),
                              collapse=" "), "[uid]")
        object <- get_pubmed_ids(query)
        papers <- fetch_pubmed_data(object, retmax=retmax)
    
        papers <- articles_to_list(papers)
        papers <- mclapply(1:length(papers), function(i) {
            cat(date(), "retrieving paper", start+i-1, "of", length(pmids), "\n", file=stderr())
            paper <- papers[[i]]
            df <- article_to_df(paper, max_chars=-1, getKeywords=T)
            if (is.null(df)) {
                warning(pmids[[i]], " could not be accessed")
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


#' Query PubMed for recent publications
#'
#' Retrieve PMIDs for recent publications matching a query.
#'
#' @param query PubMed query.
#' @param days Number of days to look back for publications from the current day (Default: 30).
#' @return A vector of PMIDs matching the query.
#' 
#' @export
journalclub.query <- function(query,days=30,retmax=1e5) {
    cat(date(), "Querying PubMed:", query, "\n")
    pubmed.url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed"
    query <- gsub(" ", "+", as.character(query))
    query <- gsub("\"", "%22", query)
    
    mindate <- format(Sys.Date()-days, "%Y/%m/%d")
    maxdate <- format(Sys.Date(), "%Y/%m/%d")
    ## if i don't specify a max date then min date didn't seem to do anything!
    
    query.url <- paste(c(pubmed.url,
                         paste("term", query, sep="="),
                         paste("retmax", format(retmax,scientific=F), sep="="),
                         paste("mindate", mindate, sep="="),
                         paste("maxdate", maxdate, sep="="),
                         "usehistory=n"),
                           collapse="&")
    results <- xmlTreeParse(getURL(query.url))
    results <- xmlRoot(results)
    sapply(1:length(results[["IdList"]]),
           function(i) as.character(xmlValue(results[["IdList"]][[i]])))
}


#' Query PubMed for recent citing publications
#'
#' Retrieve recent publications that cite a given set of publications
#'
#' @param pmids PMIDs for papers to be cited.
#' @param days Number of days to look back for publications from the current day (Default: 30).
#' @return A vector of PMIDs matching the query.
#'
#' @export
journalclub.citing <- function(pmids, days=30, retmax=100, verbose=T) {
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
    setdiff(as.character(cpmids), as.character(pmids))
}
