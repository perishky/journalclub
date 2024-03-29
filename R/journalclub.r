load.input <- function(dir, name) {
    if ("googlesheet" %in% class(dir)) { ## dir <- gs_title("spreadsheet-name")
        require(googlesheets)
        if (name %in% gs_ws_ls(dir))
            as.data.frame(gs_read(ss=dir, ws=name))[[1]][-1]
        else {
            gs_ws_new(ss=dir, ws_title=name, input=data.frame(values=0)) 
            return(character(0))
        }   
    } else {
        filename <- file.path(dir, paste(name, "txt", sep="."))        
        if (!file.exists(filename)) {
            warning("Creating ", filename)
            file.create(filename)
        }
        readLines(filename)
    }
}

save.output <- function(dir, name, lines) {
    old <- load.input(dir, name)
    lines <- setdiff(lines, old)
    
    if ("googlesheet" %in% class(dir)) {
        gs_add_row(ss=dir, ws=name, input=data.frame(values=lines))
    } else {
        filename <- file.path(dir, paste(name, "txt", sep="."))
        stopifnot(file.exists(filename))
        con <- file(filename, "a")
        writeLines(as.character(lines), con=con)
        close(con)
    }
    return(TRUE)
}




#' Retrieve new publications for a journal club
#'
#' @param dir Directory where the journal club history is stored.
#' @param query PubMed query for identifying new publications (Default: NULL).
#' @param recent Number of days to look back for publications from the current day (Default: 30).
#' @param retmax Number of publications per batch when requesting citing publications (Default: 20).
#' @return A data frame listing recent publications
#' that have not been considered for previous journal clubs
#' and either match the input query or cite a previously presented publication.
#' @export
journalclub.candidates <- function(dir, query=NULL, recent=30, retmax=20, debug=F) {
    if (debug)
        browser()

    if ("character" %in% class(dir)) {
        if (!file.exists(dir)) {
            warning("Creating directory ", dir)
            dir.create(dir, recursive=T)
        }
    }

    presented <- load.input(dir, "presented")
    ignore <- load.input(dir, "ignore")
    if (is.null(query))
        query <- load.input(dir, "pubmed-query")
        
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
        cite.pmids <- journalclub.citing(presented, days=recent, retmax=retmax)

    new.pmids <- setdiff(c(query.pmids, cite.pmids), c(presented, ignore))
    if (length(new.pmids) == 0)
        NULL
    else {
        new.papers <- journalclub.annotate(new.pmids,retmax=retmax)
        save.output(dir, "ignore", new.pmids)
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
    save.output(dir, "presented", presented)
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
journalclub.annotate <- function(pmids, retmax=50, abstract=T) {
    pmids <- na.omit(as.integer(gsub(" ", "", pmids)))
    papers <- lapply(seq(1,length(pmids),retmax), function(start) {
        pmids <- na.omit(pmids[start:(start+retmax-1)])
        annotate.pmids(pmids)
    })
    papers <- do.call(rbind.flex, papers)
    colnames(papers) <- tolower(colnames(papers))
    papers <- as.data.frame(papers, stringsAsFactors=F)
    if (abstract) {
        abstracts <- lapply(seq(1,length(pmids),retmax), function(start) {
            pmids <- na.omit(pmids[start:(start+retmax-1)])
            retrieve.abstracts(pmids)
        })
        abstracts <- unlist(abstracts)
        papers$abstract <- abstracts[match(papers$pmid, names(abstracts))] 
    }    
    papers
}

annotate.pmids <- function(
    pmids,
    pubmed.url="https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=") {

    query <- paste(pmids,collapse=",")
    query.url <- paste0(pubmed.url, query)
    results <- try(xmlTreeParse(getURL(query.url)),silent=T)
    if ("try-error" %in% class(results)) {
        if (length(pmids) > 1) {
            papers <- lapply(pmids, annotate.pmids)
            papers <- do.call(rbind.flex, papers)
            as.data.frame(papers, stringsAsFactors=F)
        }
        else {
            warning(paste("PMID", pmids, "could not be annotated."))
            return(data.frame(PMID=pmids))
        }
    } else {
        papers <- xmlRoot(results)
        papers <- lapply(1:length(papers), function(i) {
            paper <- try(papers[[i]], silent=T)
            if ("try-error" %in% class(paper)) {
                warning(paste("PMID", pmids[i], "could not be annotated."))
                return(data.frame(PMID=pmids[i]))
            }
            pmidxml2df(paper)
        })
        papers <- do.call(rbind.flex, papers)
        as.data.frame(papers, stringsAsFactors=F)
    }
}
    
pmidxml2df <- function(paper) {
    ## retrieve column names
    cols <- sapply(1:length(paper), function(j) {
        tryCatch({
            attrs <- xmlAttrs(paper[[j]])
            idx <- which(names(attrs) == "Name")
            idx <- idx[1]
            attrs[[idx]]
        }, error=function(e) {
            NA
        })
    })
    cols[1] <- "PMID"
    
    ## retrieve column types
    types <- sapply(1:length(paper), function(j) {
        tryCatch({
            attrs <- xmlAttrs(paper[[j]])
            idx <- which(names(attrs) == "Type")
            idx <- idx[1]
            attrs[[idx]]
        }, error=function(e) {
            NA
        })
    })
    
    ## retrieve column values
    vals <- sapply(1:length(paper), function(j) {
        tryCatch({
            if (types[j] == "List") {
                suppressWarnings(
                    items <- sapply(1:length(paper[[j]]), function(k)
                                    xmlValue(paper[[j]][[k]])))
                paste(items, collapse=",")
            }
            else
                xmlValue(paper[[j]])
        }, error=function(e) {
            NA
        })
    })
    
    ## set empty values to missing
    idx <- which(sapply(vals, length)==0)
    if (length(idx) > 0)
        vals[idx] <- NA
    
    ## name values
    names(vals) <- cols
    unlist(vals)
}

retrieve.abstracts <- function(
    pmids,
    pubmed.url="https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&retmode=xml&rettype=abstract&id=") {
    query <- paste(pmids, collapse=",")
    query.url <- paste0(pubmed.url, query)
    results <- try(xmlTreeParse(getURL(query.url)), silent=TRUE)
    if ("try-error" %in% class(results)) {
        if (length(pmids) > 1) 
            unlist(lapply(pmids, retrieve.abstracts))
        else {
            NULL
        }
    }
    else {
        papers <- xmlRoot(results)
        pmids <- sapply(1:length(papers), function(i) {
            tryCatch({
                xmlValue(papers[[i]][["MedlineCitation"]][["PMID"]])
            }, error=function(e) {
                0
            })
        })
        abstracts <- lapply(1:length(papers), function(i) {
            tryCatch({
                xmlValue(papers[[i]][["MedlineCitation"]][["Article"]][["Abstract"]])
            }, error=function(e) {
                ""
            })
        })
        names(abstracts) <- pmids
        abstracts
    }
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
    if (length(results[["IdList"]]) > 0)
        sapply(1:length(results[["IdList"]]),
               function(i) as.character(xmlValue(results[["IdList"]][[i]])))
    else
        character(0)
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
    pmids <- na.omit(as.integer(gsub(" ", "", pmids)))

    starts <- seq(1,length(pmids),retmax)
    ends <- c(tail(starts,-1)-1, length(pmids))
    cpmids <- unique(unlist(lapply(1:length(starts), function(i) {
        if (verbose)
            cat(date(), "retrieving citations for pmids",
                starts[i], "-", ends[i], "\n", file=stdout())
        tryCatch({
            retrieve.citing.papers(pmids[starts[i]:ends[i]],days)
        }, error=function(e) {
            print(e)
            NULL
        })
    })))
    setdiff(as.character(cpmids), as.character(pmids))
}

retrieve.citing.papers <- function(
    pmids,
    days,
    pubmed.url="https://eutils.ncbi.nlm.nih.gov/entrez/eutils/elink.fcgi?dbfrom=pubmed&linkname=pubmed_pubmed_citedin") {
    mindate <- format(Sys.Date()-days, "%Y/%m/%d")
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
