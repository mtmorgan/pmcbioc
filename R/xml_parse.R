#' @importFrom XML xmlDoc xmlValue xpathApply
xml_branches <- function(db) {
    force(db)
    n <- 0L
    list(article = function(x) {
        doc <- xmlDoc(x) # convert node to document so xpath can be used
        n <<- n + 1L

        ## pmcid
        path <- "//article-meta/article-id[@pub-id-type='pmc']"
        pmcid <- xmlValue(doc[[path]])

        ## article.  pmcid 9694029 has several article-id (different
        ## stages of revision); choose the one that has a PMCID.
        path <- paste0(
            "//front/article-meta/article-id[@pub-id-type = 'pmc']/../..",
            "/article-meta/title-group/article-title"
        )
        article <- xmlValue(doc[path])
        ## FIXME: article-type from <article article-type

        ## journal.  PMCID 9694029 has several journal-title (all
        ## identical; different stages of revision); chose the one
        ## that has a PMCID
        path <- paste0(
            "//front/article-meta/article-id[@pub-id-type = 'pmc']/../..",
            "/journal-meta/journal-title-group/journal-title"
        )
        journal <- xmlValue(doc[[path]])

        ## FIXME: robust selection of publication year
        ## counts of different date tags
        ## ...
        ## 7354       <pub-date pub-type="nihms-submitted">
        ## 22147       <pub-date pub-type="ppub">
        ## 35231       <pub-date pub-type="pmc-release">
        ## 46965       <pub-date pub-type="collection">
        ## 64097       <pub-date pub-type="epub">

        ## first year in any pub-date field
        path <- "//article-metadata/pub-date/year"
        year <- as.integer(xmlValue(doc[path]))
        if (length(year) > 1L)
            year <- min(year)

        ## pmid
        path <- "//article-meta/article-id[@pub-id-type='pmid']"
        pmid <- unlist(xpathApply(doc, path, xmlValue))
        if (is.null(pmid))
            pmid <- NA_character_

        ## authors. Some authors have only 'surname', e.g., pmcid
        ## 10368696, so extract surname and given-names for each
        ## 'name' node. in PMCID 9482647 the first author has no given
        ## names
        ##
        ## FIXME: pmcid 9978720 has 'eastern' (chinese) and English
        ## versions of names; currently ignores
        path <- "//contrib[@contrib-type = 'author']//name"
        surnames <- as.character(unlist(xpathApply(
            doc, path, function(node) xmlValue(node[["surname"]])
        )))
        given_names <- as.character(unlist(xpathApply(
            doc, path, function(node) xmlValue(node[["given-names"]])
        )))

        ## keywords
        keywords <- unlist(xpathApply(doc, "//kwd-group/kwd", xmlValue))

        ## citations
        path <- "//ref//pub-id[@pub-id-type='pmid']"
        ref_pmid <- unlist(xpathApply(doc, path, xmlValue))

        ## write
        db$write_metadata(
               pmcid, article, journal, year, pmid,
               surnames, given_names,
               keywords,
               ref_pmid
           )

        ## progress
        if (n %% 1000L == 0L)
            spdl::info("parsed {} records", n)
    })
}

#' @importFrom XML xmlEventParse
#'
#' @export
xml_parse <-
    function(xml_file, db)
{        
    stopifnot(
        is_scalar_character(xml_file),
        file.exists(xml_file),
        inherits(db, "PMCBioc_db"),
        !db$is_read_only()
    )

    xmlEventParse(xml_file, branches = xml_branches(db))
    db$flush()

    db
}
