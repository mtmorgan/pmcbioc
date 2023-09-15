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

#' @rdname xml_parse
#'
#' @title Parse a PubMedCentral XML file to a DuckDB database 
#'
#' @description `xml_parse()` processes a PubMedCentral XML file by
#'     extracting XML elements, transforming them to text or integer
#'     representations, and loading the result into a database.
#'
#' @details
#'
#' `xml_parse()` can be slow, e.g., 1000 records per minute, and grow
#' to consume a large amount of memory, e.g., 18 Gb.
#'
#' `xml_parse()` uses `XML::xmlEventParse()` to iterate through the
#' XML file. Each `//article` branch is queried using XPath
#' expressions. The expressions and subsequent transformations are
#' meant to extract the following information; currently, not all
#' records are processed correctly.
#'
#' - `pmcid`: The PubMedCentral identifier associated with the record.
#'
#' - `title`: The article title.
#'
#' - `journal`: The journal in which the article was published.
#'
#' - `year`: The year of publication, represented as an
#'   integer. Articles may have several publication dates (e.g.,
#'   electronically before physically). `year` is the earliest date in
#'   the record.
#'
#' - `pmid`: PubMed identifier of the record.
#'
#' - `surname`: Surname of each author. Some authors have only a surname.
#'
#' - `givenname`: Given name(s) of each author. Some authors have only
#'   given names.
#'
#' - `keyword`: Keywords associated with the publication. Keywords are
#'   not standardized
#'
#' - `refpmid`: PubMed identifiers of each reference in the record.
#'
#' `pmcid` is used as a key across database tables.
#'
#' @return
#'
#' `xml_parse()` returns the `pmcbioc_db` database passed as argument
#' `db`, updated to include the tables described here. The tables are
#' as follows:
#'
#' - `article` includes columns `pmcid`, `title`, `journal`, `year`,
#'   and `pmid`.
#'
#' - `author` is a one-to-many map between `pmcid` and author
#'   `surname` and `givenname`.
#'
#' - `keyword` is a one-to-many map between `pmcid` and `keyword`.
#'
#' - `refpmid` is a one-to-many map between `pmcid` and the `refpmid`
#'   PubMed identifiers of cited references.

#' @importFrom XML xmlEventParse
#'
#' @export
xml_parse <-
    function(xml_file, db)
{
    stopifnot(
        is_scalar_character(xml_file),
        file.exists(xml_file),
        inherits(db, "pmcbioc_db"),
        !db$is_read_only()
    )

    xmlEventParse(xml_file, branches = xml_branches(db))
    db$flush()

    db
}
