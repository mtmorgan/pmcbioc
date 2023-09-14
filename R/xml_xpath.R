#' @rdname xml_xpath
#'
#' @title Use XPath to query records of a PubMedCentral XML file 
#'
#' @description `xml_xpath()` queries an indexed PubMedCentral XML
#'     file, returning records indicated by `db_index` and queried
#'     using `xpath`.
#'
#' @param `db_index` a tibble with columns `start` and `length`,
#'     typcially the result of filtering the `index` table of a
#'     `pmcbioc_db()` database.
#'
#' @param `xpath` `NULL` or a `character(1)` XPath expression to query
#'     the records in `db_index`.
#'
#' @return When `xpath = NULL`, `xml_xpath()` returns an XML object
#'     representing all records in `db_index` as a
#'     `<pmc-articleset>...</pmc-articleset>`.
#'
#' @return When `xpath` is `character(1)`, the records in `db_index`
#'     are queried using XPath. The return value can be coerced to an
#'     *R* representation with `XML::xmlValue()`.
#'
#' @importFrom dplyr pull
#'
#' @importFrom XML xmlParse
#'
#' @export
xml_xpath <-
    function(
        db_index,
        xpath = NULL,
        xml_file = "pmc_result.xml"
    )
{
    stopifnot(
        "start" %in% names(db_index),
        "length" %in% names(db_index),
        is.null(xpath) || is_scalar_character(xpath),
        file.exists(xml_file)
    )

    con <- file(xml_file, open = "rb")
    on.exit(close(con))

    articles <- Map(function(start, length) {
        seek(con, start)
        readChar(con, length)
    }, pull(db_index, "start"), pull(db_index, "length"))

    articleset <- paste0(
        "<pmc-articleset>\n",
        paste(articles, collapse = "\n"),
        "</pmc-articleset>\n"
    )

    xml <- xmlParse(articleset)
    if (!missing(xpath)) {
        xml[xpath]
    } else {
        xml
    }
}
