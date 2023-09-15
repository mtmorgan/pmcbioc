## command-line alternative (faster):
## grep -F --byte-offset --only-matching "<article " pmc_result.xml  |\
##   sed -e 's/:<article '

#' @rdname xml_index
#'
#' @title Index a PubMedCentral XML file for fast record access.
#'
#' @description `xml_index()` creates an index for the XML file
#'     retrieved from PubMedCentral, storing the index in the DuckDB
#'     database. Use `xml_xpath()` to query the index.
#'
#' @param xml_file `character(1)` file path to the PubMedCentral XML
#'     file.
#'
#' @param db a database object returned by `pmcbioc_db()` with
#'     `read_only = FALSE`.
#'
#' @return `xml_index()` returns the `pmcbioc_db` database argument
#'     `db`.
#'
#' @export
xml_index <-
    function(xml_file = "pmc_result.xml", db)
{
    stopifnot(
        is_scalar_character(xml_file),
        file.exists(xml_file),
        inherits(db, "pmcbioc_db"),
        db$is_connected(),
        !db$is_read_only(),
        !"index" %in% db$tables()
    )
    con <- file(xml_file, open = "rb")
    on.exit(close(con))

    buffer_size <- 10000000
    article_tag <- "<article "
    articleset_end_tag <- "</pmc-articleset>\n"

    offset <- 0
    start <- numeric()
    repeat {
        spdl::info("indexed {} articles", length(start))
        spdl::debug("offset {} (start)", offset)
        lines <- readLines(con, buffer_size)
        if (!length(lines))
            break
        idx <- grep(article_tag, lines, fixed = TRUE)
        ## character offset
        line_offsets <- cumsum(c(offset, nchar(lines) + 1))
        batch_start <- line_offsets[idx]
        spdl::debug("first article offset {}", head(batch_start, 1L))
        spdl::debug("last article offset {}", tail(batch_start, 1L))
        start <- c(start, batch_start)
        original_offset <- offset
        offset <- offset + sum(nchar(lines) + 1)
        spdl::debug("batch size {}", sum(nchar(lines) + 1))
        spdl::debug("offset {} (end)", offset)
    }

    ## write to database
    end_last <- file.size(xml_file) - nchar(articleset_end_tag)
    length <- diff(c(start, end_last))
    db$write_index(seq_along(start), start, length)

    ## return
    db
}
