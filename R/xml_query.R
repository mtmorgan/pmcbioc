## command-line alternative:
## grep -F --byte-offset --only-matching "<article " pmc_result.xml  |\
##   sed -e 's/:<article '
#' @export
xml_index <-
    function(xml_file = "pmc_result.xml", db)
{
    stopifnot(
        is_scalar_character(xml_file),
        file.exists(xml_file),
        inherits(db, "PMCBioc_db"),
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

#' @importFrom dplyr pull
#'
#' @importFrom XML xmlParse
#'
#' @export
xml_query <-
    function(.data, xml_file = "pmc_result.xml")
{
    stopifnot(
        "start" %in% names(.data),
        "length" %in% names(.data),
        file.exists(xml_file)
    )

    con <- file(xml_file, open = "rb")
    on.exit(close(con))

    articles <- Map(function(start, length) {
        seek(con, start)
        readChar(con, length)
    }, pull(.data, "start"), pull(.data, "length"))

    articleset <- paste0(
        "<pmc_articleset>\n",
        paste(articles, collapse = "\n"),
        "</pmc_articleset>\n"
    )
    xmlParse(articleset)
}
