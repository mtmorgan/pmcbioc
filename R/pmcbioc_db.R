#' @importFrom duckdb duckdb dbConnect dbDisconnect dbWriteTable
#'
#' @importFrom dplyr tbl
#'
#' @export
pmcbioc_db <- function(db_dir, read_only = TRUE) {
    ## validate
    if (!file.exists(db_dir)) {
        spdl::debug("creating '{}'", db_dir)
        read_only <- FALSE
    } else {
        spdl::debug("using '{}'", db_dir)
        force(read_only)
    }

    ## initialize
    connection <- NULL
    n <- 0L
    ## article table
    article_internal_id <- integer()
    article_pmcid <- character()
    article_title <- character()
    article_journal <- character()
    article_year <- integer()
    article_pmid <- character()
    ## author table
    author_pmcid <- character()
    author_surname <- character()
    author_givenname <- character()
    ## keyword table
    keyword_pmcid <- character()
    keyword_keyword <- character()
    ## refpmid table
    refpmid_pmcid <- character()
    refpmid_refpmid <- character()

    is_connected <- function()
        !is.null(connection)

    connect <- function() {
        if (is.null(connection)) {
            spdl::info(
                "connecting to '{}' read_only: {}",
                db_dir, read_only
            )
            n <<- 0L
            buffer_reset()
            connection <<- dbConnect(duckdb(
                dbdir = db_dir,
                read_only = read_only
            ))
        }
        invisible(connection)
    }

    reset <- function() {
        spdl::debug("resetting connection buffer with size {}", n)
        ## reset
        n <<- 0L
        buffer_reset()
        connection <<- NULL
    }

    write_metadata <- function(
        pmcid, title, journal, year, pmid,
        surname, givenname,
        keyword,
        refpmid
    ) {
        stopifnot(
            is.character(surname),
            is.character(givenname)
        )
        ## article
        i <- length(article_pmcid) + 1L
        article_internal_id[i] <<- n + 1L
        article_pmcid[i] <<- pmcid
        article_title[i] <<- title
        article_journal[i] <<- journal
        article_year[i] <<- year
        article_pmid[i] <<- pmid
        ## author
        i <- length(author_pmcid) + seq_along(surname)
        author_pmcid[i] <<- pmcid
        author_surname[i] <<- surname
        author_givenname[i] <<- givenname
        ## keyword
        i <- length(keyword_pmcid) + seq_along(keyword)
        keyword_pmcid[i] <<- pmcid
        keyword_keyword[i] <<- keyword
        ## refpmid
        i <- length(refpmid_pmcid) + seq_along(refpmid)
        refpmid_pmcid[i] <<- pmcid
        refpmid_refpmid[i] <<- refpmid
        n <<- n + 1L
        spdl::debug("write_metadata {}", n)
        ## flush?
        if (n %% 100L == 0L)
            flush_metadata()
    }

    flush_metadata <- function() {
        spdl::debug("flush_metadata() {}", n)
        con <- connect()
        ## article
        article <- data.frame(
            id = article_internal_id,
            pmcid = article_pmcid,
            title = article_title,
            journal = article_journal,
            year = article_year,
            pmid = article_pmid
        )
        dbWriteTable(con, "article", article, append = TRUE)
        ## author
        author <- data.frame(
            pmcid = author_pmcid,
            surname = author_surname,
            givenname = author_givenname
        )
        dbWriteTable(con, "author", author, append = TRUE)
        ## keywords
        keyword <- data.frame(
            pmcid = keyword_pmcid,
            keyword = keyword_keyword
        )
        dbWriteTable(con, "keyword", keyword, append = TRUE)
        ## refpmid
        refpmid <- data.frame(
            pmcid = refpmid_pmcid,
            refpmid = refpmid_refpmid
        )
        dbWriteTable(con, "refpmid", refpmid, append = TRUE)
        ## buffer reset
        buffer_reset()
        invisible(con)
    }

    buffer_reset <- function() {
        spdl::debug("buffer_reset()")
        ## article table
        article_internal_id <<- integer()
        article_pmcid <<- character()
        article_title <<- character()
        article_journal <<- character()
        article_year <<- integer()
        article_pmid <<- character()
        ## author table
        author_pmcid <<- character()
        author_surname <<- character()
        author_givenname <<- character()
        ## keyword table
        keyword_pmcid <<- character()
        keyword_keyword <<- character()
        ## ref_pmid table
        refpmid_pmcid <<- character()
        refpmid_refpmid <<- character()
    }

    write_index <- function(internal_id, start, length) {
        spdl::debug("write_index() {}", length(internal_id))
        con <- connect()
        ## article
        index <- data.frame(
            id = internal_id,
            start = start,
            length = length
        )
        dbWriteTable(con, "index", index)
    }

    connect()
    structure(
        list(
            connect = connect,
            reset = reset,
            is_connected = is_connected,
            read_only = read_only,
            db_dir = db_dir,
            write_metadata = write_metadata,
            flush_metadata = flush_metadata,
            write_index = write_index
        ),
        class = "pmcbioc_db"
    )
}

#' @export
print.pmcbioc_db <-
    function(x, ...)
{
    is_connected <- x$is_connected()
    cat(
        "pmcbioc_db: ", db_dir(x), "\n",
        "connected: ", is_connected, "\n",
        "read_only: ", x$read_only, "\n",
        if (is_connected)
            paste0("db_tables(): ", paste(db_tables(x), collapse = ", "), "\n"),
        sep = ""
    )
}

#' @export
db_disconnect <-
    function(db)
{
    if (!db$is_connected())
        return(db)
    spdl::info("disconnecting from {}", db_dir(db))
    dbDisconnect(db$connect(), shutdown = TRUE)
    db$reset()
    db
}

#' @export
db_dir <-
    function(db)
{
    db$db_dir
}

#' @importFrom duckdb dbListTables
#'
#' @export
db_tables <-
    function(db)
{
    db$connect() |>
        dbListTables()
}

#' @importFrom dplyr tbl
#'
#' @export
tbl.pmcbioc_db <-
    function(src, from, ...)
{
    tbl(src$connect(), from, ...)
}
