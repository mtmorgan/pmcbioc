#' @importFrom duckdb duckdb dbConnect dbDisconnect dbWriteTable
#'     dbListTables
#'
#' @importFrom dplyr tbl
#'
#' @export
db_connect <- function(dbdir, read_only = TRUE) {
    ## validate
    if (!file.exists(dbdir)) {
        spdl::debug("creating '{}'", dbdir)
        read_only <- FALSE
    } else {
        spdl::debug("using '{}'", dbdir)
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

    path <- function()
        dbdir

    is_connected <- function()
        !is.null(connection)

    is_read_only <- function()
        read_only

    connect <- function() {
        if (is.null(connection)) {
            spdl::info(
                "connecting to '{}' read_only: {}",
                dbdir, read_only
            )
            n <<- 0L
            buffer_reset()
            connection <<- dbConnect(duckdb(
                dbdir = dbdir,
                read_only = read_only
            ))
        }
        invisible(connection)
    }

    disconnect <- function() {
        if (is.null(connection))
            return(invisible(NULL))
        spdl::info("disconnect from {} with buffer size {}", dbdir, n)
        dbDisconnect(connect(), shutdown = TRUE)
        ## reset
        n <<- 0L
        buffer_reset()
        connection <<- NULL
        invisible(connection)
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
            disconnect = disconnect,
            is_connected = is_connected,
            is_read_only = is_read_only,
            path = path,
            write_metadata = write_metadata,
            flush_metadata = flush_metadata,
            write_index = write_index,
            tables = function() dbListTables(connect()),
            tbl = function(src, ...) dplyr::tbl(connect(), src, ...)
        ),
        class = "PMCBioc_db"
    )
}

#' @export
print.PMCBioc_db <-
    function(x, ...)
{        
    is_connected <- x$is_connected()
    cat(
        "PMCBioc_db: ", x$path(), "\n",
        "is_connected: ", is_connected, "\n",
        "is_read_only: ", x$is_read_only(), "\n",
        if (is_connected)
            paste0("tables: ", paste(x$tables(), collapse = ", "), "\n"),
        sep = ""
    )
}
