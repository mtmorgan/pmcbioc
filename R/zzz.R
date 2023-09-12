is_scalar_character <-
    function(x)
{
    is.character(x) && length(x) == 1L && !is.na(x)
}

is_scalar_logical <-
    function(x)
{
    is.logical(x) && length(x) == 1L && !is.na(x)
}

.onLoad <-
    function(...)
{
    spdl::set_pattern("[%l %H:%M:%S] %v")
}
