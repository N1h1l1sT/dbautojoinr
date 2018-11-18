
#/Operators\
`%notin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))
is.not.null <- function(x)(!is.null(x))


#/Functions\
ReplaceStartWithSthElse <- function(String, StartsWith, ReplaceWith) {
  Ending <- substr(String, nchar(StartsWith) + 1, nchar(String))
  return(paste0(ReplaceWith, Ending))
}
