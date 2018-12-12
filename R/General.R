
#/Operators\
`%notin%` <- function(x, table) is.na(match(x, table, nomatch = NA_integer_))
is.not.null <- function(x)(!is.null(x))


#/Functions\
ReplaceStartWithSthElse <- function(String, StartsWith, ReplaceWith) {
  Ending <- substr(String, nchar(StartsWith) + 1, nchar(String))
  return(paste0(ReplaceWith, Ending))
}

#Just the gsub, but easier to remember
#[input: String, String, string] [output: String]
ReplaceStringWithStringRegEx <- function(Text, RegExStatement, ReplaceWith) {
  gsub(RegExStatement, ReplaceWith, Text, perl = TRUE)
}

#Just the gsub, but easier to remember
#[input: String, String, string] [output: String]
ReplaceStringWithString <- function(Text, TextToSearchFor, ReplaceWith) {
  ReplaceStringWithStringRegEx(Text, paste0("[", RegexEscapeForCharClass(TextToSearchFor), "]"), ReplaceWith)
}

inspectVar <- function(x, BasicOnly = TRUE) {
  #deparse(substitute(x))
  print(data.frame(
    Typeof = implode(typeof(x), sep = ","),
    Class = implode(class(x), sep = ","),
    NRow = implode(NROW(x), sep = ","),
    NCol = implode(NCOL(x), sep = ","),
    Dim = ifelse(is.not.null(dim(x)), paste(dim(x), sep = ",", collapse = ","), "NULL")
  )
  )
  if (!BasicOnly) {
    print("structure:")
    print(str(x))
    print("summary:")
    print(summary(x))
  }

}

#' as.data_model.data.frame() is taken from bergant's datamodelr v0.2.1. Unfortunately it's not publically exposed and I need it so I'm importing it. All credit to bergant for this
#' Coerce a data frame to a data model
#'
#' @export
as.data_model.data.frame <- function(x) {

  if(!inherits(x, "data.frame")) stop("Not a data.frame")

  if(!all(c("column", "table") %in% names(x)))
  {
    stop("Data frame must have elements named 'table' and 'column'.")
  }

  # set key to 0 if NA or add key if NULL:
  if(!is.null(x[["key"]])) {
    x[is.na(x[,"key"]), "key"] <- FALSE
  } else {
    x[,"key"] <- FALSE
  }

  # convert logical key markers to numeric (column order in a key)
  # x$table <- factor(x$table, ordered = TRUE)
  # if(max(x$key, na.rm = TRUE) <= 1) {
  #   keys <-
  #     lapply(split(x, x$table), function(t) {
  #       cumsum(t$key) * t$key
  #     })
  #   x$key <- unlist(keys)
  # }

  if(is.null(x[["ref"]])) x[["ref"]] <- NA


  # create references from ref and keys
  ref_table <- datamodelr::dm_create_references(x)

  table_attrs <- attr(x, "tables")
  if(is.null(table_attrs)) {
    table_attrs <-
      data.frame(
        table = unique(x[["table"]]),
        segment = NA,
        display = NA,
        row.names = NULL,
        stringsAsFactors = FALSE
      )
  }
  attr(x, "tables") <- NULL
  ret <- list(
    tables = table_attrs,
    columns = x,
    references = ref_table
  )
  datamodelr::as.data_model(ret)
}