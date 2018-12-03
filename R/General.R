
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