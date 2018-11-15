.onLoad <- function(libname, pkgname)
{
  # assign("db", new.env(), parent.env())
  .GlobalEnv$db <- new.env()
}