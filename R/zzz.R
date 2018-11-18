.onLoad <- function(libname, pkgname)
{
  # assign("db", new.env(), environment())
  # print(environment())
  # .GlobalEnv$db <- new.env()
  if (!("datamodelr" %in% rownames(installed.packages()))) devtools::install_github("bergant/datamodelr")
}
