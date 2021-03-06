% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_db_info.R
\name{zinternal_connect_odbc}
\alias{zinternal_connect_odbc}
\title{Creates an ODBC Connection}
\usage{
zinternal_connect_odbc(
  Driver,
  Database,
  Server,
  UID,
  PWD,
  Trusted_Connection,
  Port = 1433
)
}
\arguments{
\item{Driver}{A string. A SQL Driver for SQL Server, Postgre, etc., e.g.: "{SQL Server};"}

\item{Database}{A string. The name of the SQL Database itself}

\item{Server}{A string. A name that resolves into the SQL Machine, or the IP to the SQL Machine}

\item{UID}{A string. The Username credential for the SQL Connection. NULL if Windows Login is to be used}

\item{PWD}{A string. The Password credential for the SQL Connection. NULL if Windows Login is to be used}

\item{Trusted_Connection}{A Boolean. TRUE or FALSE on whether Windows Login/Credentials are to be used instead of a UID/PWD, if a string is used it'll be passed as is.}

\item{Port}{An Integer. The Port which the SQL Listener is listening at. Can be left blank whereupon it will default to 1433}
}
\description{
Creates an ODBC Connection
}
\examples{
zinternal_connect_odbc(Driver = "{SQL Server};", Database = "Chronogramme", Server = '123.456.7.8', UID = NULL, PWD = NULL, Trusted_Connection = TRUE, Port = 1433)
}
\keyword{Connection}
\keyword{ODBC}
\keyword{SQL}
