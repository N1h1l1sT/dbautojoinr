% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateOneJointTable.R
\name{create_joint_table}
\alias{create_joint_table}
\title{Create Main Joint Tables}
\usage{
create_joint_table(
  db_fields,
  db_forced_rel,
  con = db$con,
  Verbose = TRUE,
  get_sql_query = TRUE
)
}
\arguments{
\item{db_fields}{A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships}

\item{db_forced_rel}{A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS. e.g. c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")}

\item{con}{is a dbConnect {DBI} connection object to a SQL Database}

\item{Verbose}{A Boolean. Verbose = TRUE will output the consecutive joins as they happen}

\item{get_sql_query}{A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables}
}
\description{
Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel" WITHOUT Renames
}
\examples{
Example:
db_forced_rel <- c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
joint_table_Without_extended_joins <-
  create_joint_table(db_fields,
                      db_forced_rel = c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"),
                      )

print(joint_table_Without_extended_joins)
#No renames are used and therefore no extended joins.
#This assumes that all tables can be joined together without any 1 table being needed twice.
}
\keyword{Join}
\keyword{JointTable}
\keyword{OneJointTable}
\keyword{SQL}
