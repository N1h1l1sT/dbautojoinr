% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CreateOneJointTable.R
\name{create_extended_joint_table}
\alias{create_extended_joint_table}
\title{Create Extended Main Joint Tables}
\usage{
create_extended_joint_table(
  db_fields,
  db_forced_rel,
  db_ColumnsOldNamesToNewNames,
  con = db$con,
  Verbose = TRUE,
  get_sql_query = TRUE
)
}
\arguments{
\item{db_fields}{A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships}

\item{db_forced_rel}{A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS. e.g. c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")}

\item{db_ColumnsOldNamesToNewNames}{A named List. Names correspond to the Table names, and the vectors inside will be used to renamed SQL Columns starting with db_ColumnsOldNamesToNewNames[i][j] to db_ColumnsOldNamesToNewNames[i][j+1] with j going from 1 to length of db_ColumnsOldNamesToNewNames[i] by 2}

\item{con}{is a dbConnect {DBI} connection object to a SQL Database}

\item{Verbose}{A Boolean. Verbose = TRUE will output the consecutive joins as they happen}

\item{get_sql_query}{A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables}
}
\description{
Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel" WITH or WITHOUT Renames as given by "db_ColumnsOldNamesToNewNames"
}
\examples{
Example:
db_forced_rel <- c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
joint_table_With_extended_joins <-
  create_extended_joint_table(db_fields,
                              db_forced_rel = c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"),
                              db_ColumnsOldNamesToNewNames =
                                list(
                                     DIM_Employee = c(
                                                      c("Site_", "MainSite_")
                                                     )
                                     )
                              )

print(joint_table_Without_extended_joins)
#Renames as given by "db_ColumnsOldNamesToNewNames", create extended joins on main_joint_tables for foreign tables that hold different meaning depending on which table they are joined with, and then everything is joined into 1 table as given by the relationships on "db_forced_rel"
}
\keyword{Join}
\keyword{JointTable}
\keyword{OneJointTable}
\keyword{SQL}
