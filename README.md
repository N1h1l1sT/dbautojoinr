---
title: "dbautojoinr"
output: github_document
---

## Installation

```{r }
if (!("devtools" %in% rownames(installed.packages()))) install.packages("devtools", repos = "https://cloud.r-project.org")
library(devtools)
install_github("N1h1l1sT/dbautojoinr")
```

## Initialisation

Before one can use the automatic join functions, initialisation has to occur so that:
* A connection to the SQL Database is established
* The "db_fields" Dataframe is returned so that the user can select which columns they want to include.
The whole process revolves around the db_fields DF, which can be configured by the user in a user-friendly way using mouse clicks to select SQL fields.

#### Initialisation when the Relationships actually exist on the SQL Database with FOREIGN KEY constraints

```{r }
library(dbautojoinr)

#If the file exists, it's read and the db_fields object is created by it.
#If it doesn't, then it's created with every field included, with potentially IDs and FKs excluded, depending on what you select
db_fields_path <- paste0(getwd(), "/db_fields.csv")

db_fields <- initialise_return_db_fields(csv_path = db_fields_path,
                                         ForceCreate_csv = FALSE,
                                         ExcludeIdentities = FALSE,
                                         ExcludeForeignKeys = TRUE,
                                         Driver = "{SQL Server};",
                                         Database = "DB_Name",
                                         Server = "123.456.78.9",
                                         UID = NULL,
                                         PWD = NULL,
                                         Trusted_Connection = TRUE,
                                         Port = 1433
)
```

#### Initialisation when you want to impose SQL Database relationships on your own

Please notice that Foreign Keys are always on the left hand side, whilst IDs are always on the right hand side.

```{r }
library(dbautojoinr)
db_fields_path <- paste0(getwd(), "/db_fields.csv")

db_fields <- initialise_return_db_fields(csv_path = db_fields_path,
                                         ForceCreate_csv = FALSE,
                                         ExcludeIdentities = FALSE,
                                         ExcludeForeignKeys = TRUE,
                                         Driver = "{SQL Server};",
                                         Database = "DB_Name",
                                         Server = "123.456.78.9",
                                         UID = NULL,
                                         PWD = NULL,
                                         Trusted_Connection = TRUE,
                                         DIM_Employee$Employee_SiteID == DIM_Site$Site_ID,
                                         DIM_Site$Site_RegionID == DIM_Region$Region_ID,
                                         FACT_Hours$Hours_SiteID == DIM_Site$Site_ID,
                                         FACT_Hours$Hours_EmployeeID == DIM_Employee$Employee_ID,
    
```


# Useage

Depending on what you want to achieve, there are different levels of joining that you might want to do.

  * Level 1 (**main_joint_tables**): All tables with foreign keys are joined with all the tables they have a relationship with (unless said tables have all their fields as INCLUDE == FALSE by the user)
  * Level 2 (**joint_table_Without_extended_joins**): Join the tables of main_joint_tables into 1 complete table. This will bring the row level of the 1-Joint-Table to the row level of the 1st table of the LHS declared on db_forced_rel
  * Level 3 (**extended_main_joint_tables**): Some of the Main joint tables might need to be joined by other Main joint tables because 1 table may hold information that has different meaning depending on which table it's joint with (see explanation in more detail below)
  * Level 4 (**joint_table_With_extended_joins**) Join the tables of extended_main_joint_tables into 1 complete table. This will bring the row level of the 1-Joint-Table to the row level of the 1st table of the LHS declared on db_forced_rel

###### _Please note that Initialisation has to have occurred before any code below can be used._

#### Getting the main_joint_tables

```{r }
db_forced_rel <- NULL #We don't want to Force any relationships to create a 1-JointTable, so db_forced_rel is NULL

main_joint_tables <-
  CreateMainJointTables(db_fields = db_fields,
                        db_forced_rel = db_forced_rel,
                        DeselectKeysIfIncludeFalse = TRUE,
                        con = db$con,
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        )

```
Arguments:
  * db_fields: A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
  * db_forced_rel: A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS
  * DeselectKeysIfIncludeFalse: A Boolean. Must be FALSE if we need to continue to 1-Joint-Table, otherwise needed Identity and Foreign keys might be missing
  * con: A dbConnect {DBI} connection object to a SQL Database
  * Verbose: A Boolean. Verbose = TRUE will output the consecutive joins as they happen
  * get_sql_query: A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables

#### Getting the joint_table_Without_extended_joins

###### _From hereinafter we need to have configured the db_forced_rel variable with the forced relationships that we want to impose in order to join the Main tables into 1 table_

```{r }
#Assumptions: Database is in Canonical Form, No two columns have the same name (Usual good practice in Databases)
db_forced_rel <-
  c(                #The LHS of the Relationships MUST be Columns from the main table to be used for the 1-Joint-Table
    Hours_SiteID = "Site_ID",
    Hours_EmployeeID = "Employee_ID"
  )

joint_table_Without_extended_joins <-
  CreateMainJointTables(db_fields = db_fields,
                        db_forced_rel = db_forced_rel,
                        DeselectKeysIfIncludeFalse = FALSE, #False in this case because we're going to need the keys for the extended join
                        con = db$con,
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        ) %>%
  CreateOneJointTable(db_fields = db_fields,
                      con = db$con,
                      db_forced_rel = db_forced_rel,
                      Verbose = TRUE,
                      get_sql_query = TRUE
                      )

```


#### Getting the extended_main_joint_tables

###### _From hereinafter we need to have configured the db_TablesForColumnRenaming and db_ColumnsOldNamesToNewNames variables with the renaming schema so that when the same table is joined with different Main tables, the column names change to reflect the different meaning_

```{r }
#Assumptions: Database is in Canonical Form, No two columns have the same name (Usual good practice in Databases)
db_forced_rel <-
  c(                #The LHS of the Relationships MUST be Columns from the main table to be used for the 1-Joint-Table
    Hours_SiteID = "Site_ID",
    Hours_EmployeeID = "Employee_ID"
  )

#DIM_Site will be joined with DIM_Employee, but also with FACT_Hours.
#An employee will work on a certain site each day, which might be different from day to day,
#but the original site he is assigned to will always remain the same - his Main Site.
#DIM_Site holds the Site information, so when it's joined with DIM_Employee, its meaning is the employees Main Site
#However, when it's joined with FACT_Hours, its meaning is the site in which the employee has worked on that particular day.
#If we are to create a 1-Joint-Table, then the SQL columns cannot have the same name. So we're renaming the columns that
#that come from the DIM_Site table and joined with DIM_Employee into MainSite_[SomeName] instead of Site_[SomeName]
#On the final table (1-Joint-Table) MainSite_ID column will refer to the Site that the employee is assigned to, and Site_ID will refer to the one which he worked that particular day
db_TablesForColumnRenaming <- c("DIM_Employee")

db_ColumnsOldNamesToNewNames <-
  list(
    DIM_Employee = c(
      c("Site_", "MainSite_")
    )
  )

joint_table_Without_extended_joins <-
  CreateMainJointTables(db_fields = db_fields,
                        db_forced_rel = db_forced_rel,
                        DeselectKeysIfIncludeFalse = FALSE, #False in this case because we're going to need the keys for the extended join
                        con = db$con,
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        ) %>%
  CreateExtendedMainJointTables(db_fields = db_fields,
                                con = db$con,
                                db_forced_rel = db_forced_rel,
                                db_TablesForColumnRenaming = db_TablesForColumnRenaming,
                                db_ColumnsOldNamesToNewNames = db_ColumnsOldNamesToNewNames,
                                Verbose = TRUE,
                                get_sql_query = FALSE
                                )

```
Arguments:
  * db_TablesForColumnRenaming A string Vector. The names of the tables that need renaming
  * db_ColumnsOldNamesToNewNames A names List. Names correspond to the Table names, and the vectors inside will be used to renamed SQL Columns starting with db_ColumnsOldNamesToNewNames[i][j] to db_ColumnsOldNamesToNewNames[i][j+1] with j going from 1 to length of db_ColumnsOldNamesToNewNames[i] by 2

