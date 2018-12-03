# dbautojoinr

# Tutorial

## Installation

```r
if (!("devtools" %in% rownames(installed.packages()))) install.packages("devtools", repos = "https://cloud.r-project.org")
library(devtools)
if (!("dbautojoinr" %in% rownames(installed.packages()))) install_github("N1h1l1sT/dbautojoinr", upgrade = FALSE)
library(dbautojoinr)
```


___
#### Note:
###### _There's a working demo playing out a use-case, complete with the SQL Database and R code used to auto-join the SQL Database. You can either skip to the written demo below, or click **this link** to watch the demo on YouTube, or continue reading below for the how-to-use general tutorial_
___


## Initialisation

Before one can use the automatic join functions, initialisation has to occur so that:
* A connection to the SQL Database is established
* The "db_fields" Dataframe is returned so that the user can select which columns they want to include.
The whole process revolves around the db_fields DF, which can be configured by the user in a user-friendly way using mouse clicks to select SQL fields.

```r
library(dbautojoinr)

#If the file exists, it's read and the db_fields object is created by it.
#If it doesn't, then it's created with every field included, with potentially IDs and FKs excluded, depending on what you select
db_fields_path <- paste0(getwd(), "/db_fields.csv")

db_fields <- initialise_return_db_fields(csv_path = db_fields_path,
                                         Driver = "{SQL Server};",
                                         Database = "DB_Name",
                                         Server = "123.456.78.9",
                                         UID = NULL,
                                         PWD = NULL,
                                         Trusted_Connection = TRUE#,
                                         #Table1$T1_ForeignKey1 == Table2$T2_ID, #Please notice that Foreign Keys are always
                                         #Table2$T2_ForeignKey1 == Table3$T3_ID, #on the left hand side, whilst IDs are always
                                         #Table4$T4_ForeignKey1 == Table2$T2_ID, #on the right hand side.
                                         #Table4$T4_ForeignKey2 == Table1$T1_ID
#If you haven't set the SQL Relationships on the Database, you can impose them here by
#uncommenting the lines above and replacing the text with the actual tables and columns
    
```

If the db_fields .csv file doesn't exist and it's created or if you set `ForceCreate_csv = TRUE`, then you may also initialise the db_fields with IDs and FKs selected or excluded using `ExcludeIdentities = FALSE` and `ExcludeForeignKeys = TRUE`. 

## Select which SQL Columns you need

```r
db_fields <- edit_db_fields(db_fields) #What the user selected is now saved on the db_fields variable (AND on db$db_fields).
write_db_fields_csv(db_fields, db_fields_path) #For any run on the current session, the user preferences are assumed, but we need to save the file for future runs.
```

# Useage

Depending on what you want to achieve, there are different levels of joining that you might want to do.

  * Level 1 (**main_joint_tables**): All tables with foreign keys are joined with all the tables they have a relationship with (unless said tables have all their fields as INCLUDE == FALSE by the user)
  * Level 2 (**joint_table_Without_extended_joins**): Join the tables of main_joint_tables into 1 complete table. This will bring the row level of the 1-Joint-Table to the row level of the 1st table of the LHS declared on db_forced_rel
  * Level 3 (**extended_main_joint_tables**): Some of the Main joint tables might need to be joined by other Main joint tables because a table may hold information that has different meaning depending on which table it's joint with (see explanation in more detail below at the comments on 'Getting the extended_main_joint_tables' section)
  * Level 4 (**joint_table_With_extended_joins**) Join the tables of extended_main_joint_tables into 1 complete table. This will bring the row level of the 1-Joint-Table to the row level of the 1st table of the LHS declared on db_forced_rel

###### _Please note that Initialisation has to have occurred before any code below can be used._

#### Getting the main_joint_tables

```r
main_joint_tables <-
  CreateMainJointTables(db_fields = db_fields,
                        db_forced_rel = NULL, #We don't want to Force any relationships to create a 1-JointTable, so db_forced_rel is NULL
                        con = db$con,
                        DeselectKeysIfIncludeFalse = TRUE
                        )

```
New Arguments:
  * **db_fields**: A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
  * **db_forced_rel**: A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS
  * **con**: A dbConnect {DBI} connection object to a SQL Database
  * **DeselectKeysIfIncludeFalse**: A Boolean. Must be FALSE if we need to continue to 1-Joint-Table, otherwise needed Identity and Foreign keys might be missing
  * **Verbose**: A Boolean. Verbose = TRUE will output the consecutive joins as they happen
  * **get_sql_query**: A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables

#### Getting the joint_table_Without_extended_joins

###### _From hereinafter we need to have configured the db_forced_rel variable with the forced relationships that we want to impose in order to join the Main tables into 1 table_

```r
#Assumptions: Database is in Canonical Form, No two columns have the same name (Usual good practice in Databases)
db_forced_rel <-
  c(                #The LHS of the Relationships MUST be Columns from the main table to be used for the 1-Joint-Table
    Hours_SiteID = "Site_ID",
    Hours_EmployeeID = "Employee_ID"
  )

joint_table_Without_extended_joins <-
  created_joint_table(db_fields = db_fields,
                      db_forced_rel = db_forced_rel)

```


#### Getting the extended_main_joint_tables

###### _From hereinafter we need to have configured the db_ColumnsOldNamesToNewNames variable with the renaming schema so that when the same table is joined with different Main tables, the column names change to reflect the different meaning_

```r
#DIM_Site will be joined with DIM_Employee, but also with FACT_Hours.
#An employee will work on a certain site each day, which might be different from day to day,
#but the original site he is assigned to will always remain the same - his Main Site.
#DIM_Site holds the Site information, so when it's joined with DIM_Employee, its meaning is the employees Main Site
#However, when it's joined with FACT_Hours, its meaning is the site in which the employee has worked on at that particular day.
#If we are to create a 1-Joint-Table, then the SQL columns cannot have the same name. So we're renaming the columns that
#that come from the DIM_Site table and joined with DIM_Employee into MainSite_[SomeName] instead of Site_[SomeName]
#On the final table (1-Joint-Table) MainSite_ID column will refer to the Site that the employee is assigned to, and Site_ID will refer to the one which he worked that particular day

db_ColumnsOldNamesToNewNames <-
  list(
    DIM_Employee = c(
      c("Site_", "MainSite_")
    )
  )

extended_main_joint_tables <-
  create_extended_main_joint_tables(db_fields = db_fields,
                                    db_forced_rel = db_forced_rel,
                                    db_ColumnsOldNamesToNewNames = db_ColumnsOldNamesToNewNames
                                    )
```
New Arguments:
  * **db_ColumnsOldNamesToNewNames**: A named List. Names correspond to the Table names, and the vectors inside will be used to renamed SQL Columns starting with `db_ColumnsOldNamesToNewNames[i][j]` to `db_ColumnsOldNamesToNewNames[i][j+1]` with j going from 1 to length of `db_ColumnsOldNamesToNewNames[i]` by 2


#### Getting the joint_table_With_extended_joins

```r
joint_table_With_extended_joins <-
  create_extended_joint_table(db_fields = db_fields,
                              db_forced_rel = db_forced_rel,
                              db_ColumnsOldNamesToNewNames = db_ColumnsOldNamesToNewNames
                              )

```

#### **And that is all! You've now officially gotten all the different possible levels of joining. Hurray!**

# Demo

#### 1. Create and populate the Database
Copy the SQL code under the ["SQL folder"](https://github.com/N1h1l1sT/dbautojoinr/tree/master/SQL) of the package into SSMS and execute the Query.
First execute _Database Creation.sql_ and then execute _Data Population.sql_.
Now you have a "dbautojoinr" SQL Database on your SQL Server with 4 tables (DIM_Employee, DIM_Region, DIM_Site, and FACT_Hours) and with data populated on those tables.
The SQL Relationships also already exist on your SQL Database, so you won't need to explicitly impose them on the Initialisation code.

![Populated SQL Database](https://github.com/N1h1l1sT/dbautojoinr/blob/master/SQL/dbautojoinr-sql-database.png?raw=true "Populated SQL Database")


#### 2. Install and Initialise dbautojoinr

```r
if (!("devtools" %in% rownames(installed.packages()))) install.packages("devtools", repos = "https://cloud.r-project.org")
library(devtools)
if (!("dbautojoinr" %in% rownames(installed.packages()))) install_github("N1h1l1sT/dbautojoinr", upgrade = FALSE)
library(dbautojoinr)

db_fields_path <- paste0(getwd(), "/db_fields.csv")

db_fields <- initialise_return_db_fields(csv_path = db_fields_path,
                                         ForceCreate_csv = FALSE,
                                         ExcludeIdentities = FALSE,
                                         ExcludeForeignKeys = TRUE,
                                         Driver = "{SQL Server};",
                                         Database = "dbautojoinr",
                                         Server = "Put your own Server IP/Name here",
                                         UID = NULL,
                                         PWD = NULL,
                                         Trusted_Connection = TRUE,
                                         Port = 1433
)


show_ER_diagramme(db$dm_f) #Shows the SQL Database ER Diagramme

```

![dbautojoinr ER Diagramme](https://github.com/N1h1l1sT/dbautojoinr/blob/master/SQL/dbautojoinr-er-diagramme.png?raw=true "dbautojoinr ER Diagramme")


#### 3. Select only the SQL Columns that you want to view


```r
db_fields <- edit_db_fields(db_fields)
write_db_fields_csv(db_fields, db_fields_path)

```

![Feature Selection](https://github.com/N1h1l1sT/dbautojoinr/blob/master/SQL/edit_db_fields_feature_selection.png?raw=true "Feature Selection")

#### 4. Set needed parameters

* db_forced_rel is only needed if we don't just want the Main Tables joined, but we want to end up with just 1 table with everything else joined.
Otherwise this parameter can be null `db_forced_rel <- NULL`.
```r
db_forced_rel <-
  c(
    Hours_SiteID = "Site_ID",
    Hours_EmployeeID = "Employee_ID"
  )
```


* db_ColumnsOldNamesToNewNames is only needed if a certain table is to be joined to more than 1 table, as it is the case with DIM_Site which will be joined with DIM_Employee & FACT_Hours.

```r
db_ColumnsOldNamesToNewNames <-
  list(
    DIM_Employee = c(
      c("Site_", "MainSite_")
    )
  )
```

#### 5. Get the Main Tables

```r
main_joint_tables <-
  CreateMainJointTables(db_fields,
                        db_forced_rel,
                        db$con,
                        DeselectKeysIfIncludeFalse = TRUE, #No need to make any other joins, so let's only get what the User selected
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        )
```

#### 6. Get the Extended Main Tables
```r
extended_main_joint_tables <-
  CreateMainJointTables(db_fields,
                        db_forced_rel,
                        db$con,
                        DeselectKeysIfIncludeFalse = FALSE,
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        ) %>%
  CreateExtendedMainJointTables(db_fields,
                                db_forced_rel,
                                db_ColumnsOldNamesToNewNames,
                                db$con,
                                DeselectKeysIfIncludeFalse = TRUE,
                                Verbose = TRUE,
                                get_sql_query = FALSE
                                )
```

You've probably noticed that instead of using `create_extended_main_joint_tables` to get the result, we're now using 2 different functions that each performs 1 step.
The 1st one (`CreateMainJointTables`) will retrieve the Main Tables, whilst the 2nd (`CreateExtendedMainJointTables`) does the extended joins (in our case, it joins `main_joint_tables[[DIM_Employee]].[MainSite_ID]` with `[DIM_Site].[Site_ID]` )
Now, you might want to proceed with this long way if you want to also make custom transformations to some table before the next joining level occurs.
**Be careful with DeselectKeysIfIncludeFalse which must always be FALSE prior to the last level and always TRUE at the last one.** 


#### 7. Get the 1-Joint-Table (No renaming or extended joins)
```r
joint_table_Without_extended_joins <-
  CreateMainJointTables(db_fields,
                        db_forced_rel,
                        db$con,
                        DeselectKeysIfIncludeFalse = FALSE,
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        ) %>%
  CreateOneJointTable(db_fields,
                      db_forced_rel,
                      db$con,
                      Verbose = TRUE,
                      get_sql_query = FALSE
                      )
```

#### 8. Get the 1-Joint-Table with Extended Joins
```r
joint_table_With_extended_joins <-
  CreateMainJointTables(db_fields,
                        db_forced_rel,
                        db$con,
                        DeselectKeysIfIncludeFalse = FALSE,
                        Verbose = TRUE,
                        get_sql_query = FALSE
                        ) %>%
  CreateExtendedMainJointTables(db_fields,
                                db_forced_rel,
                                db_ColumnsOldNamesToNewNames,
                                db$con,
                                DeselectKeysIfIncludeFalse = FALSE,
                                Verbose = TRUE,
                                get_sql_query = FALSE
                                ) %>%
  CreateOneJointTable(db_fields,
                      db_forced_rel,
                      db$con,
                      Verbose = TRUE,
                      get_sql_query = FALSE
                      )
```

#### 9. Looking at the results

```r
sapply(names(main_joint_tables), function(x) NCOL(main_joint_tables[[x]]))
# Number of Columns on the tables of main_joint_tables
#    DIM_Site DIM_Employee   FACT_Hours 
#           2            4            6

sapply(names(extended_main_joint_tables), function(x) NCOL(extended_main_joint_tables[[x]]))
# Number of Columns on the tables of extended_main_joint_tables
#    DIM_Site DIM_Employee   FACT_Hours 
#           2            5            6 

print(c(joint_table = NCOL(joint_table_Without_extended_joins), joint_table_extended = NCOL(joint_table_With_extended_joins)))
# Number of Columns on the 2 Fully Joint Tables, withand without extended joins
#    joint_table joint_table_extended 
#           7            9 

```


#### **End of Demo**
