# #Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel"
# #No renames and therefore no extended joins. This assumes that all tables can be joined together without any 1 table being needed twice
# joint_table_Without_extended_joins <-
#   CreateMainJointTables(db_fields, db_forced_rel, FALSE, db$con) %>%
#   CreateOneJointTable(db_fields, db$con)
# #Yields a Table
#
# #Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel"
# #Renames as given by "db_ColumnsOldNamesToNewNames", extended joins tables that hold different meaning depending on which table
# #they are joined with, and then joins everything into 1 table as given by the relationships on "db_forced_rel"
# joint_table_With_extended_joins <-
#   CreateMainJointTables(db_fields, db_forced_rel, FALSE, db$con) %>%
#   CreateExtendedMainJointTables(db_fields, db$con, c("DIM_Employee"), list(DIM_Employee = c(c("Site_", "MainSite_")))) %>%
#   CreateOneJointTable(db_fields, db$con)
# #Yields a Table

########################################################
### Getting the 1-joint-table by joining main tables ###
########################################################
#!!CAUTION!!: If the unconventional relationships are not set correctly, OR wholly, OR possibly with a specific order, the joins may fail
#The assumption is there's an order to this so that each consequent index can join with the first one after
#the first one itself has been subject to all previous joins until each implied join up-until the current index
#The order is set on Database.R::update_db_info::db_forced_rel and the initial table there will be the table used (Left) to join with everything else

### !Remember to put rename conventions on db_ColumnsOldNamesToNewNames to rename Columns in main_joint_tables that have the same name
###  between tables, but hold different information.
########################################################################################################
### For example, if an employee is employed by a certain Building (his Main Building) but also works ###
### on others too. So DIM_Employee will have a BuildingID AND FACT_Work will have a BuildingID too.  ###
### BOTH DIM_Employee & FACT_Work will be joined with DIM_Building (which BuildingID refers to), but ###
### the former will refer to the Building which the worker is assigned to, whilst the latter refers  ###
### to the building which he worked on at that period of time. It can be different to the first.     ###
### This MUST be renamed otherwise .x & .y columns will be created and the former info will be lost  ###
########################################################################################################

### When to use ###
#If all or some of the tables included in main_joint_tables can be joined in a logical order that can be defined
#as simply as main_joint_tables[[TabX]].[ColXn] = main_joint_tables[[TabY]].[ColYn].
#In that case, declare the relationship by ColNames only on db_forced_rel (Cols only as they are assumed to be unique)

#' Create Extended Main Joint Tables
#'
#' Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel" WITH or WITHOUT Renames as given by "db_ColumnsOldNamesToNewNames"
#' @param main_joint_tables A named list of tibbles/DFs (usually given by CreateMainJointTables() as a SQL DB Pointer containing all user-selected fields plus needed ones for joins
#' @param db_fields A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
#' @param con is a dbConnect {DBI} connection object to a SQL Database
#' @param db_forced_rel A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS. e.g. c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#' @param Verbose A Boolean. Verbose = TRUE will output the consecutive joins as they happen
#' @param get_sql_query A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables
#' @keywords SQL Join JointTable OneJointTable
#' @export
#' @examples
#' db_forced_rel <- c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#' Example 1:
#' joint_table_Without_extended_joins <-
#'   CreateMainJointTables(db_fields, db_forced_rel, FALSE, db$con) %>%
#'   CreateOneJointTable(db_fields, db$con, db_forced_rel)
#'
#' print(joint_table_Without_extended_joins)
#' #No renames are used and therefore no extended joins.
#' #This assumes that all tables can be joined together without any 1 table being needed twice.
#'
#'
#' Example 2:
#' joint_table_With_extended_joins <-
#'   CreateMainJointTables(db_fields, db_forced_rel, FALSE, db$con) %>%
#'   CreateExtendedMainJointTables(db_fields, db$con, c("DIM_Employee"), db_forced_rel) %>%
#'   CreateOneJointTable(db_fields, db$con, db_forced_rel)
#'
#' print(joint_table_Without_extended_joins)
#' #Renames as given by "db_ColumnsOldNamesToNewNames", create extended joins on main_joint_tables for foreign tables that hold different meaning depending on which table they are joined with, and then everything is joined into 1 table as given by the relationships on "db_forced_rel"
CreateOneJointTable <- function(main_joint_tables, db_fields, db_forced_rel, con = db$con, Verbose = TRUE, get_sql_query = TRUE) {
  ColsFromDbFields <-
    db_fields %>%
    filter(Include == "Yes") %>%
    pull(Column)

  # ForcedTable <- names(main_joint_tables)
  if (length(db_forced_rel) > 0) {
    NewTabNames <- NULL
    NewColNames <- NULL
    for (TabName in names(main_joint_tables)) {
      CurTabColNames <- colnames(main_joint_tables[[TabName]])
      NewColNames <- c(NewColNames, CurTabColNames)
      NewTabNames <- c(NewTabNames, rep(TabName, NROW(CurTabColNames)))
    }

    if (Verbose) cat("\n\n")
    joint_table <- main_joint_tables[[NewTabNames[which(NewColNames == names(db_forced_rel[1]))]]]
    for (i in 1:NROW(db_forced_rel)) {
      CurRightTableName <- db$db_all_tabs[match(as.character(db_forced_rel[[i]]), db$db_all_cols)]
      CurRightColName <- as.character(db_forced_rel[i])
      RightColNames <- colnames(main_joint_tables[[CurRightTableName]])
      if (Verbose) cat(paste0("i = ", i, ". Join on: [", "joint_table", "].[", names(db_forced_rel)[i], "] = main_joint_tables[[", CurRightTableName, "]].[", CurRightColName,"]\n"))

      DuplicateColumnsToRem <-
        RightColNames[RightColNames %in% colnames(joint_table)] %>%
        {.[. %notin% CurRightColName]}

      RightTabForJoin <- main_joint_tables[[CurRightTableName]]
      if (NROW(DuplicateColumnsToRem) > 0) {
        RightTabForJoin %<>%
          select(-!!(DuplicateColumnsToRem))
      }

      joint_table %<>%
        left_join(RightTabForJoin,
                  by = (CurRightColName %>% set_names(names(db_forced_rel)[i])),
                  copy = FALSE
        ) %>%
        mutate(!! sym(CurRightColName) := !! sym(names(db_forced_rel)[i]))

      selected_cols <- data.frame(raw = colnames(joint_table),
                                  clean = sub("^([^.]*).*", "\\1", colnames(joint_table)),
                                  stringsAsFactors = FALSE
      ) %>%
        group_by(clean) %>%
        summarize(translated = max(raw)) %>%
        pull(translated) %>%
        {colnames(joint_table)[colnames(joint_table) %in% .]}

      renamed_cols <- stri_replace_all_fixed(selected_cols, ".y", "")

      if (NROW(colnames(joint_table)) != NROW(renamed_cols) || any(colnames(joint_table) != renamed_cols)) {
        joint_table %<>%
          select_(.dots = selected_cols %>% set_names(renamed_cols))
      }
      # if (get_sql_query) db$sql_joint_table <- dbplyr_to_sql(joint_table, con) #If left inside the loop, it can be used for debugging purposes, too.

    }

    included_cols <-
      c(
        ColsFromDbFields[ColsFromDbFields %in% colnames(joint_table)],
        db$NeededRenamedColNames[db$NeededRenamedColNames %in% colnames(joint_table)]
      ) %>%
      unique() %>%
      {colnames(joint_table)[colnames(joint_table) %in% .]}

    if (NROW(colnames(joint_table)) != NROW(included_cols) || any(colnames(joint_table) != included_cols)) {
      joint_table %<>%
        select(one_of(!!(included_cols)))
    }
    if (get_sql_query) db$sql_joint_table <- dbplyr_to_sql(joint_table, con)

  }

  return(joint_table)
}

#' Create Main Joint Tables
#'
#' Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel" WITHOUT Renames
#' @param db_fields A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
#' @param db_forced_rel A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS. e.g. c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#' @param con is a dbConnect {DBI} connection object to a SQL Database
#' @param Verbose A Boolean. Verbose = TRUE will output the consecutive joins as they happen
#' @param get_sql_query A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables
#' @keywords SQL Join JointTable OneJointTable
#' @export
#' @examples
#' Example:
#' db_forced_rel <- c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#' joint_table_Without_extended_joins <-
#'   created_joint_table(db_fields,
#'                       db_forced_rel = c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"),
#'                       )
#'
#' print(joint_table_Without_extended_joins)
#' #No renames are used and therefore no extended joins.
#' #This assumes that all tables can be joined together without any 1 table being needed twice.
created_joint_table <- function(db_fields, db_forced_rel, con = db$con, Verbose = TRUE, get_sql_query = TRUE) {
  joint_table_without_extended_joins <-
  CreateMainJointTables(db_fields = db_fields,
                        db_forced_rel = db_forced_rel,
                        DeselectKeysIfIncludeFalse = FALSE, #False in this case because we're going to need the keys for the extended join
                        con = db$con,
                        Verbose = Verbose,
                        get_sql_query = get_sql_query
                        ) %>%
  CreateOneJointTable(db_fields = db_fields,
                      con = db$con,
                      db_forced_rel = db_forced_rel,
                      Verbose = Verbose,
                      get_sql_query = get_sql_query
                      )
  return(joint_table_without_extended_joins)
}

#' Create Extended Main Joint Tables
#'
#' Get 1 table containing the information that main_joint_tables contained joined as given in "db_forced_rel" WITH or WITHOUT Renames as given by "db_ColumnsOldNamesToNewNames"
#' @param db_fields A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
#' @param con is a dbConnect {DBI} connection object to a SQL Database
#' @param db_forced_rel A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS. e.g. c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#' @param db_ColumnsOldNamesToNewNames A named List. Names correspond to the Table names, and the vectors inside will be used to renamed SQL Columns starting with db_ColumnsOldNamesToNewNames[i][j] to db_ColumnsOldNamesToNewNames[i][j+1] with j going from 1 to length of db_ColumnsOldNamesToNewNames[i] by 2
#' @param Verbose A Boolean. Verbose = TRUE will output the consecutive joins as they happen
#' @param get_sql_query A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables
#' @keywords SQL Join JointTable OneJointTable
#' @export
#' @examples
#' Example:
#' db_forced_rel <- c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#' joint_table_With_extended_joins <-
#'   create_extended_joint_table(db_fields,
#'                               db_forced_rel = c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"),
#'                               db_ColumnsOldNamesToNewNames =
#'                                 list(
#'                                      DIM_Employee = c(
#'                                                       c("Site_", "MainSite_")
#'                                                      )
#'                                      )
#'                               )
#'
#' print(joint_table_Without_extended_joins)
#' #Renames as given by "db_ColumnsOldNamesToNewNames", create extended joins on main_joint_tables for foreign tables that hold different meaning depending on which table they are joined with, and then everything is joined into 1 table as given by the relationships on "db_forced_rel"
create_extended_joint_table <- function(db_fields, db_forced_rel, db_ColumnsOldNamesToNewNames, con = db$con, Verbose = TRUE, get_sql_query = TRUE) {
  joint_table_With_extended_joins <-
    CreateMainJointTables(db_fields = db_fields,
                          db_forced_rel = db_forced_rel,
                          con = con,
                          DeselectKeysIfIncludeFalse = FALSE,
                          Verbose = Verbose,
                          get_sql_query = get_sql_query
                          ) %>%
    CreateExtendedMainJointTables(db_fields = db_fields,
                                  db_forced_rel = db_forced_rel,
                                  db_ColumnsOldNamesToNewNames = db_ColumnsOldNamesToNewNames,
                                  con = con,
                                  DeselectKeysIfIncludeFalse = FALSE,
                                  Verbose = Verbose,
                                  get_sql_query = get_sql_query
                                  ) %>%
    CreateOneJointTable(db_fields = db_fields,
                        db_forced_rel = db_forced_rel,
                        con = con,
                        Verbose = Verbose,
                        get_sql_query = get_sql_query
                        )
  return(joint_table_With_extended_joins)
}
