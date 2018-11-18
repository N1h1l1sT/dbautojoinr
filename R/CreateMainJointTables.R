# #Get a list of all the Main tables in the database joined with all their relationships with only Include == TRUE columns
# main_joint_tables <-
#   CreateMainJointTables(db_fields, c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"), TRUE, db$con)
# #Yields List of Tables

#' Create Extended Main Joint Tables
#'
#' Get a list of all the Main tables in the database joined with all their relationships with only Include == TRUE columns
#' @param db_fields A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
#' @param db_forced_rel A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS
#' @param DeselectKeysIfIncludeFalse A Boolean. Must be FALSE if we need to continue to 1-Joint-Table, otherwise needed Identity and Foreign keys might be missing
#' @param con A dbConnect {DBI} connection object to a SQL Database
#' @param Verbose A Boolean. Verbose = TRUE will output the consecutive joins as they happen
#' @param get_sql_query A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables
#' @keywords SQL Join JointTable OneJointTable
#' @export
#' @examples
#' main_joint_tables <-
#'   CreateMainJointTables(db_fields, c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"), FALSE, db$con) %>%
#'
#' print(main_joint_tables)
#' All the Main Tables (tables with foreign keys) are joined with their relationships and are returned as a list of tibbles (n = number of tabels with foreign keys that also contain User Selected fields)
CreateMainJointTables <- function(db_fields, db_forced_rel, con = db$con, DeselectKeysIfIncludeFalse = TRUE, Verbose = TRUE, get_sql_query = FALSE) {
  #Selects TABLES according to db_fields
  #Assumptions: Database is in Canonical Form, No 2 columns have the same name (Usual good practice in Databases)
  #If there are ANY .x, .y variables created THEN it means one or more columns between Tables had the same name! (It' frowned upon)
  #Nonetheless, if any .x, .y columns are created, it is assumed they hold the same information and only 1 of the 2 is selected and renamed
  #"DeselectKeysIfIncludeFalse=FALSE" if we need to continue to 1-Joint-Table, otherwise needed Identity and Foreign keys might be missing

  ####################################################
  ### Joining the main tables all their references ###
  ##############################s######################

  LeftSideTablesNamesWthReplacement <- db$db_all_tabs[db$db_all_is_rel]           #Getting the names of the main tables which relate to the rest of the tables
  LeftSideColsNamesWthReplacement <- db$db_all_cols[db$db_all_is_rel]             #Getting the names of the main Columns which relate to the rest of the tables

  #Getting only the indexes to the tables that are within Include=Yes (db_fields)
  AllNeededTabs <-
    db_fields %>%
    filter(Include == "Yes") %>%
    pull(Table) %>%
    unique()

  #Removing superfluous Tables and Columns
  iLeftAllNeededTabs <- which(LeftSideTablesNamesWthReplacement %in% AllNeededTabs)
  LeftTabsOfInterest <- LeftSideTablesNamesWthReplacement[iLeftAllNeededTabs]
  LeftColsOfInterest <- LeftSideColsNamesWthReplacement[iLeftAllNeededTabs]
  AllNeededCols <-
    db_fields %>%
    filter(Include == "Yes") %>%
    pull(Column)

  main_joint_tables <- list()
  db$sql_main_joint_tables <- list()
  MainTablesNames <- unique(LeftTabsOfInterest)              #Having the main tables' names we can now iterate over them

  if (Verbose) cat("\n")
  for (i in 1:length(MainTablesNames)) {                                  #For each table name
    curTableName <- MainTablesNames[i]

    #Removing superfluous Tables and Columns
    NeededLeftColsIndx <-
      db$db_all_cols[which(db$db_all_tabs == curTableName)][
        db$db_all_cols[which(db$db_all_tabs == curTableName)] %in% (c(AllNeededCols, db$db_ident_col_names, db$db_for_col_names, as.character(db_forced_rel)) %>%
                                                                      unique()
                                                                    )
        ]

    iRightAllNeededTabs <- which((db$db_for_tab_names == curTableName) & (db$db_rel_tab_names %in% AllNeededTabs))
    RightSideTablesNames <- db$db_rel_tab_names[iRightAllNeededTabs]
    RightSideColsNames <- db$db_rel_col_names[iRightAllNeededTabs]

    if (Verbose) cat(paste0("\ncurTableName = ", curTableName, "\n"))

    main_joint_tables[[curTableName]] <-
      tbl(con, curTableName) %>%         #Retrieve the actual table Schema (SQL DB pointer)
      select(!!(NeededLeftColsIndx))

    if (get_sql_query) db$sql_main_joint_tables[[curTableName]] <- dbplyr_to_sql(main_joint_tables[[curTableName]], con)

    for (j in 1:length(RightSideTablesNames)) {
      NeededRightCols <-
        db$db_all_cols[which(db$db_all_tabs == RightSideTablesNames[j])][
          db$db_all_cols[which(db$db_all_tabs == RightSideTablesNames[j])] %in% (c(AllNeededCols, db$db_ident_col_names, db$db_for_col_names, as.character(db_forced_rel)) %>%
                                                                                unique()
                                                                              )
          ]

      iwhich <- which(db$db_for_tab_names[which(db$db_rel_tab_names == RightSideTablesNames[j])] == curTableName)
      leftByCol <- db$db_for_col_names[which(db$db_rel_tab_names == RightSideTablesNames[j])[iwhich]]

      #LeftColsOfInterest[LeftTabsOfInterest == curTableName][j]
      if (Verbose) cat(paste0("j = ", j, "\nJoin on: [",
                              curTableName, "].[", leftByCol, "] = [",
                              RightSideTablesNames[j], "].[", RightSideColsNames[j],"]\n"))

      curRightTab <-
        tbl(con, RightSideTablesNames[j]) %>%
        select(!!(NeededRightCols))

      main_joint_tables[[curTableName]] <- main_joint_tables[[curTableName]] %>%
        left_join(curRightTab,                  #and join this table with its relationship-table as defined on db_fields.csv
                  by = (RightSideColsNames[j] %>% set_names(leftByCol)),
                  copy = FALSE
        ) %>%
        mutate(!!RightSideColsNames[j] := !!(rlang::sym(leftByCol)))
      if (get_sql_query) db$sql_main_joint_tables[[curTableName]] <- dbplyr_to_sql(main_joint_tables[[curTableName]], con)
    }
  }

  if (DeselectKeysIfIncludeFalse) {
    for (i in 1:NROW(names(main_joint_tables))) {
      ColsToSelect <-
        db_fields %>%
        filter(Include == "Yes" & Table %in% (
          db$db_all_tabs[db$db_all_cols %in% colnames(main_joint_tables[[i]])] %>% unique()
        )) %>%
        pull(Column)

      main_joint_tables[[names(main_joint_tables)[[i]]]] %<>%
        select(one_of(!!(ColsToSelect)))
    }
  }

  if (get_sql_query) {
    while (sum(search() == "db") > 0) detach(db)
    attach(db)
  }

  return(main_joint_tables)
}
