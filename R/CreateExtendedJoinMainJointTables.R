# #Get a list of all the Main tables in the database joined with all their relationships, Renaming Columns in certain tables
# # according to "db_ColumnsOldNamesToNewNames" and then joining the renamed columns according to the relationship that exists
# # on "db_forced_rel" for original column name (IF it exists). This way, if, for instance, both
# # lstTables[[DIM_Employee]] and lstTables[[FACT_Work]] reference lstTables[[DIM_Site]], then Site* can be renamed to MainSite*
# # and Extended_Join on lstTables[[DIM_Employee]] as this table refers to the main/base Site of the Employee, whilst
# # lstTables[[FACT_Work]] refers to the Site the employee worked on at that point of time (row).
# #lstTables[[FACT_Work]] row = another work related entry (1 employee has worked many times i.e. many rows)
# #lstTables[[DIM_Employee]] row = an employee. 1 and only 1 row per employee
# extended_main_joint_tables <-
#   CreateMainJointTables(db_fields, db_forced_rel, FALSE, db$con) %>%
#   CreateExtendedMainJointTables(db_fields, db$con, c("DIM_Employee"), list(DIM_Employee = c(c("Site_", "MainSite_"))))
# #Yields List of Tables

#' Create Extended Main Joint Tables
#'
#' Get a list of all the Main tables in the database joined with all their relationships, Renaming Columns in certain tables according to "db_ColumnsOldNamesToNewNames" and then joining the renamed columns according to the relationship that exists on "db_forced_rel" for original column name (IF it exists). This way, if, for instance, both lstTables[[DIM_Employee]] and lstTables[[FACT_Work]] reference lstTables[[DIM_Site]], then Site can be renamed to MainSite and Extended_Join on lstTables[[DIM_Employee]] as this table refers to the main/base Site of the Employee, whilst lstTables[[FACT_Work]] refers to the Site the employee worked on at that point of time (row).
#' @param main_joint_tables A named list of tibbles/DFs (usually given by CreateMainJointTables() as a SQL DB Pointer containing all user-selected fields plus needed ones for joins
#' @param db_fields A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
#' @param con is a dbConnect {DBI} connection object to a SQL Database
#' @param db_forced_rel A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS
#' @param db_ColumnsOldNamesToNewNames A named List. Names correspond to the Table names, and the vectors inside will be used to renamed SQL Columns starting with db_ColumnsOldNamesToNewNames[i][j] to db_ColumnsOldNamesToNewNames[i][j+1] with j going from 1 to length of db_ColumnsOldNamesToNewNames[i] by 2
#' @param Verbose A Boolean. Verbose = TRUE will output the consecutive joins as they happen
#' @param get_sql_query A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables
#' @keywords SQL Join ExtendedJoins MainJointTables
#' @export
#' @examples
#' extended_main_joint_tables <-
#'   CreateMainJointTables(db_fields, db_forced_rel, FALSE, db$con) %>%
#'   CreateExtendedMainJointTables(db_fields, db$con,
#'                                 db_forced_rel = c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID")
#'                                 db_ColumnsOldNamesToNewNames =
#'                                   list(
#'                                        DIM_Employee = c(
#'                                                         c("Site_", "MainSite_")
#'                                                        )
#'                                        )
#'                                 )
#'
#' print(joint_table_With_extended_joins)
CreateExtendedMainJointTables <- function(main_joint_tables, db_fields, db_forced_rel, db_ColumnsOldNamesToNewNames, con = db$con,
                                          DeselectKeysIfIncludeFalse = TRUE, Verbose = TRUE, get_sql_query = FALSE
) {
  ColsFromDbFields <-
    db_fields %>%
    filter(Include == "Yes") %>%
    pull(Column)

  ########################################################################################################
  ### Renaming Columns with same names amongst tables (due to joining) that hold different meaning     ###
  ### For example, if an employee is employed by a certain Building (his Main Building) but also works ###
  ### on others too. So DIM_Employee will have a BuildingID AND FACT_Work will have a BuildingID too   ###
  ########################################################################################################
  db$NeededRenamedColNames <- NULL    #Used in order to select() only Include==Yes at the end of 1-Joint-Table
  OldNamesToForceJoin <- NULL      #Used in order to Join Renamed Columns with foreign tables as well #NROW = NROW(db_forced_rel)
  NewNamesToForceJoin <- NULL      #Used in order to Join Renamed Columns with foreign tables as well #NROW = NROW(db_forced_rel)
  OldAndNewTabToForceJoin <- NULL  #Used in order to Join Renamed Columns with foreign tables as well #NROW = NROW(db_forced_rel)

  for (curTableName in names(db_ColumnsOldNamesToNewNames)) {
    CurColNames <- colnames(main_joint_tables[[curTableName]])
    OldAndNewNames <- db_ColumnsOldNamesToNewNames[[curTableName]]

    AllOldColNames <- NULL
    AllNewColNames <- NULL
    for (i in seq(1, NROW(OldAndNewNames), by = 2)) {
      OldStartsWith <- OldAndNewNames[[i]]
      NewStartsWith <- OldAndNewNames[[i + 1]]

      iColsNeedingChange <- startsWith(CurColNames, OldStartsWith) %>% which()
      if (NROW(iColsNeedingChange) > 0) {
        OldColNames <- CurColNames[iColsNeedingChange]
        NewColNames <- sapply(OldColNames, ReplaceStartWithSthElse, StartsWith = OldStartsWith, ReplaceWith = NewStartsWith, USE.NAMES = FALSE)
        AllOldColNames <- c(AllOldColNames, OldColNames)
        AllNewColNames <- c(AllNewColNames, NewColNames)
      }

      iNeededRenamedColNames <- OldColNames %in% (CurColNames[CurColNames %in% ColsFromDbFields])
      db$NeededRenamedColNames <- c(db$NeededRenamedColNames, NewColNames[iNeededRenamedColNames])

      iNamesToForceJoin <- OldColNames %in% as.character(db_forced_rel)
      OldNamesToForceJoin <- c(OldNamesToForceJoin, OldColNames[iNamesToForceJoin])
      NewNamesToForceJoin <- c(NewNamesToForceJoin, NewColNames[iNamesToForceJoin])
      OldAndNewTabToForceJoin <- c(OldAndNewTabToForceJoin, curTableName)
    }

    if (NROW(AllOldColNames) > 0) {
      main_joint_tables[[curTableName]] %<>%
        dplyr::rename_(
          .dots = AllOldColNames %>% set_names(AllNewColNames)
        )
    }
  }

  #This is *NOT* the normal loop to make JointTable, this is to complete renamed joins on main_joint_tables
  if (NROW(OldAndNewTabToForceJoin) > 0) {
    if (Verbose) cat("\n\n")
    for (i in 1:NROW(OldAndNewTabToForceJoin)) {
      CurRightTableName <- db$db_all_tabs[match(OldNamesToForceJoin[[i]], db$db_all_cols)]
      CurRightColName <- OldNamesToForceJoin[[i]]
      if (Verbose) cat(paste0("i = ", i, ". Join on: main_joint_tables[[", OldAndNewTabToForceJoin[[i]], "]].[", NewNamesToForceJoin[i], "] = [", CurRightTableName, "].[", CurRightColName ,"] as extended join for forced-join\n"))

      TabColNames <- colnames(main_joint_tables[[CurRightTableName]])
      OldAndNewNames <- db_ColumnsOldNamesToNewNames[[OldAndNewTabToForceJoin[[i]]]]

      #Renaming the Column Names on the Right Side that have a Renaming Schema
      NewRenamedColNames <- TabColNames
      if (NROW(OldAndNewNames) > 1) {
        for (k in seq(1, NROW(OldAndNewNames), by = 2)) {
          iColsNeedingChange <- startsWith(TabColNames, OldAndNewNames[[k]]) %>% which()
          if (NROW(iColsNeedingChange) > 0) {
            NewColNames <- NULL
            OldColNames <- TabColNames[iColsNeedingChange]
            for (j in 1:NROW(OldColNames)) {
              NewColNames <- c(NewColNames, ReplaceStartWithSthElse(OldColNames[[j]], OldAndNewNames[[k]], OldAndNewNames[[k + 1]]))
              NewRenamedColNames[[iColsNeedingChange[[j]]]] <- NewColNames[[j]]
            }
          }

          iNeededRenamedColNames <- OldColNames %in% (TabColNames[TabColNames %in% ColsFromDbFields])
          db$NeededRenamedColNames <- c(db$NeededRenamedColNames, NewColNames[iNeededRenamedColNames])

          #Renaming the rest of the names that doesn't have a Renaming Schema
          iColsNeedingChange <- (!startsWith(TabColNames, OldAndNewNames[[k]])) %>% which()
          if (NROW(iColsNeedingChange) > 0) {
            NewColNames <- NULL
            OldColNames <- TabColNames[iColsNeedingChange]
            for (j in 1:NROW(OldColNames)) {
              NewColNames <- c(NewColNames, paste0(OldAndNewNames[[k + 1]], OldColNames[[j]]))
              NewRenamedColNames[[iColsNeedingChange[[j]]]] <- NewColNames[[j]]
            }
          }
          iNeededRenamedColNames <- OldColNames %in% (TabColNames[TabColNames %in% ColsFromDbFields])
          db$NeededRenamedColNames <- c(db$NeededRenamedColNames, NewColNames[iNeededRenamedColNames])
        }

      } else {
        NewRenamedColNames <- paste0(OldAndNewNames[[2]], NewRenamedColNames)

        iNeededRenamedColNames <- OldColNames %in% (TabColNames[TabColNames %in% ColsFromDbFields])
        db$NeededRenamedColNames <- c(db$NeededRenamedColNames, NewRenamedColNames[iNeededRenamedColNames])
      }

      db$NeededRenamedColNames %<>%
        unique()

      #Doesn't need a special SELECT because if excluded columns existed, they would have already been removed in the main_joint_tables creation function.
      CurRightTable <-
        main_joint_tables[[CurRightTableName]] %>%
        dplyr::rename_(
          .dots = colnames(main_joint_tables[[CurRightTableName]]) %>% set_names(NewRenamedColNames)
        )

      DuplicateColumnsToRem <-
        colnames(CurRightTable)[colnames(CurRightTable) %in% colnames(main_joint_tables[[OldAndNewTabToForceJoin[[i]]]])] %>%
        {.[. %notin% NewNamesToForceJoin]}

      if (NROW(DuplicateColumnsToRem) > 0) {
        CurRightTable %<>%
          select(-one_of(DuplicateColumnsToRem))
      } #TODO Perhaps this could be a select where the dplyr::rename_ is now

      main_joint_tables[[OldAndNewTabToForceJoin[[i]]]] %<>%
        left_join(CurRightTable,
                  by = (NewRenamedColNames[TabColNames == CurRightColName] %>% set_names(NewNamesToForceJoin[i])),
                  copy = FALSE
        )

      selected_cols <- data.frame(raw = colnames(main_joint_tables[[OldAndNewTabToForceJoin[[i]]]]),
                                  clean = sub("^([^.]*).*", "\\1", colnames(main_joint_tables[[OldAndNewTabToForceJoin[[i]]]])),
                                  stringsAsFactors = FALSE
      ) %>%
        group_by(clean) %>%
        summarize(translated = max(raw)) %>%
        pull(translated) %>%
        {colnames(main_joint_tables[[OldAndNewTabToForceJoin[[i]]]])[colnames(main_joint_tables[[OldAndNewTabToForceJoin[[i]]]]) %in% .]}

      renamed_cols <- stri_replace_all_fixed(selected_cols, ".y", "")

      if (any(selected_cols != renamed_cols)) {
        main_joint_tables[[OldAndNewTabToForceJoin[[i]]]] %<>%
          select_(.dots = selected_cols %>% set_names(renamed_cols))
      }
    }
  }

  if (DeselectKeysIfIncludeFalse) { # MUST BE FALSE if we need to do any more joins (e.g. CreateOneJointTable())
    TableNames <- names(main_joint_tables)

    for (i in 1:NROW(TableNames)) {
      included_cols <-
        c(
          ColsFromDbFields[ColsFromDbFields %in% colnames(main_joint_tables[[TableNames[[i]]]])],
          db$NeededRenamedColNames[db$NeededRenamedColNames %in% colnames(main_joint_tables[[TableNames[[i]]]])]
        ) %>% unique()

      if (NROW(colnames(main_joint_tables[[TableNames[[i]]]])) != NROW(included_cols) || any(colnames(main_joint_tables[[TableNames[[i]]]]) != included_cols)) {
        main_joint_tables[[TableNames[[i]]]] %<>%
          select(one_of(!!(included_cols)))
      }
    }
    if (get_sql_query) db$sql_main_joint_tables[[TableNames[[i]]]] <- dbplyr_to_sql(main_joint_tables[[TableNames[[i]]]], con)
  }
  return(main_joint_tables)
}

#' Create Extended Main Joint Tables
#'
#' Get a list of all the Main tables in the database joined with all their relationships, Renaming Columns in certain tables according to "db_ColumnsOldNamesToNewNames" and then joining the renamed columns according to the relationship that exists on "db_forced_rel" for original column name (IF it exists). This way, if, for instance, both lstTables[[DIM_Employee]] and lstTables[[FACT_Work]] reference lstTables[[DIM_Site]], then Site can be renamed to MainSite and Extended_Join on lstTables[[DIM_Employee]] as this table refers to the main/base Site of the Employee, whilst lstTables[[FACT_Work]] refers to the Site the employee worked on at that point of time (row).
#' @param db_fields A DF with columns: "Include, KeyType, Table, Column, Type, RelationshipWithTable, RelationshipWithColumn, Transformation, Comment" about the User Selected fields and Relationships
#' @param con is a dbConnect {DBI} connection object to a SQL Database
#' @param db_forced_rel A Named String Vector. The vector names MUST point to the main table to be used for the 1-Joint-Table as its LHS
#' @param db_ColumnsOldNamesToNewNames A named List. Names correspond to the Table names, and the vectors inside will be used to renamed SQL Columns starting with db_ColumnsOldNamesToNewNames[i][j] to db_ColumnsOldNamesToNewNames[i][j+1] with j going from 1 to length of db_ColumnsOldNamesToNewNames[i] by 2
#' @param Verbose A Boolean. Verbose = TRUE will output the consecutive joins as they happen
#' @param get_sql_query A Boolean. get_sql_query = TRUE will create/edit the db$sql_main_joint_tables that output the SQL Code for the tables
#' @keywords SQL Join ExtendedJoins MainJointTables
#' @export
#' @examples
#' extended_main_joint_tables <-
#'   create_extended_main_joint_tables(db_fields,
#'                                     db_forced_rel = c(Hours_SiteID = "Site_SiteID", Hours_EmployeeID = "Employee_ID"),
#'                                     db_ColumnsOldNamesToNewNames =
#'                                       list(
#'                                            DIM_Employee = c(
#'                                                             c("Site_", "MainSite_")
#'                                                            )
#'                                            )
#'                                     )
#'
#' print(extended_main_joint_tables)
create_extended_main_joint_tables <- function(db_fields, db_forced_rel, db_ColumnsOldNamesToNewNames, con = db$con, Verbose = TRUE, get_sql_query = FALSE) {
  extended_main_joint_tables <-
  CreateMainJointTables(db_fields = db_fields,
                        db_forced_rel = db_forced_rel,
                        DeselectKeysIfIncludeFalse = FALSE,
                        con = db$con,
                        Verbose = Verbose,
                        get_sql_query = get_sql_query
                        ) %>%
  CreateExtendedMainJointTables(db_fields = db_fields,
                                con = db$con,
                                db_forced_rel = db_forced_rel,
                                db_ColumnsOldNamesToNewNames = db_ColumnsOldNamesToNewNames,
                                Verbose = Verbose,
                                get_sql_query = get_sql_query
                                )
  return(extended_main_joint_tables)
}
