#' Plots the ER Digramme for the SQL Database
#'
#' Shows a plot of the ER diagramme for the Database
#' @param DataModel A data model object as given by the datamodelr Library/Package
#' @param GraphDirection A string. Valid values are given by the rankdir parameter of the datamodelr Library/Package
#' @keywords ER Diagramme Diagram
#' @export
#' @examples
#' show_ER_diagramme(db$dm_f)
show_ER_diagramme <- function(DataModel = db$dm_f, GraphDirection = "RL") {
### Plot an ER Diagramme
  datamodelr::dm_create_graph(DataModel, rankdir = GraphDirection) %>%
    datamodelr::dm_render_graph()
}

#' Initialises the Library/Package with a SQL connection and retrieves information so that the exposed functions can work correctly
#'
#' Creates the SQL Connection and creates/Reads the db_fields .csv file according to the db_fields standard and according to what the SQL database looks like
#' @param csv_path A String. The path of the db_fields .csv file to read (or to be created if it doesn't exist)
#' @param Driver A string. A SQL Driver, etc.: {SQL Server};
#' @param Database A string. The name of the SQL Database itself
#' @param Server A string. A name that resolves into the SQL Machine, or the IP to the SQL Machine
#' @param UID A string. The Username credential for the SQL Connection. NULL if Windows Login is to be used
#' @param PWD A string. The Password credential for the SQL Connection. NULL if Windows Login is to be used
#' @param Trusted_Connection A Boolean. TRUE or FALSE on whether Windows Login is to be used
#' @param Port An Integer. The Port which the SQL Listener is listening at
#' @param ... If the relationships exist on the SQL Database, this can be left blank. Logical expressions for the SQL Relationships in the format: table1$FKcolumn1 == table2$IDcolumn5, table1$FKcolumn2 == table3$IDcolumn5, ...
#' @param ForceCreate_csv A Boolean. If TRUE then even if the db_fields exist, it will be deleted and overwriten by a newly created default db_fields
#' @param ExcludeIdentities A Boolean. The default inclusion behaviour for SQL Table Identities. If we're going to perform more joins afterwards or if we need to have a reference as to which table each column belongs to, then we shouldn't exclude them; if we only care about extrapolating information from each specific row, then we can exclude them by default and edit to include just the row identifier ID.
#' @param ExcludeForeignKeys A Boolean. If we need to perform manual joins afterwards or keep a reference as to with which table there's a connection, then we need them; otherwise we can safely exclude them
#' @param Update_DBEnv_DBFields A Boolean. If set to TRUE, you can have an internal main db_fields accessible via db$db_fields usually acting as the main db_fields. Default is FALSE as having local db_fields variables is the default behaviour.
#' @param ExcludeAuditingFields. A Boolean. If TRUE, any SQL Columns ending with "_OrigEntryOn", "_OrigEntryBy", "_EntryOn", "_EntryBy", "_CompName", "_Remote" or "_Username" will have INCLUDE == FALSE by default
#' @param ExcludeSYSDIAGRAMS. A Boolean. If TRUE, any SQL Columns on the table "sysdiagrams" will have INCLUDE == FALSE by default
#' @param RegexToSelectTables. A String. It defaults to getting all SQL Tables whose name begins with DIM_, FACT_, or TBL_.
#' @param UpdateDBFieldsFromDBCon A Boolean. If set to TRUE, then the db_fields saved on the file will be updated to reflect any changes in the SQL Database (removal or addition of Columns and Tables). User options are kept.
#' @keywords create db_fields dbfields
#' @export
#' @examples
#' db_fields <- initialise_return_db_fields(csv_path = "db_fields.csv",
#'                                          ForceCreate_csv = FALSE,
#'                                          ExcludeIdentities = FALSE,
#'                                          ExcludeForeignKeys = TRUE,
#'                                          Driver = "{SQL Server};",
#'                                          Database = "MyDatabaseName",
#'                                          Server = "123.456.7.8",
#'                                          UID = NULL,
#'                                          PWD = NULL,
#'                                          Trusted_Connection = TRUE,
#'                                          Port = 1433,
#'                                          table1$FKcolumn2 == table2$IDcolumn1,
#'                                          table1$FKcolumn3 == table3$IDcolumn1
#'                                          )
initialise_return_db_fields <- function(csv_path, Driver, Database, Server, UID, PWD, Trusted_Connection, Port = 1433, ...,
                                        ForceCreate_csv = FALSE, ExcludeIdentities = FALSE, ExcludeForeignKeys = TRUE, Update_DBEnv_DBFields = FALSE,
                                        ExcludeAuditingFields = FALSE, ExcludeSYSDIAGRAMS = TRUE, RegexToSelectTables = "^(DIM_|FACT_|TBL_)", UpdateDBFieldsFromDBCon = FALSE) {
  .GlobalEnv$db <- new.env()
  zinternal_update_db_info(Driver, Database, Server, UID, PWD, Trusted_Connection, Port, ..., RegexToSelectTables = RegexToSelectTables)

  if (!ForceCreate_csv && file.exists(csv_path)) {
    cat(paste0("\nReading db_fields from: ", csv_path, "\n\n"))
    db_fields <- read_db_fields_csv(csv_path, Update_DBEnv_DBFields, UpdateDBFieldsFromDBCon, ExcludeIdentities, ExcludeForeignKeys, ExcludeAuditingFields, ExcludeSYSDIAGRAMS)
  } else {
    cat(paste0("\n'", csv_path, "' does not exist.\nCalling zinternal_create_default_db_fields_from_db_con()\n"))
    db_fields <- zinternal_create_default_db_fields_from_db_con(csv_path, ExcludeIdentities, ExcludeForeignKeys,
                                                                Update_DBEnv_DBFields = FALSE, ExcludeAuditingFields, ExcludeSYSDIAGRAMS)
  }

  if (Update_DBEnv_DBFields == TRUE) db$db_fields <- db_fields
  return(db_fields)
}


#' Creates the db Environment and the SQL Connection (db$con)
#' @param Driver A string. A SQL Driver, etc.: {SQL Server};
#' @param Database A string. The name of the SQL Database itself
#' @param Server A string. A name that resolves into the SQL Machine, or the IP to the SQL Machine
#' @param UID A string. The Username credential for the SQL Connection. NULL if Windows Login is to be used
#' @param PWD A string. The Password credential for the SQL Connection. NULL if Windows Login is to be used
#' @param Trusted_Connection A Boolean. TRUE or FALSE on whether Windows Login/Credentials are to be used
#' @param Port An Integer. The Port which the SQL Listener is listening at. Can be left blank whereupon it will default to 1433
#' @param ... If the relationships exist on the SQL Database, this can be left blank. Logical expressions for the SQL Relationships in the format: table1$FKcolumn2 == table2$IDcolumn1, table1$FKcolumn3 == table3$IDcolumn1, ...
#' @param ReadRelationshipsFromDB. A Boolean. If TRUE an attempt to read the SQL Tables relationships from the Database will occur. User-defined relationships can also be added afterwards
#' @param AddUserRelationships. A Boolean. If TRUE User-defined relationships declared on the ... parameter will be added to the local SCHEMA. To ignore online relationships and only add user-defined, set ReadRelationshipsFromDB to FALSE
#' @param RegexToSelectTables. A String. It defaults to getting all SQL Tables whose name begins with DIM_, FACT_, or TBL_.
#' @keywords SQL Join db SQL Connection
#' @export
#' @examples
#' zinternal_update_db_info(Driver = "{SQL Server};", Database = "Chronogramme", Server = '123.456.7.8', UID = NULL, PWD = NULL, Trusted_Connection = TRUE, Port = 1433,
#'                DIM_Employee$Employee_MainSiteMainObjekt_SiteID == DIM_Site$Site_SiteID,
#'                DIM_Site$Site_RegionID == DIM_Region$Region_ID
#'                )
zinternal_update_db_info <- function(Driver, Database, Server, UID, PWD, Trusted_Connection, Port = 1433, ...,
                                     ReadRelationshipsFromDB = TRUE, AddUserRelationships = TRUE, RegexToSelectTables = "^(DIM_|FACT_|TBL_)") {
  db$con <- zinternal_connect_odbc(Driver, Database, Server, UID, PWD, Trusted_Connection, Port)
  #odbcClose(db$con)

  #Retrieving the names of all the tables starting with DIM or FACT
  db$db_tables_names <- dbListTables(db$con) %>%
    grep(RegexToSelectTables, .) %>%
    dbListTables(db$con)[.]

  #Getting a glimpse of all the tables
  cat("\nConnecting to the database and retrieving information...\n\n")
  db$lst_glimpses <- list()
  if (length(db$db_tables_names) > 0) {
    for (SQLTableCurNum in 1:length(db$db_tables_names)) {
      db$lst_glimpses[[db$db_tables_names[[SQLTableCurNum]]]] <-
        glimpse(tbl(db$con, db$db_tables_names[[SQLTableCurNum]])) %>%
        collect()
    }
  }

  ### Creating a Data Model Diagram ###
  sQuery <- datamodelr::dm_re_query("sqlserver") #TODO Implementations for non SQL Server databases as well (Postrgre already exists on package, see website)
  DataModel <- DBI::dbGetQuery(db$con, sQuery) %>% collect() %>% as.data.frame(stringsAsFactors = FALSE)

  # ### OLD
  # if (any(!is.na(DataModel$ref))) { #Retrieving the Data Model Diagram from the Database
  #   cat("\nRelationships found on the SQL Database, we're ignoring manually written ones and rely on what is shown on the SQL Database\n")
  #   db$dm_f <- datamodelr::as.data_model(DataModel)
  #
  # } else { #Creating a Data Model Diagram for the SQL Database from the glimpses
  #   cat("\nNo relationships found on the SQL Database, relying on what has been typed manually\n")
  #   db$dm_f <- datamodelr::dm_from_data_frames(db$lst_glimpses)
  #
  #   #Inserting the Relationships into the ER Diagram
  #   #                            MainTable$Ref == OtherTable$Identity
  #   # db$dm_f <- datamodelr::dm_add_references(db$dm_f,
  #   #                           TBL_IMPORT$IM_ID == SomeTable$SomeID,
  #   # )
  #   db$dm_f <- datamodelr::dm_add_references(db$dm_f, ...
  #   )
  #
  # }

  ### NEW
  if (ReadRelationshipsFromDB && any(!is.na(DataModel$ref))) { #Retrieving the Data Model Diagram from the Database
    cat("\nRelationships found on the SQL Database\n")
    db$dm_f <- datamodelr::as.data_model(DataModel)

    #When reading from the SQL Database we're not just getting the tables we selected on db$db_tables_names, and also we're not getting the VIEWS.
    if (db$dm_f$tables$table %>% NROW() != db$lst_glimpses %>% names() %>% NROW() || any((db$lst_glimpses %>% names()) %notin% db$dm_f$tables$table)) {
      MissingTables <- (db$lst_glimpses %>% names())[(db$lst_glimpses %>% names()) %notin% db$dm_f$tables$table]

      NewModelDF <- db$dm_f$columns %>% as_tibble()
      AdditionalRows <- datamodelr::dm_from_data_frames(db$lst_glimpses[MissingTables])$columns %>% as_tibble()
      NewModelDF %<>% rbind(tibble(table = AdditionalRows$table,
                                   column = AdditionalRows$column,
                                   key = AdditionalRows$key,
                                   ref = AdditionalRows$ref,
                                   ref_col = NA,
                                   mandatory = 0,
                                   type = NA,
                                   max_length = NA,
                                   precision = NA,
                                   scale = NA))

      db$dm_f <- as.data_model.data.frame(NewModelDF)

    }

  } else {                                                     #Creating a Data Model Diagram for the SQL Database from the glimpses
    cat("\nNo relationships found on the SQL Database (or something prevented searching for them)\n")
    db$dm_f <- datamodelr::dm_from_data_frames(db$lst_glimpses)
  }


  if (AddUserRelationships) {
    cat("Adding user-defiend relationships (if applicable)\n")
    #Inserting the Relationships into the ER Diagram
    #                            MainTable$Ref == OtherTable$Identity
    # db$dm_f <- datamodelr::dm_add_references(db$dm_f,
    #                           TBL_IMPORT$IM_ID == SomeTable$SomeID,
    # )
    db$dm_f <- datamodelr::dm_add_references(db$dm_f, ...
    )
  }

  #Defining Database Variables
  cat("\nDefining Database Variables.\n")
  #db_tables_names #already exists
  db$db_all_cols <- db$dm_f$columns$column #SQL Columns (length=length(tables))
  db$db_all_tabs <- db$dm_f$columns$table #The tables each SQL Column belongs to (length=length(tables))
  db$db_all_types <- db$dm_f$columns$type #numeric, char, etc. (length=length(tables))
  db$db_all_is_ident <- as.logical(db$dm_f$columns$key) #(Boolean isKey) Unique Primary Key SQL Columns, (length=length(tables), most are FALSE)
  db$db_all_is_rel <- (!is.na(db$dm_f$columns$ref)) #(length=length(tables))
  db$db_all_rel_tabs <- db$dm_f$columns$ref #Relationships between the tables (as given in dm_add_references), (length=length(tables), Most are NA)
  db$db_all_rel_cols <- db$dm_f$columns $ref_col #The respective SQL Columns defined in dm_add_references with (length=length(tables) WHEN rels exist, Most are NA)
  if (is.null(db$db_all_rel_cols)) db$db_all_rel_cols <- rep(NA, NROW(db$db_all_rel_tabs))

  db$db_ident_col_names <- db$dm_f$columns$column[as.logical(db$dm_f$columns$key)] #The (some) Identity Column Names
  db$db_ident_tab_names <- db$dm_f$columns$table[as.logical(db$dm_f$columns$key)] #The (some) Identity Table Names
  #db_ident_tab_names_dist <- unique(db$db_rel_tab_names) #By definition The (some) identities are unique so their respective tables should be unique too (not table names with replacement)

  db$db_for_col_names <- db$dm_f$columns$column[!is.na(db$dm_f$columns$ref_col)] #The actual (some) foreign keys columns alone (different number to the Identities)
  db$db_for_tab_names <- db$dm_f$columns$table[!is.na(db$dm_f$columns$ref_col)] #The actual (some) foreign keys tables alone  (different number to the Identities)

  db$db_rel_tab_names <- db$dm_f$columns$ref[!is.na(db$dm_f$columns$ref)] #The actual (some) Tables alone (different number to the Identities)
  db$db_rel_tab_names_dist <- unique(db$db_rel_tab_names) #amongst (some) tables (No replacement now, just the DISTINCT tables)
  db$db_rel_col_names <- db$dm_f$columns$ref_col[!is.na(db$dm_f$columns$ref_col)] #The actual (some) Columns alone (different number to the Identities)
  db$db_rel_col_names_dist <- unique(db$db_rel_col_names) #Out of the (some) fields/columns creating the relationships, (less some) are unique (such that A_Table creates relationships with n other tables)
  # db$db_ColumnsOldNamesToNewNames <-
  #   list(
  #     DIM_Employee = c(
  #       c("Site_", "MainSite_")
  #     )
  #   )
  # #Assumptions: Database is in Canonical Form, No 2 columns have the same name (Usual good practice in Databases)
  # db$db_forced_rel <- #If a Forced Rel Column Name is renamed by db_ColumnsOldNamesToNewNames above, then use new name
  #   c(                #The 1st Relationship MUST have the main table to be used for the 1-Joint-Table as its LHS
  #     Hours_SiteID = "Site_SiteID",
  #     Hours_EmployeeID = "Employee_ID",
  #     Hours_WageTypeID = "WageType_ID"
  #   )
  cat("Done with update_db_info().\n")
}

#' Tibble to T-SQL
#'
#' Retrieves the T-SQL code behind any Tbl even after R transformations
#' @param TibbleDbPointer A tbl()
#' @param con is a dbConnect {DBI} connection object to a SQL Database
#' @keywords TSQL SQL
#' @export
#' @examples
#' dbplyr_to_sql(tbl("TableName", db$con) db$con)
dbplyr_to_sql <- function(TibbleDbPointer, con = db$con) {
  SQL <- sql_render(sql_build(TibbleDbPointer, con), con)
  return(SQL)
}

#' Creates an ODBC Connection
#'
#' @param Driver A string. A SQL Driver for SQL Server, Postgre, etc., e.g.: "{SQL Server};"
#' @param Database A string. The name of the SQL Database itself
#' @param Server A string. A name that resolves into the SQL Machine, or the IP to the SQL Machine
#' @param UID A string. The Username credential for the SQL Connection. NULL if Windows Login is to be used
#' @param PWD A string. The Password credential for the SQL Connection. NULL if Windows Login is to be used
#' @param Trusted_Connection A Boolean. TRUE or FALSE on whether Windows Login/Credentials are to be used instead of a UID/PWD
#' @param Port An Integer. The Port which the SQL Listener is listening at. Can be left blank whereupon it will default to 1433
#' @keywords ODBC SQL Connection
#' @export
#' @examples
#' zinternal_connect_odbc(Driver = "{SQL Server};", Database = "Chronogramme", Server = '123.456.7.8', UID = NULL, PWD = NULL, Trusted_Connection = TRUE, Port = 1433)
zinternal_connect_odbc <- function(Driver, Database, Server, UID, PWD, Trusted_Connection, Port = 1433) {
  if (is.not.null(Trusted_Connection) && !Trusted_Connection) Trusted_Connection <- NULL
    con <- DBI::dbConnect(odbc::odbc(), Driver = Driver,
                          Database = Database, Server = Server, UID = UID,
                          PWD = PWD, trusted_connection = Trusted_Connection, Port = Port)
  return(con)
}

#' Creates a db_fields .csv file according to the db_fields standard and according to what the SQL database looks like
#'
#' Returns the db_fields variable with columns: KeyType, Table, Column, Type, Comment, RelationshipWithTable, RelationshipWithColumn, Transformation
#' @param csv_path A String. The path of the db_fields .csv file.
#' @param ExcludeIdentities A Boolean. The default inclusion behaviour for SQL Table Identities. If we're going to perform more joins afterwards or if we need to have a reference as to which table each column belongs to, then we shouldn't exclude them; if we only care about extrapolating information from each specific row, then we can exclude them by default and edit to include just the row identifier ID.
#' @param ExcludeForeignKeys A Boolean. If we need to perform manual joins afterwards or keep a reference as to with which table there's a connection, then we need them; otherwise we can safely exclude them
#' @param Update_DBEnv_DBFields A Boolean. If set to TRUE, you can have an internal main db_fields accessible via db$db_fields usually acting as the main db_fields. Default is FALSE as having local db_fields variables is the default behaviour.
#' @param ExcludeAuditingFields. A Boolean. If TRUE, any SQL Columns ending with "_OrigEntryOn", "_OrigEntryBy", "_EntryOn", "_EntryBy", "_CompName", "_Remote" or "_Username" will have INCLUDE == FALSE by default
#' @param ExcludeSYSDIAGRAMS. A Boolean. If TRUE, any SQL Columns on the table "sysdiagrams" will have INCLUDE == FALSE by default
#' @keywords create db_fields dbfields
#' @export
#' @examples
#' db_fields <- zinternal_create_default_db_fields_from_db_con("db_fields.csv")
zinternal_create_default_db_fields_from_db_con <- function(csv_path = NULL, ExcludeIdentities = FALSE, ExcludeForeignKeys = TRUE, Update_DBEnv_DBFields = FALSE, ExcludeAuditingFields = FALSE, ExcludeSYSDIAGRAMS = TRUE) {
  ############################################## IDENTITY is more important because multiple ForeignKeys might reference it, whilst ForeignKeys themselves are not relevant after the merge
  ### Creating a Feature Selection Interface ###
  ##############################################
  #== ASSUMES THAT zinternal_update_db_info() HAS BEEN EXECUTED SUCCESSFULLY FIRST ==#

  #Setting Default Values:
  SelectionOptions <- c("Yes", "No", "N/A")
  if (ExcludeIdentities && ExcludeForeignKeys) {
    Include <- if_else(db$db_all_is_ident, SelectionOptions[2], if_else(!is.na(db$db_all_rel_cols), SelectionOptions[2], SelectionOptions[1], SelectionOptions[3]), SelectionOptions[3])
  } else if (ExcludeForeignKeys) {
    Include <- if_else(!is.na(db$db_all_rel_cols), SelectionOptions[2], SelectionOptions[1], SelectionOptions[3])
  } else if (ExcludeIdentities) {
    Include <- if_else(db$db_all_is_ident, SelectionOptions[2], SelectionOptions[1], SelectionOptions[3])
  } else {
    Include <- rep(SelectionOptions[2], NROW(db$db_all_is_ident))
  }
  if (ExcludeAuditingFields == TRUE) {
    Include[db$db_all_cols %>% endsWith("_OrigEntryOn") |
            db$db_all_cols %>% endsWith("_OrigEntryBy") |
            db$db_all_cols %>% endsWith("_EntryOn") |
            db$db_all_cols %>% endsWith("_EntryBy") |
            db$db_all_cols %>% endsWith("_CompName") |
            db$db_all_cols %>% endsWith("_Remote") |
            db$db_all_cols %>% endsWith("_Username")] <- SelectionOptions[2]
  }
  if (ExcludeSYSDIAGRAMS == TRUE) Include[db$db_all_tabs == "sysdiagrams"] <- SelectionOptions[2]
  #  -Editing default values according to accrued knowledge:
  # Include[db$db_all_cols == "Customer_ID"] <- SelectionOptions[1] #At any and all points, we need to be able to identify the Customer
  # Include[db$db_all_cols == "SomeColumnName"] <- SelectionOptions[2] #The information contained here is better portrayed by AnotherColumnName

  KeyTypeOptions <- c("Identity", "ForeignKey", "None", "N/A")
  KeyType <- if_else(db$db_all_is_ident, KeyTypeOptions[1], if_else(!is.na(db$db_all_rel_cols), KeyTypeOptions[2], KeyTypeOptions[3]))

  RelationshipWithTable <- if_else(is.na(db$db_all_rel_tabs), "", as.character(db$db_all_rel_tabs))
  RelationshipWithColumn <- if_else(is.na(db$db_all_rel_cols), "", as.character(db$db_all_rel_cols))

  #Putting in Comments on the CSV
  Comments <- character(NROW(db$db_all_cols))
  # Comments[db_all_cols == "Some_ID"] <- "Identification for each type. Contains: All possible types"

  CommentsOnForcedRelationships <- character(NROW(db$db_forced_rel))

  db_fields <- tibble(
    Include = factor(c(Include, rep.int(SelectionOptions[3], length(db$db_forced_rel))), levels = SelectionOptions),
    KeyType = c(KeyType, rep.int(KeyTypeOptions[4], length(db$db_forced_rel))),
    Table = c(db$db_all_tabs, db$db_all_tabs[match(names(db$db_forced_rel), db$db_all_cols)]),
    Column = c(db$db_all_cols, names(db$db_forced_rel)),
    Type = c(db$db_all_types, rep(KeyTypeOptions[4], length(db$db_forced_rel))),
    RelationshipWithTable = c(RelationshipWithTable, db$db_all_tabs[match(as.character(db$db_forced_rel), db$db_all_cols)]),
    RelationshipWithColumn = c(RelationshipWithColumn, as.character(db$db_forced_rel)),
    Transformation = "",
    Comment = c(Comments, CommentsOnForcedRelationships)
  )

  if (is.not.null(csv_path)) write_db_fields_csv(db_fields, csv_path, Update_DBEnv_DBFields)

  return(db_fields)
}

#' Updates the db_fields to the newest SQL Schema, keeping your previous selected Includes, after Fields or whole Tables have been added or deleted.
#'
#' Returns the db_fields variable with columns: KeyType, Table, Column, Type, Comment, RelationshipWithTable, RelationshipWithColumn, Transformation
#' @param csv_path A String. The path of the db_fields .csv file.
#' @param csv_output A Boolean or a File Path as String. If FALSE then the changes will only return as a variable. If TRUE then the original db_fields file (csv_path) will be updated. If it's a File Path String then the original file remains intact and changes are saved on that File Path
#' @param ExcludeIdentities A Boolean. The default inclusion behaviour for SQL Table Identities. If we're going to perform more joins afterwards or if we need to have a reference as to which table each column belongs to, then we shouldn't exclude them; if we only care about extrapolating information from each specific row, then we can exclude them by default and edit to include just the row identifier ID.
#' @param ExcludeForeignKeys A Boolean. If we need to perform manual joins afterwards or keep a reference as to with which table there's a connection, then we need them; otherwise we can safely exclude them
#' @param Update_DBEnv_DBFields A Boolean. If set to TRUE, you can have an internal main db_fields accessible via db$db_fields usually acting as the main db_fields. Default is FALSE as having local db_fields variables is the default behaviour.
#' @param ExcludeAuditingFields. A Boolean. If TRUE, any SQL Columns ending with "_OrigEntryOn", "_OrigEntryBy", "_EntryOn", "_EntryBy", "_CompName", "_Remote" or "_Username" will have INCLUDE == FALSE by default
#' @param ExcludeSYSDIAGRAMS. A Boolean. If TRUE, any SQL Columns on the table "sysdiagrams" will have INCLUDE == FALSE by default
#' @keywords create db_fields dbfields
#' @export
#' @examples
#' db_fields <- update_db_fields_from_db_con("db_fields.csv")
update_db_fields_from_db_con <- function(csv_path, csv_output = NULL, ExcludeIdentities = FALSE, ExcludeForeignKeys = TRUE, Update_DBEnv_DBFields = FALSE, ExcludeAuditingFields = FALSE, ExcludeSYSDIAGRAMS = TRUE) {
  NewDBFields <- zinternal_create_default_db_fields_from_db_con(csv_path = NULL, ExcludeIdentities, ExcludeForeignKeys, Update_DBEnv_DBFields = FALSE, ExcludeAuditingFields, ExcludeSYSDIAGRAMS)
  OldDBFields <- read_db_fields_csv(csv_path)

  NewFieldsAlone <- NewDBFields %>% mutate(Fields = paste(Table, Column, sep = "__")) %>% pull(Fields)
  OldFieldsAlone <- OldDBFields %>% mutate(Fields = paste(Table, Column, sep = "__")) %>% pull(Fields)
  INDX <- NewFieldsAlone %in% OldFieldsAlone %>% which
  for (i in INDX) {
    NewDBFields$Include[[i]] <- OldDBFields %>% filter(Table == NewDBFields$Table[[i]] & Column == NewDBFields$Column[[i]]) %>% pull(Include)
    NewDBFields$Transformation[[i]] <- OldDBFields %>% filter(Table == NewDBFields$Table[[i]] & Column == NewDBFields$Column[[i]]) %>% pull(Transformation)
    NewDBFields$Comment[[i]] <- OldDBFields %>% filter(Table == NewDBFields$Table[[i]] & Column == NewDBFields$Column[[i]]) %>% pull(Comment)
  }

  if (Update_DBEnv_DBFields == TRUE) db$db_fields <- db_fields

  if (is.not.null(csv_output)) {
    if (any("logical" %in% class(csv_output))) {
      if (csv_output == TRUE) {
        write_db_fields_csv(NewDBFields, csv_path)
        cat(paste0("db_fields.csv is saved on: '", csv_path, "'\n"))
      }
    } else {
      write_db_fields_csv(NewDBFields, csv_output)
      cat(paste0("db_fields.csv is saved on: '", csv_output, "'\n"))
    }
  }

  return(NewDBFields)
}

#' Reads the csv file that is formatted according to the db_fields standard
#'
#' Returns the db_fields, a DF with columns: KeyType, Table, Column, Type, Comment, RelationshipWithTable, RelationshipWithColumn, Transformation
#' @param csv_path A String. The path of the db_fields .csv file.
#' @param Update_DBEnv_DBFields A Boolean. If set to TRUE, you can have an internal main db_fields accessible via db$db_fields usually acting as the main db_fields. Default is FALSE as having local db_fields variables is the default behaviour.
#' @param UpdateDBFieldsFromDBCon A Boolean. If set to TRUE, then the db_fields saved on the file will be updated to reflect any changes in the SQL Database (removal or addition of Columns and Tables). User options are kept.
#' @param ExcludeIdentities A Boolean. The default inclusion behaviour for SQL Table Identities. If we're going to perform more joins afterwards or if we need to have a reference as to which table each column belongs to, then we shouldn't exclude them; if we only care about extrapolating information from each specific row, then we can exclude them by default and edit to include just the row identifier ID.
#' @param ExcludeForeignKeys A Boolean. If we need to perform manual joins afterwards or keep a reference as to with which table there's a connection, then we need them; otherwise we can safely exclude them
#' @param ExcludeAuditingFields. A Boolean. If TRUE, any SQL Columns ending with "_OrigEntryOn", "_OrigEntryBy", "_EntryOn", "_EntryBy", "_CompName", "_Remote" or "_Username" will have INCLUDE == FALSE by default
#' @param ExcludeSYSDIAGRAMS. A Boolean. If TRUE, any SQL Columns on the table "sysdiagrams" will have INCLUDE == FALSE by default
#' @keywords Read db_fields dbfields
#' @export
#' @examples
#' db_fields <- read_db_fields_csv("db_fields.csv")
read_db_fields_csv <- function(csv_path, Update_DBEnv_DBFields = FALSE, UpdateDBFieldsFromDBCon = FALSE, ExcludeIdentities = FALSE, ExcludeForeignKeys = TRUE, ExcludeAuditingFields = FALSE, ExcludeSYSDIAGRAMS = TRUE) {

  if (UpdateDBFieldsFromDBCon) {
    db_fields <-
      update_db_fields_from_db_con(csv_path, csv_output = NULL, ExcludeIdentities, ExcludeForeignKeys, Update_DBEnv_DBFields = FALSE, ExcludeAuditingFields, ExcludeSYSDIAGRAMS)
  } else {
    db_fields <-
      read.csv(csv_path,
               header = TRUE,
               colClasses = c("KeyType" = "character", "Table" = "character", "Column" = "character", "Type" = "character",
                              "Comment" = "character", "RelationshipWithTable" = "character", "RelationshipWithColumn" = "character",
                              "Transformation" = "character"), stringsAsFactors = FALSE) %>%
      as_tibble()

    db_fields$Include %<>%
      factor(levels = c("Yes", "No", "N/A"))
  }

  if (Update_DBEnv_DBFields) db$db_fields <- db_fields
  return(db_fields)
}

#' Writes the csv file that is formatted according to the db_fields standard
#'
#' Creates a .csv file with columns: KeyType, Table, Column, Type, Comment, RelationshipWithTable, RelationshipWithColumn, Transformation
#' @param db_fields A DF. A dataframe formatted according to the db_fields standard.
#' @param csv_path A String. The path of the db_fields .csv file.
#' @param Update_DBEnv_DBFields A Boolean. If set to TRUE, you can have an internal main db_fields accessible via db$db_fields usually acting as the main db_fields. Default is FALSE as having local db_fields variables is the default behaviour.
#' @keywords Write db_fields dbfields
#' @export
#' @examples
#' Example 1:
#' db_fields <- zinternal_create_default_db_fields_from_db_con(csv_path)
#' write_db_fields_csv(db_fields, "db_fields.csv")
#'
#' Example 2:
#' db_fields <- read_db_fields_csv("db_fields.csv")
#' write_db_fields_csv(db_fields, "db_fields.csv")
write_db_fields_csv <- function(db_fields, csv_path, Update_DBEnv_DBFields = FALSE) {
  write.csv(db_fields, csv_path, row.names = FALSE)
  if (Update_DBEnv_DBFields) db$db_fields <- db_fields
  cat(paste0("db_fields.csv is saved on: '", csv_path, "'\n"))
}

#' Edits a db_fields formatted DF
#'
#' Opens a browser window where the user can select which columns to include and see the relationships amongst the tables.
#' When the browser window closes, the results are saved on the DF, but NOT on the db_fields.csv file; for this use write_db_fields_csv()
#' @param db_fields A DF. A dataframe formatted according to the db_fields standard.
#' @param Update_DBEnv_DBFields A Boolean. If set to TRUE, you can have an internal main db_fields accessible via db$db_fields usually acting as the main db_fields. Default is FALSE as having local db_fields variables is the default behaviour.
#' @keywords edit db_fields dbfields
#' @export
#' @examples
#' db_fields <- read_db_fields_csv("db_fields.csv")
#' edit_db_fields(db_fields)
#' write_db_fields_csv(db_fields, "db_fields.csv")
edit_db_fields <- function(db_fields = db$db_fields, Update_DBEnv_DBFields = FALSE) {
  hot_read_only_cols <- c("KeyType", "Table", "Column", "Type", "RelationshipWithTable", "RelationshipWithColumn")

  colWidths <- integer(NCOL(db_fields))
  colWidths[names(db_fields) == "Include"] <- 60
  colWidths[names(db_fields) == "KeyType"] <- 80
  colWidths[names(db_fields) == "Table"] <- 150
  colWidths[names(db_fields) == "Column"] <- 150
  colWidths[names(db_fields) == "Type"] <- 75
  colWidths[names(db_fields) == "RelationshipWithTable"] <- 150
  colWidths[names(db_fields) == "RelationshipWithColumn"] <- 162
  colWidths[names(db_fields) == "Transformation"] <- 150
  colWidths[names(db_fields) == "Comment"] <- 800

  app = shinyApp(
    shinyUI(fluidPage(
      titlePanel("Feature Selection and Manipulation Interface \n[close window to save]"),
      rHandsontableOutput("hot"),
      tags$head( #Creating a horizontal scroll bar
        tags$style(
          HTML(".handsontable {
                overflow: visible;
               }"
        )
        )
      )
    )),

    shinyServer(function(input, output, session) {
      samp <- reactiveValues()
      samp$df <- db_fields

      output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
          db_fields  <- hot_to_r(input$hot)
          if (Update_DBEnv_DBFields) db$db_fields <- db_fields
        } else {
          db_fields  <- samp$df
          if (Update_DBEnv_DBFields) db$db_fields <- db_fields
        }

        rhandsontable(db_fields, useTypes = TRUE, search = TRUE) %>%
          hot_col(hot_read_only_cols, readOnly = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE) %>% #Options for the functionality of the form
          hot_cols(colWidths = colWidths, manualColumnResize = TRUE)

      })

      session$onSessionEnded(function() {
        stopApp()
      })

    })#,

    #options = options(shiny.launch.browser = .rs.invokeShinyWindowViewer)

  )

  #runApp(app, launch.browser = .rs.invokeShinyWindowViewer)
  runApp(app, launch.browser = getOption("shiny.launch.browser", interactive()))

  return(db_fields)
}
