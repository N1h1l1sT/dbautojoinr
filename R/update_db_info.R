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
#' @param ForceCreate_csv A Boolean. If TRUE then even if the
#' @param ExcludeIdentities A Boolean. The default inclusion behaviour for SQL Table Identities. If we're going to perform more joins afterwards or if we need to have a reference as to which table each column belongs to, then we shouldn't exclude them; if we only care about extrapolating information from each specific row, then we can exclude them by default and edit to include just the row identifier ID.
#' @param ExcludeForeignKeys A Boolean. If we need to perform manual joins afterwards or keep a reference as to with which table there's a connection, then we need them; otherwise we can safely exclude them
#' @param Driver A string. A SQL Driver, etc.: {SQL Server};
#' @param Database A string. The name of the SQL Database itself
#' @param Server A string. A name that resolves into the SQL Machine, or the IP to the SQL Machine
#' @param UID A string. The Username credential for the SQL Connection. NULL if Windows Login is to be used
#' @param PWD A string. The Password credential for the SQL Connection. NULL if Windows Login is to be used
#' @param Trusted_Connection A Boolean. TRUE or FALSE on whether Windows Login is to be used
#' @param Port An Integer. The Port which the SQL Listener is listening at
#' @param ... If the relationships exist on the SQL Database, this can be left blank. Logical expressions for the SQL Relationships in the format: table1$FKcolumn1 == table2$IDcolumn5, table1$FKcolumn2 == table3$IDcolumn5, ...
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
initialise_return_db_fields <- function(csv_path, Driver, Database, Server, UID, PWD, Trusted_Connection, Port, ..., ForceCreate_csv = FALSE, ExcludeIdentities = FALSE, ExcludeForeignKeys = TRUE) {
  .GlobalEnv$db <- new.env()
  update_db_info(Driver, Database, Server, UID, PWD, Trusted_Connection, Port, ...)

  if (!ForceCreate_csv && file.exists(csv_path)) {
    cat(paste0("\nReading db_fields from: ", csv_path, "\n\n"))
    db_fields <- read_db_fields_csv(csv_path)
  } else {
    cat(paste0("\n'", "db_fields.csv", "' does not exist.\nCalling create_default_db_fields_from_db_con()\n"))
    db_fields <- create_default_db_fields_from_db_con(csv_path, ExcludeIdentities, ExcludeForeignKeys)
  }
  return(db_fields)
}


#' Creates the db Environment and the SQL Connection (db$con)
#'
#' Get a list of all the Main tables in the database joined with all their relationships with only Include == TRUE columns
#' @param Driver A string. A SQL Driver, etc.: {SQL Server};
#' @param Database A string. The name of the SQL Database itself
#' @param Server A string. A name that resolves into the SQL Machine, or the IP to the SQL Machine
#' @param UID A string. The Username credential for the SQL Connection. NULL if Windows Login is to be used
#' @param PWD A string. The Password credential for the SQL Connection. NULL if Windows Login is to be used
#' @param Trusted_Connection A Boolean. TRUE or FALSE on whether Windows Login is to be used
#' @param Port An Integer. The Port which the SQL Listener is listening at
#' @param ... If the relationships exist on the SQL Database, this can be left blank. Logical expressions for the SQL Relationships in the format: table1$FKcolumn2 == table2$IDcolumn1, table1$FKcolumn3 == table3$IDcolumn1, ...
#' @keywords SQL Join db SQL Connection
#' @export
#' @examples
#' main_joint_tables <- update_db_info(Driver = "{SQL Server};", Database = "Chronogramme", Server = '123.456.7.8', UID = NULL, PWD = NULL, Trusted_Connection = TRUE, Port = 1433,
#'                                     DIM_Employee$Employee_MainSiteMainObjekt_SiteID == DIM_Site$Site_SiteID,
#'                                     DIM_Site$Site_RegionID == DIM_Region$Region_ID
#'                                     )
update_db_info <- function(Driver, Database, Server, UID, PWD, Trusted_Connection, Port = 1433, ...) {
  db$con <- connect_odbc(Driver, Database, Server, UID, PWD, Trusted_Connection, Port)
  #odbcClose(db$con)

  #Retrieving the names of all the tables starting with DIM or FACT
  db$db_tables_names <- dbListTables(db$con) %>%
    grep("^(DIM|FACT)", .) %>%
    dbListTables(db$con)[.]

  #Getting a glimpse of all the tables
  cat("\nConnecting to the database and retrieving information...\n\n")
  db$lst_glimpses <- list()
  if (length(db$db_tables_names) > 0) {
    for (SQLTableCurNum in 1:length(db$db_tables_names)) {
      db$lst_glimpses[[db$db_tables_names[[SQLTableCurNum]]]] <-
        glimpse(tbl(db$con, db$db_tables_names[[SQLTableCurNum]])) %>%
        as.data.frame()
    }
  }

  ### Creating a Data Model Diagram ###
  sQuery <- datamodelr::dm_re_query("sqlserver") #TODO Implementations for non SQL Server databases as well (Postrgre already exists on package, see website)
  DataModel <- DBI::dbGetQuery(db$con, sQuery) %>% collect() %>% as.data.frame(stringsAsFactors = FALSE)

  if (any(!is.na(DataModel$ref))) { #Retrieving the Data Model Diagram from the Database
    cat("\nRelationships found on the SQL Database, we're ignoring manually written ones and rely on what is shown on the SQL Database\n")
    db$dm_f <- datamodelr::as.data_model(DataModel)

  } else { #Creating a Data Model Diagram for the SQL Database from the glimpses
    cat("\nNo relationships found on the SQL Database, relying on what has been typed manually\n")
    db$dm_f <- dm_from_data_frames(db$lst_glimpses)

    #Inserting the Relationships into the ER Diagram
    #                            MainTable$Ref == OtherTable$Identity
    # db$dm_f <- dm_add_references(db$dm_f,
    #                           TBL_IMPORT$IM_ID == SomeTable$SomeID,
    # )
    db$dm_f <- dm_add_references(db$dm_f, ...
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
  # db$db_forced_rel <- #If a Forced Rel Column Name is reneamed by db_ColumnsOldNamesToNewNames above, then use new name
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
#' @keywords ER Diagramme Diagram
#' @export
#' @examples
#' dbplyr_to_sql(tbl("TableName", db$con) db$con)
dbplyr_to_sql <- function(TibbleDbPointer, con = db$con) {
  SQL <- sql_render(sql_build(TibbleDbPointer, con), con)
  return(SQL)
}

connect_odbc <- function(Driver, Database, Server, UID, PWD, Trusted_Connection, Port = 1433) {
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
#' @keywords create db_fields dbfields
#' @export
#' @examples
#' db_fields <- create_default_db_fields_from_db_con("db_fields.csv")
create_default_db_fields_from_db_con <- function(csv_path, ExcludeIdentities = FALSE, ExcludeForeignKeys = TRUE) {
  ############################################## IDENTITY is more imprtant because multiple ForeignKeys might reference it, whilst ForeignKeys themselves are not relevant after the merge
  ### Creating a Feature Selection Interface ###
  ##############################################
  #== ASSUMES THAT update_db_info() HAS BEEN EXECUTED SUCCESSFULLY FIRST ==#

  if (is.null(csv_path)) csv_path <- "db_fields.csv"

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
  #  -Editing default values according to accrued knowledge:
  # Include[db$db_all_cols == "Customer_ID"] = SelectionOptions[1] #At any and all points, we need to be able to identify the Customer
  # Include[db$db_all_cols == "SomeColumnName"] = SelectionOptions[2] #The information contained here is better portrayed by AnotherColumnName

  KeyTypeOptions <- c("Identity", "ForeignKey", "None", "N/A")
  KeyType <- if_else(db$db_all_is_ident, KeyTypeOptions[1], if_else(!is.na(db$db_all_rel_cols), KeyTypeOptions[2], KeyTypeOptions[3]))

  RelationshipWithTable <- if_else(is.na(db$db_all_rel_tabs), "", as.character(db$db_all_rel_tabs))
  RelationshipWithColumn <- if_else(is.na(db$db_all_rel_cols), "", as.character(db$db_all_rel_cols))

  #Putting in Comments on the CSV
  Comments <- character(NROW(db$db_all_cols))
  # Comments[db_all_cols == "Some_ID"] <- "Identification for each type. Contains: All possible types"

  CommentsOnForcedRelationships <- character(NROW(db$db_forced_rel))

  db_fields <- data.frame(
    Include = factor(c(Include, rep.int(SelectionOptions[3], length(db$db_forced_rel))), levels = SelectionOptions),
    KeyType = c(KeyType, rep.int(KeyTypeOptions[4], length(db$db_forced_rel))),
    Table = c(db$db_all_tabs, db$db_all_tabs[match(names(db$db_forced_rel), db$db_all_cols)]),
    Column = c(db$db_all_cols, names(db$db_forced_rel)),
    Type = c(db$db_all_types, rep(KeyTypeOptions[4], length(db$db_forced_rel))),
    RelationshipWithTable = c(RelationshipWithTable, db$db_all_tabs[match(as.character(db$db_forced_rel), db$db_all_cols)]),
    RelationshipWithColumn = c(RelationshipWithColumn, as.character(db$db_forced_rel)),
    Transformation = "",
    Comment = c(Comments, CommentsOnForcedRelationships),
    stringsAsFactors = FALSE
  )

  write_db_fields_csv(db_fields, csv_path)

  return(db_fields)
}

#' Reads the csv file that is formatted according to the db_fields standard
#'
#' Returns the db_fields, a DF with columns: KeyType, Table, Column, Type, Comment, RelationshipWithTable, RelationshipWithColumn, Transformation
#' @param csv_path A String. The path of the db_fields .csv file.
#' @keywords Read db_fields dbfields
#' @export
#' @examples
#' db_fields <- read_db_fields_csv("db_fields.csv")
read_db_fields_csv <- function(csv_path) {
  db$db_fields <-
    read.csv(csv_path,
             header = TRUE,
             colClasses = c("KeyType" = "character", "Table" = "character", "Column" = "character", "Type" = "character",
                            "Comment" = "character", "RelationshipWithTable" = "character", "RelationshipWithColumn" = "character",
                            "Transformation" = "character"), stringsAsFactors = FALSE)
  db$db_fields$Include %<>%
    factor(levels=c("Yes", "No", "N/A"))
  return(db$db_fields)
}

#' Writes the csv file that is formatted according to the db_fields standard
#'
#' Creates a .csv file with columns: KeyType, Table, Column, Type, Comment, RelationshipWithTable, RelationshipWithColumn, Transformation
#' @param db_fields A DF. A dataframe formatted according to the db_fields standard.
#' @param csv_path A String. The path of the db_fields .csv file.
#' @keywords Write db_fields dbfields
#' @export
#' @examples
#' Example 1:
#' db_fields <- create_default_db_fields_from_db_con(csv_path)
#' write_db_fields_csv(db_fields, "db_fields.csv")
#'
#' Example 2:
#' db_fields <- read_db_fields_csv("db_fields.csv")
#' write_db_fields_csv(db_fields, "db_fields.csv")
write_db_fields_csv <- function(db_fields, csv_path) {
  write.csv(db_fields, csv_path, row.names = FALSE)
  cat(paste0("db_fields.csv is saved on: '", csv_path, "'"))
}

#' Edits a db_fields formatted DF
#'
#' Opens a browser window where the user can select which columns to include and see the relationships amongst the tables.
#' When the browser window closes, the results are saved on the DF, but NOT on the db_fields.csv file; for this use write_db_fields_csv()
#' @param db_fields A DF. A dataframe formatted according to the db_fields standard.
#' @keywords edit db_fields dbfields
#' @export
#' @examples
#' db_fields <- read_db_fields_csv("db_fields.csv")
#' edit_db_fields(db_fields)
#' write_db_fields_csv(db_fields, "db_fields.csv")
edit_db_fields <- function(db_fields = db$db_fields) {
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
          db$db_fields  <- hot_to_r(input$hot)
        } else {
          db$db_fields  <- samp$df
        }

        rhandsontable(db$db_fields, useTypes = TRUE, search = TRUE) %>%
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

  return(db$db_fields)
}
