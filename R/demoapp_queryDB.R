#' Query SQLite Database for a Selected Data Table
#'
#' @param selected_table The table inside of the Database the user wishes to query
#' @param database_path A string with the path directory to the database, the default is set to whatever the current working directory is using `getwd()`, searching for `database.sqlite` in that project directory.
#' @param columns For if the user wishes to only query select columns, the default retrieves all columns or the user can select `columns = "all"`, but a character vector can be supplied like so: `columns = c("text", "sentiment", "permalink")` if wanting to be selective.
#'
#' @return The queried data table from a selected SQLite database
#' @export
#'
#' @examples
#' automotive_df <- demoapp_queryDB(selected_table = "automotive_df")
#'
#' automotive_selected_cols <- demoapp_queryDB(selected_table = "automotive_df",
#'  columns = c("text", "sentiment", "permalink"))

demoapp_queryDB <- function(selected_table = "",
                            database_path = NULL,
                            columns = "all") {
  
  # determine the database path based on the provided directory
  if (is.null(database_path)) {
    database_path <- file.path(getwd(), "database.sqlite")
    message("No specific database directory provided. Querying from 'database.sqlite' within the default project directory: ", database_path)
  } else {
    database_path <- file.path(database_path)
    message("Querying from the specified database directory: ", database_path)
  }
  
  # check if the database exists
  if (!file.exists(database_path)) {
    stop("Database file does not exist at: ", database_path)
  }
  
  # connect to the database
  connected_database <- DBI::dbConnect(RSQLite::SQLite(), database_path)
  message("Connected to the database...")
  
  # define the select statement based on the columns argument
  if (is.null(columns) || columns == "all") {
    select_statement <- paste("SELECT * FROM", selected_table)
  } else {
    select_statement <- paste("SELECT", paste(columns, collapse = ", "), "FROM", selected_table)
  }
  
  # execute the query and retrieve data
  result <- DBI::dbGetQuery(conn = connected_database,
                            statement = select_statement)
  message("Query executed successfully. Data retrieved from: '", selected_table, "'")
  
  # disconnect from the database
  DBI::dbDisconnect(connected_database)
  
  return(result)
}