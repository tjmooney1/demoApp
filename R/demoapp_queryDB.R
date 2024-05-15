#' Query SQLite Database for Data
#'
#' @param selected_table The table inside of the Database the user wishes to query
#' @param database_dir A string with the path directory to the database
#' @param columns For if the user wishes to only query select columns, the default retrieves all columns or the user can select `columns = "all"`, but a character vector can be supplied like so: `columns = c("text", "sentiment", "permalink")` for being selective.
#'
#' @return The queried table as a data frame with selected columns if provided
#' @export
#'
#' @examples
#' automotive_df_queried <- demoapp_queryDB(selected_table = "automotive_df",
#'  database_dir = "demoApp_db.sqlite")
#'
#' automotive_df_queried <- demoapp_queryDB(selected_table = "automotive_df",
#'  database_dir = "demoApp_db.sqlite",
#'  columns = c("text", "sentiment", "permalink"))

demoapp_queryDB <- function(selected_table = "",
                            database_dir = "",
                            columns) {
  # connect to the database
  database <- DBI::dbConnect(RSQLite::SQLite(), database_dir)
  
  # if-else statement to define the select statement and query
  if (missing(columns) || "all" %in% columns) {
    select_statement <- paste("SELECT * FROM", selected_table)
  } else {
    select_statement <- paste("SELECT", paste(columns, collapse = ", "), "FROM", selected_table)
  }
  
  # execute the query and retrieve data as selected
  result <- DBI::dbGetQuery(conn = database, statement = select_statement)
  
  # safely disconnect from the database
  DBI::dbDisconnect(database)
  
  return(result)
}