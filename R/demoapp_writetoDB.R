#' Write Files to a SQLite Database
#'
#' @param data Data frame object that you wish to be a table in the database 
#' @param name A string representing the name of the table that the user wishes to write
#' @param database_dir A string with the path directory to the database
#' @param overwrite A logical argument as to whether the user wishes to overwrite any existing data with the same name
#' @param append A logical argument as to whether the user wishes to append new data on existing data with the same name
#'
#' @return A retrievable data table in the desired database
#' @export 
#'
#' @examples 
#' demoapp_writetoDB(data = df_automotive,
#' name = "automotive_df",
#' database_dir = "demoApp_db.sqlite",
#' overwrite = FALSE,
#' append = FALSE)

demoapp_writetoDB <- function(data,
                              name = "",
                              database_dir = "",
                              overwrite = FALSE,
                              append = FALSE) {
  # connect to the database
  database <- DBI::dbConnect(RSQLite::SQLite(), database_dir)
  
  # write the data table to the database
  DBI::dbWriteTable(value = data, name = , name, conn = database, overwrite = overwrite, append = append)
  
  # safely disconnect from the database
  DBI::dbDisconnect(database)
}