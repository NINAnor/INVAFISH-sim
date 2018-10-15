#' Download and store inndata from the NOFA database.
#'
#' @param serveradress URL of the server wher the database is located.
#' @param datafolder Relative or absolute path of the folder where the data should be stored locally.
#' @return A data.frame stored as .rds object named view_occurrence_by_event.rds
#' @examples
#' get_inndata(serveradress="my-server.no",datafolder="~/temp")
#'
#' @import dplyr
#' @import dbplyr
#' @export
#'

get_inndata <- function(serveradress,datafolder){

  # creating db connection object
  nofa_db <- dplyr::src_postgres(host=serveradress,
                          dbname="nofa",
                          user=rstudioapi::askForPassword("Please enter your user"),
                          password=rstudioapi::askForPassword("Please enter your psw"),
                          options="-c search_path=nofa")

  #.........................................................................
  # Get fish data from occurrence_by_event view ---------------------------
  #..........................................................................

  # Define SQL queries for getting data (data are in view, so raw dpyr
  # syntax does not work)
  name <-"SELECT * FROM nofa.view_occurrence_by_event"
  # download data
  view_occurrence_by_event <- tbl(nofa_db, sql(name)) %>% collect()
  # save to disk as RData
  path_to_outdatafile <- paste(datafolder,"/view_occurrence_by_event.rds",sep="")
  saveRDS(view_occurrence_by_event,path_to_outdatafile)
  RPostgreSQL::dbDisconnect(nofa_db$con)
}
