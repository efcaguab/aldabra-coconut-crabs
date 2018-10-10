read_db <- function(x){
  Hmisc::mdb.get(x, tables = NULL) %>%
    lapply(dplyr::tbl_df) 
}