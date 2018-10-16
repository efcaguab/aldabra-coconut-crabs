get_distance_to_shore <- function(picard, back_path, coast_path){
  
  # claculate distance to shore
  get_distance <- . %>%
    dplyr::select(1,2) %>%
    as.matrix() %>%
    geosphere::dist2Line(picard) %>%
    as.data.frame() %$%
    distance
  
  back_path %<>% 
    dplyr::mutate(dist_shore = get_distance(.)) %>%
    dplyr::mutate(area = "BP")
  coast_path %<>% 
    dplyr::mutate(dist_shore = get_distance(.)) %>%
    dplyr::mutate(area = "CP")
  
  # put data together and save
  dplyr::bind_rows(back_path, coast_path) %>%
    dplyr::mutate(locality = as.character(locality)) 
}
