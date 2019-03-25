get_crab_master_df <- function(crab_tbl, collection_event, dist_shore){
  crab_tbl %>%
    dplyr::full_join(collection_event) %>%
    dplyr::filter(!is.na(locality)) %>%
    dplyr::full_join(dist_shore) %>%
    dplyr::mutate(sex_l = sex == "male",
                  month = lubridate::yday(date),
                  dist_shore_l = as.numeric(dist_shore > 50)) 
  
 
}

model_sex_ratios <- function(crab_master_df, n = 100){
  sex_model <- mgcv::gam(sex_l ~ 
                           # s(as.numeric(date), k = 5) +
                           s(month, k = 12, bs = "cc") +
                           s(month, k = 12, bs = "cc", by = dist_shore_l) +
                           s(moon_ph, k = 5, bs = "cc"),
                         data = crab_master_df, 
                         select = crab_master_df$t_length > 28,
                         family = "binomial", 
                         gamma = 1.4)
  
  sex_model_plot <- plot(sex_model, pers = T, n = n, n2 = n, pages = 1)
  
  list(mod = sex_model, 
       pred = sex_model_plot)
}

