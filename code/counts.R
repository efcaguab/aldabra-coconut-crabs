model_counts <- function(crab_master_df, collection_event, dist_shore, n = 362/2){
  crab_master_df %<>%
    dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12))
  
  sexes <- c("female", "male")
  
  count_models <- sexes %>%
    plyr::llply(function(x){
      d <- crab_master_df %>%
        dplyr::filter(!is.na(sex)) %>%
        dplyr::group_by(col_id, area, locality) %>% 
        dplyr::summarise(count = sum(sex %in% x)) %>% 
        dplyr::full_join(collection_event) %>%
        dplyr::full_join(dist_shore) %>%
        dplyr::mutate(month = lubridate::yday(date)) %>%
        dplyr::group_by()
      
      mgcv::gam(count ~ 
                  # s(as.numeric(date), k = 5) +
                  s(month, k = 12, bs = "cc") +
                  s(dist_shore, k = 3, bs = "tp") +
                  ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp")) +
                  s(moon_ph, k = 3, bs = "cc"),
                # s(moon_ph, k = 5, bs = "cc", by = dist_shore_l),
                # rain + 
                #n_people,
                family = mgcv::nb(),
                data = d, 
                gamma = 1.4)
    })
  
  n <- 362/2
  count_model_plots <- lapply(count_models, plot, pages = 1, pers = T, n = n, n2 = n)
  
  list(mod = count_models, 
       pred = count_model_plots) 
}

compare_count_models <- function(crab_master_df, collection_event, dist_shore, n = 362/2){
  
  crab_master_df %<>%
    dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12))
  
  sexes <- c("female", "male")
  formulas <- list(all = formula(count ~ 
                                   s(month, k = 12, bs = "cc") +
                                   s(dist_shore, k = 3, bs = "tp") +
                                   ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp")) +
                                   s(moon_ph, k = 3, bs = "cc")), 
                   no_moon = formula(count ~ 
                                       s(month, k = 12, bs = "cc") +
                                       s(dist_shore, k = 3, bs = "tp") +
                                       ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp"))), 
                   no_month = formula(count ~ 
                                        s(dist_shore, k = 3, bs = "tp") +
                                        s(moon_ph, k = 3, bs = "cc")), 
                   no_distance = formula(count ~ 
                                           s(month, k = 12, bs = "cc") +
                                           s(moon_ph, k = 3, bs = "cc")))
  
  data <- purrr::map(c("female", "male"), function(x){
    crab_master_df %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::group_by(col_id, area, locality) %>% 
      dplyr::summarise(count = sum(sex %in% x)) %>% 
      dplyr::full_join(collection_event) %>%
      dplyr::full_join(dist_shore) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::group_by()
  }) %>% purrr::set_names(c("female", "male"))
  
  aics <- purrr::cross2(data, formulas) %>%
    purrr::map(function(x){
      mgcv::gam(x[[2]], 
                family = mgcv::nb(),
                data = x[[1]], 
                gamma = 1.4)
    }) %>%
    purrr::map_dbl(AIC)
  
  purrr::cross2(names(data), names(formulas)) %>%
    purrr::map_dfr(~tibble::tibble(sex = .[[1]], variables = .[[2]])) %>%
    dplyr::mutate(aic = aics)
  
}
