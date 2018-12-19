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

model_size <- function(crab_master_df, n = 362/2){
  crab_master_df %<>%
    dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12))
  
  sexes <- c("female", "male")
  
  size_models <- plyr::llply(sexes, function(x){
    data <- dplyr::filter(crab_master_df, sex == x)
    mgcv::gam(t_length ~ 
                # s(as.numeric(date), k = 5) +
                s(month, bs = c("cc"), k = 12) +
                s(dist_shore, k = 3, bs = "tp") + 
                ti(month, dist_shore, k = c(12, 3), bs = c("cc", "tp")) +
                s(moon_ph, bs = "cc", k = 5),
              # s(moon_ph, bs = "cc", k = 3, by = dist_shore_l) +
              # rain, 
              data = data, 
              gamma = 1.4)
  })
  
  n <- 362/2
  size_model_plots <- lapply(size_models, plot, pers = T, n = n, n2 = n, pages = 1)
  
  list(mod = size_models, 
       pred = size_model_plots)
}

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

model_moults <- function(crab_tbl, collection_event, dist_shore, n = 362){
  
  sexes <- c("female", "male")
  
  moult_models <- sexes %>%
    plyr::llply(function(x){
      data <- crab_tbl %>%
        dplyr::filter(t_length > 30, 
                      sex == x) %>%
        dplyr::group_by(col_id) %>% 
        dplyr::full_join(collection_event) %>%
        dplyr::mutate(month = lubridate::yday(date)) %>%
        dplyr::filter(area %in% c("BP", "CP")) %>%
        dplyr::filter(!is.na(locality)) %>%
        dplyr::full_join(dist_shore) %>%
        dplyr::filter(!(area == "CP" & as.numeric(locality) <= 12),
                      date > as.Date("2010-01-01"))
      
      mgcv::gam(moult ~ 
                  # s(as.numeric(date), k = 5) +
                  # ti(month, dist_shore, bs = c("cc", "cr"), k = c(5, 3)) +
                  # s(dist_shore, k = 2, bs = "cr") +
                  # s(moon_ph, bs = "cc", k = 3) +
                  s(month, k = 12, bs = "cc"),
                data = data , 
                gamma = 1.4)
    })
  
  moult_model_plots <- lapply(moult_models, plot, pers = T, n = n, n2 = n, pages = 1)
  
  
  list(mod = moult_models, 
       pred = moult_model_plots) 
}

model_reproduction <- function(crab_tbl, collection_event){
  cc_m <- crab_tbl %>%
    dplyr::filter(sex == "female") %>%
    dplyr::mutate(egg = grepl("gg", comments) & !grepl("Dragging", comments)) %>%
    dplyr::group_by(col_id) %>%
    dplyr::summarise(egg = any(egg)) %>%
    dplyr::full_join(collection_event) %>%
    dplyr::mutate(month = lubridate::yday(date)) %>%
    dplyr::filter(area %in% c("BP", "CP"))
  
  m_e <- mgcv::gam(egg ~ 
                     # s(as.numeric(date), k = 5) +
                     # ti(month, dist_shore, bs = c("cc", "cr"), k = c(12, 3)) +
                     s(month, k = 12, bs = "cc") +
                     # s(dist_shore, k = 2, bs = "cr") +
                     s(moon_ph, bs = "cc", k = 3), 
                   data = cc_m, 
                   family = "binomial", 
                   gamma = 1.4)
  
  
  n <- 362
  m_e_p <- plot(m_e, pers = T, n = n, n2 = n, pages = 1)
  
  list(mod = m_e, 
       pred = m_e_p) 
}

