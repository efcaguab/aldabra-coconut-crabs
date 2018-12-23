
# recruitment -------------------------------------------------------------

library(magrittr)
library(mgcv)

drake::loadd(crab_tbl, collection_event, dist_shore)
cc <- crab_tbl
col_event <- collection_event
loc_dist <- dist_shore

breaks <- c(0, 20, 30, 80)

m_r_c <- breaks[-1] %>%
  plyr::llply(function(x){
    data <- cc %>%
      dplyr::mutate(t_bin = cut(t_length, breaks = breaks, labels = breaks[-1]),
                    t_bin = as.numeric(as.character(t_bin))) %>% 
      dplyr::group_by(col_id) %>% 
      dplyr::summarise(count = sum(t_bin == x)) %>% 
      dplyr::full_join(col_event) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::filter(area %in% c("BP", "CP"))
    
    mgcv::gam(count ~ 
                s(as.numeric(date), k = 5) +
                s(month, k = 10, bs = "cc") +
                s(moon_ph, k = 10, bs = "cc"), 
              family = nb(),
              data = data , 
              gamma = 1.4)
  })

m_r_p <- breaks[-1] %>%
  plyr::llply(function(x){
    data <- cc %>%
      dplyr::mutate(t_bin = cut(t_length, breaks = breaks, labels = breaks[-1]),
                    t_bin = as.numeric(as.character(t_bin))) %>% 
      dplyr::group_by(col_id) %>% 
      dplyr::summarise(count = sum(t_bin == x), 
                       no_count = n() - count) %>% 
      dplyr::full_join(col_event) %>%
      dplyr::mutate(month = lubridate::yday(date)) %>%
      dplyr::filter(area %in% c("BP", "CP"))
    
    mgcv::gam(cbind(count, no_count) ~ 
                s(as.numeric(date), k = 5) +
                s(month, k = 10, bs = "cc") +
                s(moon_ph, k = 10, bs = "cc"), 
              family = "binomial",
              data = data , 
              gamma = 1.4)
  })

plot(m_r_c[[3]])
plot(m_r_p[[3]])

# mark recapture ----------------------------------------------------------

library(magrittr)
library(mgcv)

mu <- dplyr::mutate
se <- dplyr::select
gr <- dplyr::group_by
fi <- dplyr::filter
su <- dplyr::summarise
re <- dplyr::rename
ar <- dplyr::arrange

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc %>% dplyr::inner_join(col_event) %>%
  fi(mark != "") %$% table(area, resight)

wanderers <- cc %>% dplyr::inner_join(col_event) %>%
  fi(mark != "") %>%
  gr(mark) %>% 
  su(n_areas = dplyr::n_distinct(area),
     n_resights = sum(resight)) %>%
  ar(-n_areas) %T>% View %>%
  fi(n_areas > 1) %$% mark

cc %>% 
  dplyr::inner_join(col_event) %>%
  fi(mark %in% wanderers) %>%
  ar(mark, date) %>% 
  se(date, area_name, mark, resight, t_length) %>% View


# competition -------------------------------------------------------------

library(unmarked)
library(magrittr)
library(foreach)
library(ggplot2)

mu <- dplyr::mutate
se <- dplyr::select
gr <- dplyr::group_by
fi <- dplyr::filter
su <- dplyr::summarise
re <- dplyr::rename
ar <- dplyr::arrange

cc <- readRDS(file = "./data/processed/crabs.rds")
col_event <- readRDS(file = "./data/processed/col_events.rds")
loc_dist <- readRDS(file = "./data/processed/locality_distances.rds")

cc %>%
  dplyr::inner_join(col_event) %>%
  fi(!is.na(locality)) %>%
  gr(date, locality) %>%
  su(lm = sum(t_length > 50 & sex == "male"),
     sm = sum(t_length < 30 & sex == "male"),
     fe = sum(sex == "female")) %>%
  fi(!(lm == 0 & sm == 0), 
     !(lm == 0 & fe == 0),
     !(sm == 0 & fe == 0)) %>%
  mu(month = lubridate::month(date)) %>%
  ggplot(aes(x = lm, y = sm, colour = month)) + 
  geom_count() +
  geom_smooth(method = "glm", method.args = list(family = "poisson")) + 
  facet_wrap(~month)
