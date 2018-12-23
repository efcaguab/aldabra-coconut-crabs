# Turn on packrat
if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)

# load functions
f <- lapply(list.files("code", full.names = T), source)

# SET UP PLAN -------------------------------------------------------------

data_preprocessing_plan <- drake::drake_plan(
  tb = read_db(drake::file_in("./data/raw/CoconutCrab_be.mdb")), 
  back_path = read_back_path(drake::file_in("./data/raw/back_path.csv")),
  coast_path = read_coast_path(drake::file_in("./data/raw/coastal_path.csv")),
  picard = rgdal::readOGR(drake::file_in("./data/raw/picard_shapefile/pi.shp")),
  loc = list(lon = 46.2063, lat = -9.3897), 
  wind_tbl = process_wind(tb), 
  rain_tbl = process_rain(tb),
  area_tbl = process_area(tb), 
  collection_event = process_collection_event(tb, wind_tbl, rain_tbl, area_tbl, loc),
  morph_tbl = process_morph(tb),
  moult_tbl = process_moult(tb),
  sex_tbl = process_sex(tb),
  crab_tbl = process_crab(tb, morph_tbl, moult_tbl, sex_tbl),
  habitat = read_habitat(drake::file_in("data/raw/habitat_back_path.csv"), drake::file_in("data/raw/habitat_coastal_path.csv"))
)

habitat_plan <- drake::drake_plan(
  habitat_distance = dplyr::inner_join(habitat, dist_shore), 
  habitat_pca = get_habitat_pca(habitat_distance), 
  habitat_pca_fig = plot_habitat_pca(habitat_pca, habitat_distance), 
  habitat_pca_components = dplyr::bind_cols(habitat_distance, dplyr::as_data_frame(habitat_pca$x))
)

models_plan <- drake::drake_plan(
  length_weight = get_length_weight_relationship(crab_tbl),
  dist_shore = get_distance_to_shore(picard, back_path, coast_path), 
  crab_master_df = get_crab_master_df(crab_tbl, collection_event, dist_shore),
  sex_ratio_models = model_sex_ratios(crab_master_df), 
  size_models = model_size(crab_master_df), 
  count_models = model_counts(crab_master_df, collection_event, dist_shore), 
  moult_models = model_moults(crab_master_df, collection_event, dist_shore), 
  reproduction_models = model_reproduction(crab_tbl, collection_event)
)

density_plan <- drake::drake_plan(
  detectability_abundance_model = model_detectability_abundance(crab_tbl, collection_event, habitat_pca_components, dist_shore), 
  # best model does not include environmental coovariates and has a halfnormal detection function (abu9)
  best_detectability_abundance_model = determine_best_detectability_abundance_model(detectability_abundance_model), 
  abundance_per_day = calculate_abundance_per_day(detectability_abundance_model), 
  env_effect_plots = check_env_effect_detectability(detectability_abundance_model), 
  abundance_from_density_model_plot = plot_abundance_from_density_model(abundance_per_day), 
  density_models = model_density(abundance_per_day, collection_event), 
  density_fig = plot_density(density_models)
)

save_figures_plan <- drake::drake_plan(
  ggplot2::ggsave(drake::file_out("figs/habitat-pca.pdf"), habitat_pca_fig, width = 4.7, height = 3.3, scale = 1.5),
  ggplot2::ggsave(drake::file_out("figs/density.pdf"), density_fig, width = 3.18, height = 4.5, scale = 1)
)

project_plan <- rbind(
  data_preprocessing_plan, 
  habitat_plan,
  models_plan, 
  density_plan,
  save_figures_plan
)

# RUN PROJECT --------------------------------------------------------------

project_config <- drake::drake_config(plan = project_plan)

drake::make(project_plan)

