library(magrittr)
library(drake)

pkgconfig::set_config("drake::strings_in_dots" = "literals")
# load functions
f <- lapply(list.files("code", full.names = T), source)

# SET UP PLAN -------------------------------------------------------------

data_preprocessing_plan <- drake_plan(
  tb = read_db(file_in("./data/raw/CoconutCrab_be.mdb")), 
  back_path = read_back_path(file_in("./data/raw/back_path.csv")),
  coast_path = read_coast_path(file_in("./data/raw/coastal_path.csv")),
  picard = rgdal::readOGR(file_in("./data/raw/picard_shapefile/pi.shp")),
  loc = list(lon = 46.2063, lat = -9.3897), 
  wind_tbl = process_wind(tb), 
  rain_tbl = process_rain(tb),
  area_tbl = process_area(tb), 
  collection_event = process_collection_event(tb, wind_tbl, rain_tbl, area_tbl, loc),
  morph_tbl = process_morph(tb),
  moult_tbl = process_moult(tb),
  sex_tbl = process_sex(tb),
  crab_tbl = process_crab(tb, morph_tbl, moult_tbl, sex_tbl),
  habitat = read_habitat(file_in("data/raw/habitat_back_path.csv"), file_in("data/raw/habitat_coastal_path.csv")),
  habitat_simple = read_habitat_simple("data/raw/habitat_types_picard.csv")
)

habitat_plan <- drake_plan(
  habitat_distance = dplyr::inner_join(habitat, dist_shore), 
  habitat_pca = get_habitat_pca(habitat_distance), 
  habitat_pca_fig = plot_habitat_pca(habitat_pca, habitat_distance), 
  habitat_pca_components = dplyr::bind_cols(habitat_distance, dplyr::as_data_frame(habitat_pca$x))
)

models_plan <- drake_plan(
  length_weight = get_length_weight_relationship(crab_tbl),
  dist_shore = get_distance_to_shore(picard, back_path, coast_path), 
  crab_master_df = get_crab_master_df(crab_tbl, collection_event, dist_shore),
  sex_ratio_models = model_sex_ratios(crab_master_df), 
  size_models = model_size(crab_master_df), 
  model_size_comparison = compare_size_models(crab_master_df),
  moult_models = model_moults(crab_master_df, collection_event, dist_shore), 
  reproduction_models = model_reproduction(crab_tbl, collection_event),
  count_models = model_counts(crab_master_df, collection_event, dist_shore), 
  count_model_comparison = compare_count_models(crab_master_df, collection_event, dist_shore),
  seasonality_count_size_fig = plot_seasonality_counts_size(count_models, size_models),
  moon_count_size_fig = plot_moon_counts_size(count_models, size_models), 
  moon_count_size_data_fig = plot_moon_data(count_models, size_models),
  size_distribution_fig = plot_size_distribution(crab_tbl), 
  moult_fig = plot_moulting(moult_models), 
  moult_data_fig = plot_moulting_data(moult_models)
)

density_plan <- drake_plan(
  detectability_abundance_model = model_detectability_abundance(crab_tbl, collection_event, habitat_simple, dist_shore), 
  det_ab_model_global = model_detectability_abundance_global(crab_tbl, collection_event, habitat_simple, dist_shore), 
  # best model does not include environmental coovariates and has a halfnormal detection function (abu9)
  best_detectability_abundance_model = determine_best_detectability_abundance_model(detectability_abundance_model), 
  abundance_per_day = calculate_abundance_per_day(detectability_abundance_model), 
  det_per_day = calculate_detectability_per_day(detectability_abundance_model, habitat_simple),
  env_effect_plots = check_env_effect_detectability(detectability_abundance_model), 
  abundance_from_density_model_plot = plot_abundance_from_density_model(abundance_per_day), 
  density_models = model_density(abundance_per_day, collection_event), 
  density_fig = plot_density(density_models), 
  reproduction_fig = plot_reproduction(reproduction_models)
)

save_figures_plan <- drake_plan(
  # ggplot2::ggsave(file_out("figs/habitat-pca.tiff"), habitat_pca_fig, width = 4.7, height = 3.3, scale = 1.5),
  ggplot2::ggsave(file_out("figs/density.tiff"), density_fig, width = 3.18, height = 4.5, scale = 1, dpi = 300),
  ggplot2::ggsave(file_out("figs/seasonality_count_size.tiff"), seasonality_count_size_fig, width = 6.65, height = 4.5), 
  ggplot2::ggsave(file_out("figs/moon_count_size.tiff"), moon_count_size_fig, width = 3.18, height = 4.5),
  ggplot2::ggsave(file_out("figs/moon_count_size_data.tiff"), moon_count_size_data_fig, width = 3.18*2, height = 4.5),
  ggplot2::ggsave(file_out("figs/size_distribution.tiff"), size_distribution_fig, width = 3.18, height = 4.5/2),
  ggplot2::ggsave(file_out("figs/moulting.tiff"), moult_fig, width = 3.18, height = 4.5/2),
  ggplot2::ggsave(file_out("figs/moulting_data.tiff"), moult_data_fig, width = 3.18 * 2, height = 4.5/2),
  ggplot2::ggsave(file_out("figs/reproduction.tiff"), reproduction_fig, width = 3.18, height = 4.5/2)
)

manuscript_plan <- drake_plan(
  rmarkdown::render(knitr_in("paper/supp_info.Rmd"))
)

project_plan <- rbind(
  data_preprocessing_plan, 
  habitat_plan,
  models_plan, 
  density_plan,
  save_figures_plan
)

# RUN PROJECT --------------------------------------------------------------

project_config <- drake_config(plan = project_plan)

make(project_plan)

