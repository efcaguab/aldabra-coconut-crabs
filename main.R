# Turn on packrat
if(!packrat:::isPackratModeOn()) packrat::on()
pkgconfig::set_config("drake::strings_in_dots" = "literals")

library(magrittr)

# load functions
f <- lapply(list.files("code", full.names = T), source)

# SET UP PLAN -------------------------------------------------------------

data_preprocessing_plan <- drake::drake_plan(
  tb = read_db("./data/raw/CoconutCrab_be.mdb"), 
  loc = list(lon = 46.2063, lat = -9.3897)
)

project_plan <- rbind(
  data_preprocessing_plan
)

# RUN PROJECT --------------------------------------------------------------

project_config <- drake::drake_config(plan = project_plan)

drake::make(project_plan)
