# Turn on packrat
packrat::on(project = ".")

# SET UP PLAN -------------------------------------------------------------

data_preprocessing_plan <- drake::plan(
  db = Hmisc::mdb.get("./data/raw/CoconutCrab_be.mdb", tables = NULL)
)

project_plan <- rbind(
  data_preprocessing_plan
)


# RUN PROJECT --------------------------------------------------------------

project_config <- drake::drake_config(plan = project_plan)

drake::make(project_plan)
