# read database
read_db <- function(x){
  Hmisc::mdb.get(x, tables = NULL) %>%
    lapply(dplyr::tbl_df) 
}

# read and preprocess file with sections and coordinates of back path
read_back_path <- function(x){
  read.csv(x) %>%
    dplyr::select(X, Y, name) %>%
    get_mid_section()
}

# read and preprocess file with sections and coordinates of coastal path
read_coast_path <- function(x){
  read.csv(x) %>%
    dplyr::select(X, Y, name) %>%
    dplyr::mutate(name = as.character(name), 
                  name = stringr::str_extract(name, "([0-9])+"),
                  name = as.numeric(name)) %>%
    get_mid_section()
}

# calculate mid-section locations of transects
get_mid_section <- function(x)
{ 
  x %>%
    dplyr::mutate(length = tr_length(.)) %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(X = (X + dplyr::lag(X))/2,
                  Y = (Y + dplyr::lag(Y))/2) %>%
    dplyr::filter(!is.na(X)) %>%
    dplyr::rename(locality = name)
}

# transect length
tr_length <- function(x) {
  l <- vector(mode = "double", length = nrow(x))
  for(i in 2:nrow(x)){
    l[i] <- geosphere::distVincentyEllipsoid(c(x$X[i], x$Y[i]), 
                                             c(x$X[i-1], x$Y[i-1]))
  }
  l
}

process_wind <- function(tb){
  tb$tblWind %>%
    dplyr::rename(wind_id = ID,
                  wind = Wind.strength) %>%
    dplyr::mutate(wind_id = as.character(wind_id),
                  wind = as.character(wind))
}

process_rain <- function(tb){
  tb$tblRain %>%
    dplyr::rename(rain_id = ID,
                  rain = Conditions) %>%
    dplyr::mutate(rain_id = as.character(rain_id),
                  rain = c("dry", "rain", "wet")) %>%
    dplyr::select(-Description)
}

process_area <- function(tb){
  tb$tblArea %>%
    dplyr::rename(area_id = ID,
                  area = Code,
                  area_name = Name.) %>%
    dplyr::mutate(area_id = as.character(area_id),
                  area = as.character(area),
                  area_name = as.character(area_name)) %>%
    dplyr::select(area_id, area, area_name)
}

process_collection_event <- function(tb, wind, rain, area, loc){
  tb$tblColEvent %>%
    dplyr::rename(col_id = CollectionEventNumber,
                  date = ColEventDate, 
                  area_id = Area,
                  t_start = TimeStart,
                  t_end = TimeEnd,
                  cloud_cover = CloudCover,
                  wind_id = WindStrength,
                  rain_id = Rain,
                  n_people = NumberPeople) %>%
    dplyr::mutate(col_id = as.character(col_id),
                  date = as.Date(date, "%m/%d/%y %H:%M:%S"),
                  t_start = as.character(t_start),
                  t_start = replace(t_start, t_start == "", "20:00:00"),
                  t_start = stringr::str_sub(t_start, start = -8),
                  t_start = as.POSIXct(paste(date, t_start), 
                                       format = "%Y-%m-%d %H:%M:%S",
                                       tz = "Indian/Mahe"),
                  t_start = replace(t_start,
                                    difftime(t_start, trunc(t_start, "days"), units = "secs") < 3600 * 12,
                                    t_start[difftime(t_start, trunc(t_start, "days"), units = "secs") < 3600 * 12] + 3600*12),
                  t_end = stringr::str_sub(as.character(t_end), 10),
                  t_end = as.POSIXct(paste(date, t_end), 
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "Indian/Mahe"),
                  t_end = replace(t_end, is.na(t_end),
                                  t_start[is.na(t_end)] + 45 *60),
                  area_id = as.character(area_id),
                  cloud_cover = cloud_cover/100,
                  moon_if = oce::moonAngle(as.POSIXlt(t_start, tz = "UTC"), 
                                           longitude = loc$lon,
                                           latitude = loc$lat)$illuminatedFraction,
                  moon_ph = oce::moonAngle(as.POSIXlt(t_start, tz = "UTC"), 
                                           longitude = loc$lon,
                                           latitude = loc$lat)$phase,
                  moon_ph = moon_ph - floor(moon_ph),
                  wind_id = as.character(wind_id),
                  rain_id = as.character(rain_id)) %>%
    dplyr::full_join(wind, by = "wind_id") %>%
    dplyr::full_join(rain, by = "rain_id") %>%
    dplyr::full_join(area, by = "area_id") %>%
    dplyr::select(-Moon, -wind_id, -rain_id, -RecordersInitials, -DataEnterer,
                  -DataChecker, -area_id)
}

process_morph <- function(tb){
tb$tblMorphColour %>%
  dplyr::rename(morph_id = ID,
                morph_name = MorphColour,
                morph = MorphCode) %>%
  dplyr::mutate(morph_id = as.character(morph_id),
                morph = as.character(morph),
                morph_name = as.character(morph_name))
}

process_moult <- function(tb){
  tb$tblMoultStatus %>%
    dplyr::rename(moult = MS.code,
                  moult_name = MS.Name.) %>%
    dplyr::mutate(moult = as.integer(moult),
                  moult_name = as.character(moult_name)) %>%
    dplyr::select(moult, moult_name)
}

process_sex <- function(tb){
  tb$tblSex %>%
    dplyr::rename(sex_id = ID,
                  sex = Sex) %>%
    dplyr::mutate(sex_id = as.character(sex_id),
                  sex = as.character(sex))
}

process_crab <- function(tb, morph, moult, sex){
  tb$tblCrab %>%
  dplyr::rename(number = Entry.number,
                col_id = CollectionEventNumber,
                locality = Locality,
                distance = Distance,
                resight = Resight, 
                mark = MR.ID,
                t_length = Mid.to.mid,
                weight = Weight,
                c_weight = Corrected.weight,
                sex_id = Sex,
                moult = Moult.status,
                morph_id = Morph,
                comments = Comments,
                a_comments = Analysis.comments,
                tt_length = Tip.to.tip,
                rostrum_length = Rostrum,
                width = Width) %>%
  dplyr::mutate(col_id = as.character(col_id),
                locality = as.character(locality),
                distance = as.integer(distance),
                resight = as.logical(resight),
                mark = as.character(mark),
                t_length = as.numeric(t_length),
                weight = as.numeric(weight),
                c_weight = as.numeric(c_weight),
                sex_id = as.character(sex_id),
                moult = as.integer(moult),
                moult = replace(moult, moult == 0, NA),
                morph_id = as.character(morph_id),
                comments = as.character(comments),
                a_comments = as.character(a_comments)) %>%
  dplyr::full_join(morph, by = "morph_id") %>%
  dplyr::full_join(moult, by = "moult") %>%
  dplyr::full_join(sex, by = "sex_id") %>%
  dplyr::select(-MarkType,
                -Recorders,
                -Location.original,
                -Mark,
                -sex_id,
                -morph_id) %>%
  # standarise  corrected weight
  dplyr::mutate(c_weight = replace(c_weight,
                                   is.na(c_weight),
                                   weight[is.na(c_weight)])) %>%
  # remove 1mm crab...
  dplyr::filter(t_length > 1)
}
