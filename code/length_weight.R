# Calculates a standarised major axis relationshop between the weight and the total length

get_length_weight_relationship <- function(cc){

  # normal vs corrected weight
  we_we <- lm(c_weight ~ weight, data = cc)
  # ∆- corrected weight is normal weight - 85 grams, but later I figured out that
  # for the ones that is not filled the weight field might have already been
  # corrected because there are weights smaller than 85
  
  # final model -------------------------------------------------------------
  
  smatr::sma(c_weight ~ t_length, data = cc, log = "xy", robust = T)
}