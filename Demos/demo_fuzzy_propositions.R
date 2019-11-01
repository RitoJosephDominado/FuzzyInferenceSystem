rm(list = ls())
source('Main/linguistic_variable.R')
source('Main/fuzzy_propositions.R')
source('Main/membership_functions.R')


height <- linguistic_variable(
  short = z_membership_function(150, 160), 
  medium = trapezoidal_membership_function(145, 160, 175, 190),
  tall = s_membership_function(170, 200)
)

weight <- linguistic_variable(
  light = z_membership_function(50, 65),
  medium = trapezoidal_membership_function(45, 60, 80, 90),
  heavy = s_membership_function(75, 95)
)

rng <- 140:210

library(purrr)

plot(height$short(rng), type = 'l')
lines(height$medium(rng), type = 'l')
lines(height$tall(rng), type = 'l')


weight_range <- 1:100
plot(weight$light(weight_range), type = 'l')
lines(weight$medium(weight_range), type = 'l')
lines(weight$heavy(weight_range), type = 'l')



evaluate_fuzzy_proposition <- function(fuzzy_proposition, feature_df){
  if(fuzzy_proposition$type == 'simple_fuzzy_proposition'){
    linguistic_variable_name <- fuzzy_proposition$linguistic_variable_name
    fuzzy_set_name <- fuzzy_proposition$fuzzy_set_name
    membership_function <- linguistic_variable_list[[linguistic_variable_name]][[fuzzy_set_name]]
    
    membership_df <- membership_function(feature_df[, linguistic_variable_name])
    
    return(membership_df)
  }else if(fuzzy_proposition$type == 'union_fuzzy_proposition'){
    result_list <- map(fuzzy_proposition$argument_list, evaluate_fuzzy_proposition, feature_df = feature_df)
    return(do.call(pmax, result_list))
  }else if(fuzzy_proposition$type == 'intersection_fuzzy_proposition'){
    result_list <- map(fuzzy_proposition$argument_list, evaluate_fuzzy_proposition, feature_df = feature_df)
    return(do.call(pmin, result_list))
  }else if(fuzzy_proposition$type == 'negation_fuzzy_proposition'){
    return(1 - evaluate_fuzzy_proposition(fuzzy_proposition$argument, feature_df))
  }else{
    stop('Not a fuzzy proposition')
  }
}



linguistic_variable_list <- list(
  weight = weight, height = height
)



bob <- c(weight = 58, height = 167)

feature_df <- data.frame(
  weight = c(52, 76, 81, 55),
  height = c(154, 167, 177, 151)
)

rownames(feature_df) <- c('A', 'B', 'D', 'E')

is_short <- simple_fuzzy_proposition('height', 'short')

is_heavy <- simple_fuzzy_proposition('weight', 'heavy')

a <- evaluate_fuzzy_proposition(is_short, feature_df = feature_df)
evaluate_fuzzy_proposition(is_heavy, feature_df = feature_df)
feature_df


fp_list <- list('is_short' = is_short, 'is_heavy' = is_heavy)
b <- map(fp_list, evaluate_fuzzy_proposition, feature_df = feature_df)
names(b) <- names(fp_list)
b
d <- do.call(cbind, b)
d
colnames(d)
d
colnames(d) <- names(fp_list)
d

fp_list$is_short %>% evaluate_fuzzy_proposition(feature_df = feature_df)

a <- do.call(pmax, b)
b
a

1- a
is_light_short <- union_fuzzy_proposition(
  simple_fuzzy_proposition('weight', 'light'),
  simple_fuzzy_proposition('height', 'short')
)

evaluate_fuzzy_proposition(
  simple_fuzzy_proposition('weight', 'light'),
  feature_df
)

a <- evaluate_fuzzy_proposition(
  simple_fuzzy_proposition('height', 'short'),
  feature_df
)

evaluate_fuzzy_proposition(is_light_short, feature_df = feature_df)

r <- map(is_light_short$argument_list, evaluate_fuzzy_proposition, feature_df = feature_df)
do.call(pmax, r)

neg <- negation_fuzzy_proposition(is_light_short)
neg
b <- evaluate_fuzzy_proposition(neg, feature_df)
b
