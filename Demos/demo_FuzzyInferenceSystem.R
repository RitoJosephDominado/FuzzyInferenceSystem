rm(list = ls())


source('Main/FuzzyInferenceSystem.R')
source('Main/linguistic_variable.R')
source('Main/membership_functions.R')
source('Main/fuzzy_propositions.R')


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

feature_df <- data.frame(
  weight = c(52, 76, 81, 55),
  height = c(154, 167, 177, 151)
)
rownames(feature_df) <- c('bob', 'dee', 'charlie', 'frank')

fis <- FuzzyInferenceSystem$new()
fis$linguistic_variable_list <- list(height = height, weight = weight)

fis$fuzzy_proposition_list[['is_short']] <- simple_fuzzy_proposition('height', 'short')
fis$fuzzy_proposition_list[['is_medium_ht']] <- simple_fuzzy_proposition('height', 'medium')
fis$fuzzy_proposition_list[['is_tall']] <- simple_fuzzy_proposition('height', 'tall')
fis$fuzzy_proposition_list[['is_light']] <- simple_fuzzy_proposition('weight', 'light')
fis$fuzzy_proposition_list[['is_medium_wt']] <- simple_fuzzy_proposition('weight', 'medium')
fis$fuzzy_proposition_list[['is_heavy']] <- simple_fuzzy_proposition('weight', 'heavy')

fis$fuzzy_proposition_list[['is_short_light']] <- intersection_fuzzy_proposition(
  simple_fuzzy_proposition('height', 'short'),
  simple_fuzzy_proposition('weight', 'light')
)

fis$fuzzy_proposition_list[['is_short_or_light']] <- union_fuzzy_proposition(
  simple_fuzzy_proposition('height', 'short'),
  simple_fuzzy_proposition('weight', 'light')
)

fis$evaluate_fuzzy_proposition_list(feature_df = feature_df)

fis$plot_feature(feature_df[3,], 'height', 100:220)
feature_df


fp_list <- fis$fuzzy_proposition_list

fis$evaluate_fuzzy_proposition(fp_list$is_medium_ht, feature_df[1,])
fis$evaluate_fuzzy_proposition(fp_list$is_tall, feature_df[1,])
