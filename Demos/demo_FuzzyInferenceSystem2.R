rm(list = ls())
source('Main/FuzzyInferenceSystem.R')
source('Main/linguistic_variable.R')
source('Main/membership_functions.R')
source('Main/fuzzy_propositions.R')
source('Main/fuzzy_sets.R')


fis <- FuzzyInferenceSystem$new()

a <- linguistic_variable(
  name = 'a', xlim = c(0, 100),
  fuzzy_set_list = list(
    a1 = z_fuzzy_set(10, 90),
    a2 = s_fuzzy_set(10, 100)
  )
)

fp1 <- simple_fuzzy_proposition('a', 'a2',negated = TRUE)


fis$fuzzy_proposition_list <- list(
  fp1 = simple_fuzzy_proposition('a', 'a1', negated = FALSE),
  fp2 = simple_fuzzy_proposition('a', 'a2', negated = FALSE),
  fp3 = intersection_fuzzy_proposition(
    simple_fuzzy_proposition('a', 'a1', negated = FALSE),
    simple_fuzzy_proposition('a', 'a2', negated = FALSE)
  ),
  fp4 = intersection_fuzzy_proposition(
    simple_fuzzy_proposition('a', 'a1', negated = FALSE),
    simple_fuzzy_proposition('a', 'a2', negated = FALSE), negated = TRUE
  )
)



fis$linguistic_variable_list <- list(
  a = a
)


feature_df <- data.frame(a = c(30, 59, 20))
feature_df

fis$evaluate_fuzzy_proposition_list(feature_df)

