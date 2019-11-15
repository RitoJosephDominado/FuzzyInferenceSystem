rm(list = ls())


source('Main/FuzzyInferenceSystem.R')
source('Main/fuzzy_sets.R')
source('Main/linguistic_variable.R')
source('Main/membership_functions.R')
source('Main/fuzzy_propositions.R')
source('Main/translate_fuzzy_proposition.R')
source('Main/fuzzy_proposition_environments.R')

a1 <- z_fuzzy_set(20, 90)
a2 <- s_fuzzy_set(25, 85)

a <- linguistic_variable(
  name = 'a',
  xlim = c(0, 100),
  fuzzy_set_list = list(
    'a1' = a1, 
    'a2' = a2
  )
)


b <- linguistic_variable(
  name = 'b',
  xlim = c(20, 100),
  b1 = gaussian_fuzzy_set(30, 40),
  b2 = gaussian_fuzzy_set(80, 40)
)



is_a1 <- simple_fuzzy_proposition('a', 'a1')
is_not_a1 <- simple_fuzzy_proposition('a', 'a1', negated = TRUE)
is_a2 <- simple_fuzzy_proposition('a', 'a2')
is_b1 <- simple_fuzzy_proposition('b', 'b1')
is_b2 <- simple_fuzzy_proposition('b', 'b2')

is_a2_or_b1 <- union_fuzzy_proposition(
  simple_fuzzy_proposition('a', 'a2'),
  simple_fuzzy_proposition('b', 'b1')
)

is_a1_and_not_b2 <- intersection_fuzzy_proposition(
  'is a1' = simple_fuzzy_proposition('a', 'a1'),
  'is NOT b2' = simple_fuzzy_proposition('b', 'b2', negated = TRUE)
)

is_either <- union_fuzzy_proposition(
  is_a2_or_b1, 
  is_a1_and_not_b2
)


fis <- FuzzyInferenceSystem$new()

fis$linguistic_variable_list <- list(
  'a' = a, 
  'b' = b
)

fis$fuzzy_proposition_list <- list(
  'is a1' = is_a1,
  'is not a1' = is_not_a1,
  'is a2' = is_a2,
  'is b1' = is_b1,
  'is b2' = is_b2,
  'is a2 or b1' = is_a2_or_b1,
  'is a1 and not b2' = is_a1_and_not_b2,
  'is either' = is_either
)



input_df <- data.frame(
  a = c(23, 70, 60, 80),
  b = c(54, 90, 70, 90)
)

fis$evaluate_fuzzy_proposition_list(input_df) %>% rhandsontable()

fis$linguistic_variable_list

fis$fuzzy_proposition_list$`is a1`
fis$linguistic_variable_list$a$fuzzy_set_list
fis$evaluate_fuzzy_proposition(is_a1, input_df)


par(mfrow = c(4, 2))
fis$plot_feature(input_df[1,], 'a')
fis$plot_feature(input_df[1,], 'b')
fis$plot_feature(input_df[2,], 'a')
fis$plot_feature(input_df[2,], 'b')


fis$plot_feature(input_df[3,], 'a')
fis$plot_feature(input_df[3,], 'b')
fis$plot_feature(input_df[4,], 'a')
fis$plot_feature(input_df[4,], 'b')




json <- read_json('json/Fuzzy inference system for test 1.json')
fis_list <- json[[1]] %>% fromJSON(simplifyDataFrame = FALSE)
fis <- fis_list %>% convert_list_to_FuzzyInferenceSystem()
x <- fis$fuzzy_proposition_list$`is not a1` %>% convert_fuzzy_proposition_to_environment()
x$fuzzy_set_name
x$negated

q <- list(is_a1, is_a2)
w <- intersection_fuzzy_proposition_environment(c(q, negated = TRUE))
w <- do.call(intersection_fuzzy_proposition_environment, c(q, negated = TRUE))
w$negated
