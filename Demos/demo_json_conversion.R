getwd()
rm(list = ls())
source('Main/FuzzyInferenceSystem.R')
source('Main/json_conversion.R')
source('Main/fuzzy_sets.R')
source('Main/linguistic_variable.R')

source('Main/membership_functions.R')
source('Main/fuzzy_propositions.R')

height <- linguistic_variable(
  name = 'height',
  xlim = c(130, 250),
  short = z_fuzzy_set(150, 160), 
  medium = trapezoidal_fuzzy_set(145, 160, 175, 190),
  tall = s_fuzzy_set(170, 200)
)

weight <- linguistic_variable(
  name = 'weight',
  xlim = c(40, 110),
  light = z_fuzzy_set(50, 65),
  medium = trapezoidal_fuzzy_set(45, 60, 80, 90),
  heavy = s_fuzzy_set(75, 95)
)

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
str(fis$fuzzy_proposition_list)
str(fis$linguistic_variable_list)


feature_df <- data.frame(
  weight = c(52, 76, 81, 55),
  height = c(154, 167, 177, 151)
)
rownames(feature_df) <- c('bob', 'dee', 'charlie', 'frank')

fis$evaluate_fuzzy_proposition_list(feature_df)

fis_list <- convert_FuzzyInferenceSystem_to_list(fis)
fis$linguistic_variable_list %>% str
fis_list %>% str
fis_from_list <- convert_list_to_FuzzyInferenceSystem(fis_list)
fis_from_list$linguistic_variable_list %>% str
fis_list$linguistic_variable_list


fis$evaluate_fuzzy_proposition_list(feature_df)
fis_from_list$evaluate_fuzzy_proposition_list(feature_df)

fis_from_list$linguistic_variable_list %>% str

# ----
fis_list$linguistic_variable_list
reactjson(fis_list$linguistic_variable_list)

fis_list %>% toJSON(pretty = TRUE) %>% write_json('Demos/demo2.json')
?write_json

fis_list %>% toJSON(pretty = TRUE) %>% write('Demos/ok.json')

demo1_path <- 'Data/demo1.json'
json <- read_json('Data/demo1.json', simplifyVector = FALSE)

json[[1]]
fis_list_from_json <- jsonlite::fromJSON(txt = json[[1]])
json
fis_list_from_json$linguistic_variable_list
reactjson(fis_list_from_json$linguistic_variable_list$height)
fis_list %>% reactjson

m <- fis_list %>% convert_list_to_FuzzyInferenceSystem()
a <- jsonlite::serializeJSON(fis_list)
reactjson(a)
a
b <- unserializeJSON(a)
d <- convert_list_to_FuzzyInferenceSystem(b)
d$evaluate_fuzzy_proposition_list(feature_df)
fis$evaluate_fuzzy_proposition_list(feature_df)
m$evaluate_fuzzy_proposition_list(feature_df)


str(fis$fuzzy_proposition_list)
str(m$fuzzy_proposition_list)

str(fis$linguistic_variable_list$height)
str(m$linguistic_variable_list)

reactjson(fis$linguistic_variable_list$height)
reactjson(m$linguistic_variable_list$height)

m$evaluate_fuzzy_proposition(simple_fuzzy_proposition('height', 'medium'), feature_df = feature_df)
fis$evaluate_fuzzy_proposition(simple_fuzzy_proposition('height', 'medium'), feature_df = feature_df)

fis$linguistic_variable_list$height
m$linguistic_variable_list$height

fis_list_from_json %>% reactjson
fis_from_json <- convert_list_to_FuzzyInferenceSystem(fis_list_from_json)
fis_from_json$fuzzy_proposition_list
fis_from_json$evaluate_fuzzy_proposition_list(feature_df)
fis_from_json$fuzzy_proposition_list



a <- read_file('Demos/ok.json') %>% fromJSON(simplifyDataFrame = FALSE)
a
b <- a %>% convert_list_to_FuzzyInferenceSystem()
b$evaluate_fuzzy_proposition_list(feature_df)
reactjson(b$linguistic_variable_list$height$fuzzy_set_list$short$p1)
b$fuzzy_proposition_list$is_short_light$argument_list
