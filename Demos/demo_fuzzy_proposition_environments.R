rm(list = ls())
source('Main/fuzzy_proposition_environments.R')
source('Main/fuzzy_propositions.R')

is_heavy_env <- simple_fuzzy_proposition_environment('weight', 'heavy')

is_heavy <- convert_environment_to_fuzzy_proposition(is_heavy_env)
is_heavy
reactjson(is_heavy_env)
reactjson(is_heavy)
is_heavy_env %>% as.list()

b <- is_heavy %>% as.environment
str(b %>% as.list)


is_heavy_tall_env <- union_fuzzy_proposition_environment(
  simple_fuzzy_proposition_environment('weight', 'heavy'),
  simple_fuzzy_proposition_environment('height', 'tall')
)

a <- is_heavy_tall_env %>% as.list

for(i in a){
  print(i %>% as.list)
}


b <- convert_environment_to_fuzzy_proposition(is_heavy_tall_env)
str(b)
str(a)
b
b
d <- convert_fuzzy_proposition_to_environment(b)
e <- convert_environment_to_fuzzy_proposition(d)
e$argument_list
