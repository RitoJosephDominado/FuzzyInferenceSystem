source('Main/linguistic_variable.R')
source('Main/membership_functions.R')
source('Main/fuzzy_propositions.R')
source('Main/translate_fuzzy_proposition.R')


height <- linguistic_variable(
  short = z_membership_function(150, 160),
  medium = trapezoidal_membership_function(145, 160, 175, 190),
  tall = s_membership_function(170, 200)
)

weight <- linguistic_variable(
  light = z_membership_function(50, 65),
  medium = trapezoidal_membership_function(45, 60, 80, 90),
  heavy = s_membership_function(70, 100)
)


is_light <- simple_fuzzy_proposition('weight', 'light')
is_short <- simple_fuzzy_proposition('height', 'short')

is_light_short <- intersection_fuzzy_proposition(
  is_light, is_short
)

is_not_light_short <- negation_fuzzy_proposition(is_light_short)

translate_fuzzy_proposition(is_light)
translate_fuzzy_proposition(is_short)
translate_fuzzy_proposition(is_light_short)
translate_fuzzy_proposition(is_not_light_short)

a <- translate_fuzzy_proposition(
  union_fuzzy_proposition(
    is_not_light_short, is_light
  )
)
library(listviewer)
reactjson(is_not_light_short)
a
