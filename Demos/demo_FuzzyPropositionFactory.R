

source('Main/membership_functions.R')
source('Main/linguistic_variable.R')
source('Main/FuzzyPropositionFactory.R')

linguistic_variable_list <- list(
  weight = linguistic_variable(
    light = create_z_membership_function(10, 20),
    normal = create_trapezoidal_membership_function(25, 35, 40, 60),
    heavy = create_s_membership_function(45, 70)
  ),
  
  height = linguistic_variable(
    short = create_z_membership_function(150, 160),
    tall = create_s_membership_function(150, 160)
  )
)


fuzzy_proposition_factory <- FuzzyPropositionFactory$new(linguistic_variable_list = linguistic_variable_list)

bob <- c(weight = 13, height = 152)
fuzzy_proposition_factory$plot_feature(b, 'weight', 1:50)


is_bob_heavy <- fuzzy_proposition_factory$create_evaluation_fuzzy_proposition('weight', 'light')
is_bob_heavy(bob)
