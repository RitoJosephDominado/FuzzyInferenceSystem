library(R6)

FuzzyPropositionFactory <- R6Class(
  classname = 'FuzzyPropositionFactory',
  public= list(
    linguistic_variable_list = NULL,
    
    initialize = function(linguistic_variable_list){
      self$linguistic_variable_list <- linguistic_variable_list
    }
  )
)



FuzzyPropositionFactory$set('public', 'create_evaluation_fuzzy_proposition', function(linguistic_variable_name, fuzzy_set_name){
  function(features){
    membership_function <- self$linguistic_variable_list[[linguistic_variable_name]][[fuzzy_set_name]]
    membership_function(features[linguistic_variable_name])
  }
})

FuzzyPropositionFactory$set('public', 'create_union_fuzzy_proposition', function(...){
  function(features){
    map(list(...), ~.x(features)) %>% unlist %>% max
  }
})

FuzzyPropositionFactory$set('public', 'create_intersection_fuzzy_proposition', function(...){
  function(features){
    map(list(...), ~.x(features)) %>% unlist %>% min
  }
})


FuzzyPropositionFactory$set('public', 'create_negation_fuzzy_proposition', function(fuzzy_proposition){
  function(features) 1 - fuzzy_proposition(features)
})

FuzzyPropositionFactory$set('public', 'plot_feature', function(features, linguistic_variable_name, rng){
  linguistic_variable <- self$linguistic_variable_list[[linguistic_variable_name]]
  num_fuzzy_sets <- length(linguistic_variable())
  plot(rng, linguistic_variable[[1]], type = 'o')
  for(i in 2:num_fuzzy_sets){
    lines(rng, linguistic_variable[[i]](rng), type = 'o')
  }
  abline(v = features[linguistic_variable_name], col = 'red', lwd = 2)
})