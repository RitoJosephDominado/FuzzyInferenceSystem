library(purrr)

s_membership_function <- function(p1, p2){
  function(a){
    right_indices <- which(p2 <= a)
    middle_indices <- which(p1 < a & a < p2)
    
    membership <- numeric(length(a))
    membership[right_indices] <- 1
    membership[middle_indices] <- (a[middle_indices] - p1)/(p2 - p1)
    membership
  }
}

z_membership_function <- function(p1, p2){
  function(a){
    left_indices <- which(a <= p1)
    middle_indices <- which(p1 < a & a < p2)
    membership <- numeric(length(a))
    membership[left_indices] <- 1
    membership[middle_indices] <- (p2 - a[middle_indices])/(p2 - p1)
    membership
  }
}


trapezoidal_membership_function <- function(p1, p2, p3, p4){
  function(a){
    left_indices <- which(p1 < a & a < p2)
    middle_indices <- which(p2 <= a & a <= p3)
    right_indices <- which(p3 < a & a < p4)
    
    membership <- numeric(length(a))
    membership[left_indices] <- (a[left_indices] - p1)/(p2 - p1)
    membership[middle_indices] <- 1
    membership[right_indices] <- (p4 - a[right_indices])/(p4 - p3)
    membership
  }
}

gaussian_membership_function <- function(gaussian_mean, gaussian_sd){
  function(a){
    exp(-((a - gaussian_mean)/gaussian_mean_sd)^2)
  }
}

negation_membership_function <- function(membership_function){
  function(a) 1 - membership_function(a)
}

union_membership_function <- function(...){
  function(a){
    membership_list <- map(list(...), ~.x(a))
    do.call(pmax, membership_list)
  }
}

intersection_membership_function <- function(...){
  function(a){
    membership_list <- map(list(...), ~.x(a))
    do.call(pmin, membership_list)
  }
}