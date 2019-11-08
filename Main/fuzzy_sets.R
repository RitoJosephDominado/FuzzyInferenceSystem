z_fuzzy_set <- function(p1, p2){
  list(
    type = 'z_fuzzy_set',
    p1 = p1,
    p2 = p2,
    membership_function = z_membership_function(p1, p2)
  )
}

s_fuzzy_set <- function(p1, p2){
  list(
    type = 's_fuzzy_set',
    p1 = p1,
    p2 = p2,
    membership_function = s_membership_function(p1, p2)
  )
}

trapezoidal_fuzzy_set <- function(p1, p2, p3, p4){
  list(
    type = 'trapezoidal_fuzzy_set',
    p1 = p1,
    p2 = p2,
    p3 = p3,
    p4 = p4,
    membership_function = trapezoidal_membership_function(p1, p2, p3, p4)
  )
}

gaussian_fuzzy_set <- function(gaussian_mean, gaussian_sd){
  list(
    type = 'gaussian_fuzzy_set',
    gaussian_mean = gaussian_mean,
    gaussian_sd = gaussian_sd,
    membership_function = gaussian_membership_function(gaussian_mean, gaussian_sd)
  )
}