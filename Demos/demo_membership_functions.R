rm(list = ls())

source('Main/membership_functions.R')

a <- s_membership_function(1, 35)
b <- z_membership_function(5, 30)
d <- trapezoidal_membership_function(10, 15, 25, 40)

rng <- 1:40

plot(a(rng), type = 'l')
lines(b(rng), col = 'red')
lines(d(rng), col = 'blue')


e <- union_membership_function(a, b, d)
f <- intersection_membership_function(b, d)
g <- negation_membership_function(e)

plot(e(rng), col = 'orange', type = 'l', ylim = c(0, 1))
lines(f(rng), col = 'green')
lines(g(rng), col = 'purple', ylim = c(0, 1))

