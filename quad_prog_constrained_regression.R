# Quadratic Programming for Humans ----
# How to make a constrained regression using quadratic programming in R

# Credit to Berwin Turlach for providing the basis for this example on:
# https://stat.ethz.ch/pipermail/r-help/2008-March/155990.html

# Data & libraries
library(MASS)     ## to access the Boston data
library(quadprog) # The package for the quadratic computation

Boston  <- Boston[, c(14, 1:3)] # We are going to limit the data to just three columns 


# Remembering our OLS object:
# - to minimize the sum of squared residuals: sum((y - y_hat)^2)
# We find that the best beta is
# beta_hat = (X' X)^(-1) X' Y

# First we build component from OLS
x_mat <- model.matrix(medv~., data=Boston) # our X
xt_x  <- crossprod(x_mat, x_mat)       # Xt X
xt_y  <- crossprod(x_mat, Boston$medv) # Xt Y

# We would then find our betas through
beta_hat = solve(t(x_mat)%*%x_mat)%*%(t(x_mat)%*%Boston$medv)

round(t(beta_hat), digits = 2) # Control 1
round(lm(medv~., data = Boston)$coefficients, digits = 2) # Control 2

# quadprog allows us to restrain this using a constrain matrix A, our beta vector and b_0, a 
# constraint value vector, so that:
# t(A)*beta >= b_0


# Example 1 ----
# a simple example that constrains all beta to be positiv or 0
Amat <- diag(NROW(xt_x))   # diagonal matrix with dimension as our explanatory vars
bvec <- rep(0,NROW(xt_x))  # a vector of zeros, same length as number of x vars
meq  <- rep(0,NROW(x_mat)) # 1 or 0 for length of bvec, 1 treats constraint as equality, 0 as inequality
res <- solve.QP(Dmat = xt_x, dvec = xt_y, Amat = Amat, bvec = bvec)
round(res$solution, digits = 2) # We see all betas positive or zero


# Example 2 ----
# Example, beta 2 (first var after intercept) negative, rest positive or 0
Amat <- matrix(data = c(
    1,    0,    0,    0,
    0,    -1,   0,    0,
    0,    0,    1,    0,
    0,    0,    0,    1
), byrow = T, ncol = 4)
       # diagonal matrix with dimension as our explanatory vars
bvec <- rep(0,NROW(xt_x))  # a vector of zeros, same length as number of x vars
meq  <- rep(0,NROW(x_mat)) # 1 or 0 for length of bvec, 1 treats constraint as equality, 0 as inequality
res <- solve.QP(Dmat = xt_x, dvec = xt_y, Amat = Amat, bvec = bvec)
round(res$solution, digits = 2) # We see all betas positive or zero



# Example 3 ----
# Example, beta 2 (first var after intercept) = .05, rest positive or 0
Amat <- matrix(data = c(
    0,    0,    0,    1,
    1,    0,    0,    0,
    0,    1,    0,    0,
    0,    0,    1,    0
), byrow = T, ncol = 4)
# diagonal matrix with dimension as our explanatory vars
bvec <- c(.05,0,0,0)  # a vector of zeros, same length as number of x vars
meq  <- c(1,0,0,0) # 1 or 0 for length of bvec, 1 treats constraint as equality, 0 as inequality
res <- solve.QP(Dmat = xt_x, dvec = xt_y, Amat = Amat, bvec = bvec)
round(res$solution, digits = 2) # We see all betas positive or zero



# Example 4 ----
# sum of beta 2 and beta 4 equal 2.5, rest unconstrained
# Example, beta 2 (first var after intercept) = .05, rest positive or 0
Amat <- matrix(data = c(
    0,    
    1,    
    0,    
    1
), byrow = T, ncol = 1)

# diagonal matrix with dimension as our explanatory vars
bvec <- c(2.5)  # a vector of zeros, same length as number of x vars
meq  <- c(1) # 1 or 0 for length of bvec, 1 treats constraint as equality, 0 as inequality
res <- solve.QP(Dmat = xt_x, dvec = xt_y, Amat = Amat, bvec = bvec)
round(res$solution, digits = 2) # We see all betas positive or zero
round(res$solution[2] + res$solution[4], digits = 2)


# Example 1 ----
# sum of beta 2 and beta 4 equal 2.5
# beta 1 positive
# rest unconstrained
# Example, beta 2 (first var after intercept) = .05, rest positive or 0
Amat <- matrix(data = c(
    0, 1,    
    1, 0,   
    0, 0,   
    1, 0
), byrow = T, ncol = 2)

# diagonal matrix with dimension as our explanatory vars
bvec <- c(2.5, 0)  # a vector of zeros, same length as number of x vars
meq  <- c(1, 0) # 1 or 0 for length of bvec, 1 treats constraint as equality, 0 as inequality
res <- solve.QP(Dmat = xt_x, dvec = xt_y, Amat = Amat, bvec = bvec)
round(res$solution, digits = 2) # We see all betas positive or zero
round(res$solution[2] + res$solution[4], digits = 2)
