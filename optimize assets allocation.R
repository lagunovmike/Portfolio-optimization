library(lpSolve)

money_have <- 820
adjprice <- price_data_best %>%
    filter(symbol %in% wts_best$Asset) %>%
    group_by(symbol) %>%
    select(symbol, adjusted) %>%
    filter(row_number() == n())

wghts <- round(extractWeights(maxSR.lo.ROI),4)
wghts <- wghts[names(wghts) %in% wts_best$Asset]
C <- adjprice$adjusted
ind_weight <- as.numeric(diag(wghts))
A <- matrix(c(wghts, ind_weight, C), ncol = length(wghts), byrow = T)
B <- c(1,wghts+0.4, money_have)
constranints_direction  <- c(rep("<=", nrow(A)))


optimum <-  lp(direction="max",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)


qnty <- optimum$solution
names(qnty) <- wts_best$Asset
qnty
optimum$objval
props <- C*qnty / 375





## Set the coefficients of the decision variables -> C
## Cost=30∗4P+40∗3P+80∗WoodenBlock
t_names <- c("FRHC", "POLY.ME", "AMD", "AAPL")
C <- c(69, 196, 127.6, 150)

# Create constraint martix B
A <- matrix(c(0.0841, 0.2390, 0.1556, 0.1829,
              0.0841,0,0,0,
              0,0.2390,0,0,
              0,0,0.1556,0,
              0,0,0,0.1829,
              69, 196, 127.6, 150), nrow=6, byrow=TRUE)
A

# Right hand side for the constraints
B <- c(1, 0.51, 0.3, 0.26, 0.2, 820)

# Direction of the constraints
constranints_direction  <- c("<=", "<=", "<=", "<=", "<=", "<=")

optimum <-  lp(direction="max",
               objective.in = C,
               const.mat = A,
               const.dir = constranints_direction,
               const.rhs = B,
               all.int = T)
qnty <- optimum$solution
names(qnty) <- t_names
qnty
optimum$objval
props <- C*qnty / 820
props

