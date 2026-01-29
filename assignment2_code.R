################################################################################
### Quantitative Finance
### Home Assignment 2
################################################################################

rm(list = ls())
set.seed(123)

# In the German market various index certificates are traded. From a legal 
# perspective a certificate is a bond which guarantees a certain stream of 
# payments to its holders. In the standard case (straight bond) 100% of the 
# notional value is paid at the maturity of the bond and there are fixed annual 
# coupon payments. For an index certificate the payment at maturity and/or the
# coupon payments depend on the development of the DAX. 

# All instruments are traded in a frictionless market with a continuously 
# compounded riskless money market account with an annualized rate of 3%

# The starting value of the DAX is 23,500

# The DAX follows a Geometric Brownian Motion with a drift rate of 8.7% under 
# the physical measure and a volatility of 14%

# Each certificate has a maturity of five years

# Parameters
S_0 <- 23500
drift <- 0.087
sigma <- 0.14
r_free <- 0.03

# Task 1 - European Call option
K <- 25000
M <- 60
T <- 5
N <- 5000
delta <- T/M

# Create a MxN empty matrix to filled with normal random numbers
Z <- matrix(rnorm(M*N), nrow = M, ncol = N)

# Build a small helper function to see the dimensions of a matrix in a nice way
dimension <- function(X) {
  cat("The matrix has", dim(X)[1], "rows and", dim(X)[2], "columns")
}

dimension(Z)

# Multiply Z by a diagonal matrix filled with -1 on the diagonal with the same 
# dimensions as Z (from the left) to get antithetic numbers and attach to Z
Z <- cbind(Z, diag(-1, nrow = M) %*% Z)
dimension(Z)

# Check if it worked out
Z[1:3,1:3]
Z[1:3, (1+N):(3+N)]
# It worked :)

# Initialize empty (M+1)x(2*N) matrix to store the DAX prices
# We want to have M+1 rows so that we can store the price at t_0 in the first row
# We want to have 2*N columns as we have to account for the antithetic numbers
S <- matrix(NA, nrow = M+1, ncol = 2*N)
dimension(S)

# Now fill the first row with the t_0 price of the DAX
S[1,] <- S_0

# Calculate drift and volatility shock
drift <- (r_free-0.5*sigma^2)*delta
vol_shock <- sigma*sqrt(delta)

# Calculate the prices USING VECTORIZATION 
for (t in 2:(M+1)) {
  S[t, ] <- S[t-1, ] * exp(Z[t-1, ]*vol_shock + drift)
}

# Now we have the distribution of DAX prices at t_60
{
hist(S[61,], 
     breaks = 30,
     main   = "Histogram of DAX price draws at maturity",
     xlab   = "Price (EUR)",
     col    = 'lightblue',
     border = 'blue')

mean_S_maturity <- mean(S[61,])

abline(v   = mean(S[61,]),
       col = "firebrick",
       lwd = 3)

legend("topright", 
       legend = paste("Mean price =", round(mean_S_maturity, 2), "EUR"), 
       col    = "firebrick", 
       lwd    = 3,  
       pch = 19,
       bty    = "n",     
       cex    = 1.2)     
}
# Build a helper function for the european call payoff
european_call_payoff <- function(X, K) {
  C <- max(X - K, 0)
  return(C)
}

# Create a vector with the draws of the final call value given the payoff func
C_T <- sapply(S[61, ], function(X) european_call_payoff(X, K))
C_0 <- exp(-r_free*T)*mean(C_T)

cat("The present value of the European Call option written on the DAX is", C_0, 
    "EUR")

# Task 2 - Bonus certificate
# The coupons C at dates t are 10% if the DAX is above a lower boundary
# and zero otherwise
# Terminal payment is 100%
# Bonus boundary is 28 000

### WRONG !!!
# Let S_i denote the price of the DAX at time i, i in [0,60].
# Let BB denote the bonus boundary of the certificate.
# Moreover, let C_t denote the coupon payments at time t, t in [1,5].
# Then C_t = 0.1*S_(12*i) if S_(12*i) > BB and C_t = 0 otherwise.
# We know that the present value of the coupon payment is
# C_t_0 = exp(-r.t)*C_t
# Then by the no-arbitrage principle we can say that the present price of this
# certificate has to be equal to the present value of all its future cash flows/
# payments, i.e.
# C_0 = sum from t=1 to 5 of C_t_0 + PV(C_0) =
#     = sum from t=1 to 5 of C_t_0 + e^(-r*5)*C_0
# Then by subtracting e^(-r*5)*C_0 from both sides we get
# C_0(1 - e^(-r*5)) = sum from t=1 to 5 of C_t_0
# Then by dividing both sides by 1 - e^(-r*5) we get
# c_0 = (1 - e^(-r*5))^(-1) * sum from t=1 to 5 of C_t_0
### WRONG !!!


########## CORRECT
# Let S_i denote the price of the underlying at time i, where i in [0,M].
# Let N denote the notional price of the certificate
# Let BB denote the bonus boundary of the certificate.
# Let T denote the terminal date.
# Moreover, let C_t denote the coupon payments at time t, t in [1,T].
# Then C_t = 0.1*N if S_(M/T*i) > BB and C_t = 0 otherwise.
# We know that the present value of the coupon payment is
# C_t_0 = exp(-r.t)*C_t
# Then by the no-arbitrage principle we can say that the present price of this
# certificate has to be equal to the present value of all its future cash flows/
# payments, i.e.
# C_0 = sum from t=1 to T of C_t_0 + N * e^(-r*T)
###########

# Let S_k denote the price of the underlying at step k, where k in [0,M].
# Let N denote the notional price of the certificate (e.g. 100).
# Let BB denote the bonus boundary.
# Let T denote the terminal maturity in years (e.g. 5).
# Let M denote the total number of time steps (e.g. 60).

# Loop t from 1 to T:
#    1. Identify the simulation step corresponding to year t:
#       Step_Index = (M / T) * t    (e.g., 12 * 1 = 12)
#
#    2. Check the condition:
#       If S_Step_Index > BB:
#           C_t = 0.10 * N  [cite: 24]
#       Else:
#           C_t = 0
#
#    3. Discount to present value:
#       C_t_0 = C_t * exp(-r * t)
#
#    4. Add to running total:
#       Total_Sum = Total_Sum + C_t_0

# Add the discounted Terminal Payment (100% of Notional)[cite: 26]:
# Path_Value = Total_Sum + (N * exp(-r * T))

S_coupon_dates <- S[c(12,24,36,48,60)+1, ]
BB <- 28000
Notional <- 100
coupon_payment_matrix <- matrix(NA, nrow = dim(S_coupon_dates)[1], 
                                ncol = dim(S_coupon_dates)[2])
dimension(coupon_payment_matrix)
for (i in 1:length(S_coupon_dates)) {
  coupon_payment_matrix[i] <- ifelse(S_coupon_dates[i] > BB, 0.1*Notional, 0)
}
coupon_payment_matrix[dim(S_coupon_dates)[1], ] <- (
  coupon_payment_matrix[dim(S_coupon_dates)[1], ] + Notional)

discounting_vector <- c()
for (i in 1:T) {discounting_vector <- c(discounting_vector, exp(-r_free*i))}

# Multiply discounting vector by the coupon payments matrix to obtain 
# a vector with the sum of present value of all future cash flows for each
# draw

certificate_prices <- discounting_vector %*% coupon_payment_matrix

hist(certificate_prices, breaks = 20)

table(certificate_prices)
