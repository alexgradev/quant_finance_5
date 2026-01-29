# Home Assignment 1
# Quantitative Finance 

# frictionless market
# index traded at 4600 eur
# risk-free MM account with r = 1.9% p.a. (continuously compounded)
# options with expiry of two years
# Cox-Ross-Rubinstein binomial tree
# 40 time steps
S0 = 4600
sigma <- 0.1
T <- 2
N <- 40
r <- 0.019

# Calculate the underlying's price
k <- exp(sigma*sqrt(T/N))

# Build it as a list of vectors
for (i in 1:3){
  for (j in 1:(i+1)){
    print(i-(j-1)*2)
  }
  cat("\n")
}

# Calculate prices
underlying <- list(S0)
powers <- list(0)

for (i in 1:40){
  powers_t <- c()
  prices_t <- c()
  for (j in 1:(i+1)){
    power_state <- (i-(j-1)*2)
    price_t_state <- S0*k^power_state
    prices_t <- c(prices_t, price_t_state) 
    powers_t <- c(powers_t, power_state)
  }
  underlying <- append(underlying, list(prices_t))
  powers <- append(powers, list(powers_t))
}
underlying[1:6]
powers

# Task 1
# European call with strike price 4800
K = 4800
q <- (exp(r*T/N) - 1/k)/(k-1/k)

# Option value at expiry
call_exercise_value <- sapply(underlying[[length(underlying)]], 
                              function(X) max(X-K,0))
call_value <- list(call_exercise_value)

# Calculate options intrinsic value across time steps
for (i in 1:N){
  values <- call_value[[i]]
  current_values <- c()
  for (j in 1:(length(values)-1)){
    #cat("S_u",values[i]," S_d", values[])
    current_value <- (q*values[j] + (1-q)*values[j+1])*exp(-r*T/N)
    current_value <- round(current_value,2)
    current_values <- c(current_values, current_value)
  }
  call_value <- append(call_value, list(current_values))
}


tail(call_value)

# Task 2
# American put with strike price 4000
K = 4000
q <- (exp(r*T/N) - 1/k)/(k-1/k)

# Option value at expiry
put_exercise_value <- sapply(underlying[[length(underlying)]], 
                              function(X) max(K-X,0))
put_value <- list(put_exercise_value)

for (i in 1:N){
  underlying_values <- underlying[[length(underlying) - i]]
  values <- put_value[[i]]
  current_values <- c()
  
  for (j in 1:(length(values)-1)){
    # Calculate value at t=i
    current_value <- (q*values[j] + (1-q)*values[j+1])*exp(-r*T/N)
    # Check early exercise of american option
    current_value_checked <- max(K - underlying_values[j], current_value)
    current_value_checked <- round(current_value_checked, 2)
    # Append the value at state j to the vector for values at time i
    current_values <- c(current_values, current_value_checked)
  }
  
  # Store the values at time i
  put_value <- append(put_value, list(current_values))
}


tail(put_value)

# Bermudian option
K = 4800
q <- (exp(r*T/N) - 1/k)/(k-1/k)

# Option value at expiry
bermudian_put_exercise_value <- sapply(underlying[[length(underlying)]], 
                             function(X) max(K-X,0))
bermudian_put_value <- list(bermudian_put_exercise_value)

# i=1 -> t=39
# i=2 -> t=38
# i=5 -> t=35
# i=10 -> t=30
# i=20 -> t=20
# i=30 -> t=10
# i=40 -> t=0
for (i in 1:N){
  underlying_values <- underlying[[length(underlying) - i]]
  values <- bermudian_put_value[[i]]
  current_values <- c()
  cat("time step =", N-i)
  cat("\nIndex =", i)
  if(i%%10==0){print("check")}
  for (j in 1:(length(values)-1)){
    # Calculate value at t=i
    current_value <- (q*values[j] + (1-q)*values[j+1])*exp(-r*T/N)
    # Check early exercise of the bermudian option
    if(i%%10 == 0){
      #print("check")
      current_value_checked <- max(K - underlying_values[j], current_value)
      current_value_checked <- round(current_value_checked, 2)
      # Append the value at state j to the vector for values at time i
      current_values <- c(current_values, current_value_checked)
    }
    else{
      current_value <- round(current_value, 2)
      current_values <- c(current_values, current_value)
    }
    
  }
  print(current_values)
  cat("\n")
  # Store the values at time i
  bermudian_put_value <- append(bermudian_put_value, list(current_values))
  
  Sys.sleep(2)
}


tail(bermudian_put_value)
