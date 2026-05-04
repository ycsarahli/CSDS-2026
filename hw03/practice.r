# === function ===
standardize <- function(numbers) {
  (numbers - mean(numbers)) / sd(numbers)
}

first_ten <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
standardize(first_ten)

# -----

standardize <- function(numbers) {
  ave <- mean(numbers)
  std_dev <- sd(numbers)
  n <- length(numbers)
  for (i in 1:n) numbers[i] <- (numbers[i] - ave) / std_dev
  return(numbers)
}

standardize(first_ten) # same result!

# -----

x <- c(1, 2, 3, 4)
tracemem(x) # "<0x10e0a34b8>"

x * 2
tracemem(x) # "<0x10e0a34b8>"

x[2] <- 9
tracemem(x) # "<0x113b70508>" -> copy on write

# -----

standardize <- function(numbers) {
  ave <- mean(numbers)
  std_dev <- sd(numbers)
  n <- length(numbers)
  cat(paste("BEFORE change, numbers is at: ", tracemem(numbers), "\n"))
  for (i in 1:n) numbers[i] <- (numbers[i] - ave) / std_dev
  cat(paste("AFTER change, numbers is at: ", tracemem(numbers), "\n"))
  return(numbers)
}

standardize(first_ten) # <0x113194788> -> <0x114bc3588> 

stdize <- standardize(first_ten) # <0x113194788> -> <0x112d2d658> 
tracemem(stdize) # <0x112d2d658>

# === for loop ===
norm_data <- rnorm(500000)

results <- c() # empty vector, put the result

for (index in 1:length(norm_data)) {
  if (norm_data[index] < 0) {
  results[index] <- FALSE
  } else {
  results[index] <- TRUE
  }
}

results[0:10] # TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE ...

# -----
results <- c()

for (num in norm_data) {
  if (num > 0) {
  results <- c(results, TRUE)
  } else {
  results <- c(results, FALSE)
  }
}

results[0:10] # same result

# === functional iteration ===
is_positive <- function(num) {
  if (num > 0) {
  return(TRUE)
  } else {
  return(FALSE)
  }
}

is_positive(5) # TRUE
is_positive(-5) # FALSE

# -----

# apply the function on every element of the data
results <- sapply(norm_data, is_positive)
results # TRUE FALSE  TRUE FALSE FALSE  TRUE  TRUE  TRUE FALSE  TRUE  TRUE ...

# === vectorized iteration ===
5 > 0 # TRUE
c(5, -3) > 0 # TRUE FALSE

results <- norm_data > 0
ifelse(norm_data > 0, "positive", "negative")

system.time(sapply(norm_data, is_positive))
#    user  system elapsed 
#   0.094   0.002   0.096 

system.time(norm_data > 0)
#    user  system elapsed 
#   0.000   0.001   0.001

# === sampling ===
library(compstatslib)
d3 <- rnorm(n=500000, mean=5, sd=5)
d2 <- rnorm(n=200000, mean=20, sd=5)
d1 <- rnorm(n=100000, mean=35, sd=5)
d123 <- c(d1, d2, d3)

interactive_sampling(d123)