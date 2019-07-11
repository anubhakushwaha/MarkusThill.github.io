# Number of Sailors 
k <- 3

# Number of Monkeys on the island
m <- 1

# Range of coconuts to try (1 2 3 ... n_max):
n_max <- 1e6

# There should be less monkeys than sailors
stopifnot(m < k)

# Recursive function simulating the sailors behaviour
divideCocos <- function (n, sailor) {
  if(sailor < 0) return (TRUE)
  if((n %% k) == m) return (divideCocos((n-m) / k * (k - 1), sailor - 1))
  FALSE
}

tryN <- sapply(1:n_max, function(i) divideCocos(i, k))
realN <- which(tryN)

# Compare with formula
formulaNCocos <- function(q, k, m) {
  q*k^(k+1) - m*(k-1)
}
formulaN <- sapply(1:length(realN), formulaNCocos, k, m)

# Check if the simulated results fit to the formula we obtained
anyError <- which(formulaN != realN)
stopifnot(length(anyError) == 0)

cat("Formula appears to be correct!!!\n")
cat("\nSome numbers n that work:", realN[1:min(25, length(realN))])