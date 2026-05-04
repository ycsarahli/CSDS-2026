plot_dist <- function(x,
                      title = "Distribution",
                      col_hist = "lightblue",
                      col_density = "blue") {

  # histogram (scaled to density)
  hist(x,
       probability = TRUE,
       col = col_hist,
       border = "white",
       main = title)

  # density curve
  lines(density(x), col = col_density, lwd = 1.5)
}

# Normalization: rescale range to [0,1]
## Min-Max Scaling
minmax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

set.seed(42)
heights <- round(rnorm(n = 10000, mean = 164.7, sd = 7.07))

max(heights) - min(heights)

heights_n <- minmax(heights)
heights_n
plot_dist(heights_n, title="Min-Max")

# Standardization
standardize <- function(x) {
  (x - mean(x)) / sd(x)
}

heights_std <- standardize(heights)
heights_std
plot_dist(heights_std, title="Standardization")

# Probabilities and z-scores
manager_hyp <- 410

pnorm(-1.13) # 0.1292381
pnorm(0.80, lower.tail = FALSE)

# T-test
auditor_data <- read.csv("audit.txt")
auditor_sample <- auditor_data$audit

sample_size <- length(auditor_sample)
sample_mean <- mean(auditor_sample)
sample_sd <- sd(auditor_sample)

## Standard Error
se <- (sample_sd /sqrt(sample_size))

## t-statistics
t <- (sample_mean- manager_hyp) / se

## Probability of t
df <- sample_size - 1
p <- 1 - pt(t, df) # -> almost 0


# Boostrap
# minday from question 3
bookings <- read.table("first_bookings_datetime_sample.txt", header = TRUE)
hours <- as.POSIXlt(bookings$datetime, format = "%m/%d/%Y %H:%M")$hour
mins <- as.POSIXlt(bookings$datetime, format = "%m/%d/%Y %H:%M")$min
minday <- hours * 60 + mins

# create minday_std
minday_std <- scale(minday)

set.seed(456789)

n <- length(minday)
num_boot <- 2000

sample_statistics <- function(sample0, stat_function) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  stat_function(resample)
}

sample_means <- replicate(num_boot, sample_statistics(minday, mean))
plot(density(sample_means), lwd=2, main="sample means")
quantile(sample_means, probs = c(0.025, 0.975))

sample_medians <- replicate(num_boot, sample_statistics(minday, median))
plot(density(sample_medians), lwd=2, main="sample medians")
quantile(sample_medians, probs = c(0.025, 0.975))

## Bootstrapping Difference of Means
mean(auditor_sample) - manager_hyp

boot_mean_diffs <- function(sample0, mean_hyp) {
  resample <- sample(sample0, length(sample0), replace=TRUE)
  return( mean(resample) - mean_hyp )
}

num_boots <- 2000
mean_diffs <- replicate(
  num_boots,
  boot_mean_diffs(auditor_sample, manager_hyp)
)
diff_ci_95 <- quantile(mean_diffs, probs=c(0.025, 0.975))

plot(density(mean_diffs), xlim=c(0,150))
abline(v=diff_ci_95, lty="dashed")