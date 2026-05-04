# library("swirl")
# swirl()

# load data
df <- read.table("customers.txt", header = TRUE)
df <- df[!is.na(df$age), , drop = FALSE] # Remove empty records

# 1. What is the 5th element in the original list of ages?
fifth_element <- df$age[5]
print(fifth_element)

# 2. What is the fifth lowest age?
# sorted_ages <- sort(df$age)
sorted_unique_ages <- sort(unique(df$age))
fifth_lowest_age <- sorted_unique_ages[5]
print(sorted_unique_ages)
print(fifth_lowest_age)

# 3. Extract the five lowest ages together
five_lowest_ages <- sorted_unique_ages[1:5]
print(five_lowest_ages)

# 4. Get the five highest ages by first sorting them in decreasing order first.
sorted_unique_ages_desc <- sort(unique(df$age), decreasing = TRUE)
five_highest_ages <- sorted_unique_ages_desc[1:5]
print(five_highest_ages)

# 5. What is the average (mean) age?
mean_age <- mean(df$age)
print(mean_age)

# 6. What is the standard deviation of ages? (guess or google the standard deviation function in R)
standard_deviation_age <- sd(df$age)
print(standard_deviation_age)

# 7. Make a new variable called age_diff, with the difference between each age and the mean age
df$age_diff <- df$age - mean_age
print(head(df$age_diff))

# 8. What is the average “difference between each age and the mean age”?
mean_age_diff <- mean(df$age_diff)
print(mean_age_diff)

# 9. Visualize the raw data as we did in class: (a) histogram, (b) density plot, (c) boxplot+stripchart
png("hist.png", width = 800, height = 600)
hist(df$age, main = "(a) Histogram of ages", xlab = "Age")
dev.off()

png("density.png", width = 800, height = 600)
plot(density(df$age), main = "(b) Density plot of ages")
dev.off()

png("box_strip.png", width = 800, height = 600)
boxplot(df$age, horizontal = TRUE, main = "(c) Boxplot of ages")
stripchart(df$age, method = "stack", add = TRUE)
dev.off()
