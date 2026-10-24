#Q1. Compute the approximate median for grouped data
age <- c(5, 15, 20, 50, 80, 110)
frequency <- c(200, 450, 300, 1500, 700, 44)

# Total frequency
N <- sum(frequency)

# Find median class
cumulative_frequency <- cumsum(frequency)
median_position <- N / 2
median_class <- which(cumulative_frequency >= median_position)[1]

# Calculate approximate median
L <- c(1, 5, 15, 20, 50, 80)[median_class]
cf <- ifelse(median_class == 1, 0,
             cumulative_frequency[median_class - 1])
f <- frequency[median_class]
h <- age[median_class] - L

median_value <- L + ((N / 2 - cf) / f) * h

print(N)
print(median_value)

#Q2. Calculate mean, median, mode, midrange and quartiles
# Given sorted age data
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22, 22,
         25, 25, 25, 25, 30, 33, 33, 35, 35, 35,
         35, 36, 40, 45, 46, 52, 70)

# (a) Mean and Median
mean(age)
median(age)

# (b) Mode
mode_age <- names(table(age))[table(age) == max(table(age))]
mode_age

# (c) Midrange
(range(age)[1] + range(age)[2]) / 2

# (d) First and third quartiles
quantile(age, 0.25)
quantile(age, 0.75)

#Q3. Normalization (Reduction and Transformation)
# Data
x <- c(200, 300, 400, 600, 1000)

# (a) Min-max normalization (range 0 to 1)
min_max_norm <- (x - min(x)) / (max(x) - min(x))
print(min_max_norm)

# (b) Z-score normalization
z_score_norm <- (x - mean(x)) / sd(x)
print

#Q4. Smoothing by Binning (Reduction and Transformation)
data <- c(11,13,13,15,15,16,19,20,20,20,21,21,
          22,23,24,30,40,45,45,45,71,72,73,75)

# Divide data into 5 equal-width bins
bins <- cut(data, breaks = 5)

# (a) Smoothing by bin mean
mean_smooth <- tapply(data, bins, mean)
print(mean_smooth)

# (b) Smoothing by bin median
median_smooth <- tapply(data, bins, median)
print(median_smooth)

# (c) Bin boundaries
min_max_smooth <- tapply(
  data, bins, function(x) c(min(x), max(x))
)
print(min_max_smooth)
#Q5. Statistical Analysis of Age and Body Fat Using R
age <- c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61)

fat <- c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,
         34.6,42.5,28.8,33.4,30.2,34.1,32.9,41.2,35.7)

# (a) Mean, median and standard deviation
mean(age)
median(age)
sd(age)

mean(fat)
median(fat)
sd(fat)

# (b) Boxplots
par(mfrow = c(1, 2))
boxplot(age, main = "Boxplot of Age", ylab = "Age")
boxplot(fat, main = "Boxplot of Body Fat", ylab = "Body Fat (%)")

# (c) Scatter plot
plot(age, fat,
     main = "Age vs Body Fat",
     xlab = "Age",
     ylab = "Body Fat (%)",
     pch = 19)
scatter.smooth(age, fat, add = TRUE)

# Q-Q plot comparing age and body fat
qqplot(age, fat,
       main = "Q-Q Plot: Age vs Body Fat",
       xlab = "Age",
       ylab = "Body Fat (%)")
abline(0, 1, col = "green")
#Q6. Normalization Using R

# Age data
v <- c(23,23,27,27,39,41,47,49,50,
       52,54,54,56,57,58,58,60,61)

# (i) Min-max normalization to [0,1]
min_max <- (35 - min(v)) / (max(v) - min(v))
print(min_max)

# (ii) Z-score normalization
m <- mean(v)
s <- 12.94
z_score <- (35 - m) / s
print(z_score)

# (iii) Decimal scaling
j <- ceiling(log10(max(abs(v)) + 1))
decimal_scaling <- 35 / (10^j)
print(decimal_scaling)
#Q7. Find Mean, Median, and Mode of Pencils Using R

# Number of pencils in each box
pencils <- c(9, 25, 23, 12, 11, 6, 7, 8, 9, 10)

# Calculate mean
mean(pencils)

# Calculate median
median(pencils)

# Calculate mode
mode_value <- names(table(pencils))[
  table(pencils) == max(table(pencils))
]
mode_value
#Q8. Scatter Plot of Mobile Phones Sold and Money

# Number of mobile phones sold
x <- c(4, 1, 5, 7, 10, 2, 50, 25, 90, 36)

# Money earned
y <- c(12, 5, 13, 19, 31, 7, 153, 72, 275, 110)

# Create scatter plot with a smooth curve
scatter.smooth(x, y,
               main = "Mobile Phones Sold vs Money",
               xlab = "Number of Mobile Phones Sold",
               ylab = "Money Earned",
               pch = 19)
#Q9. Partitioning Marks into Three Bins Using R

marks <- c(55, 60, 71, 63, 55, 65, 50, 55, 58,
           59, 61, 63, 65, 67, 71, 72, 75)

# Sort marks
sorted_marks <- sort(marks)

# (a) Equal-frequency partitioning
n <- length(sorted_marks)
bins_eq_frequency <- split(
  sorted_marks,
  rep(1:3, each = ceiling(n / 3), length.out = n)
)
print(bins_eq_frequency)

# Histogram for equal-frequency partitioning
hist(marks, breaks = quantile(marks, probs = c(0, 1/3, 2/3, 1)),
     include.lowest = TRUE,
     main = "Equal-Frequency Partitioning",
     xlab = "Marks", col = "lightblue")

# (b) Equal-width partitioning
bins_eq_width <- cut(marks, breaks = 3, include.lowest = TRUE)
print(split(marks, bins_eq_width))

# Histogram for equal-width partitioning
hist(marks, breaks = 3, include.lowest = TRUE,
     main = "Equal-Width Partitioning",
     xlab = "Marks", col = "lightgreen")
#Q10. Calculate Interquartile Range and Standard Deviation

# Car speed data
v <- c(78.3, 81.8, 82, 74.2, 83.4,
       84.5, 82.9, 77.5, 80.9, 70.6)

# Interquartile range
IQR(v)

# Standard deviation
sd(v)
#Q11. Find the First and Third Quartiles Using R

# Sorted age data
age <- c(13, 15, 16, 16, 19, 20, 20, 21, 22,
         22, 25, 25, 25, 25, 30, 33, 33, 35,
         35, 35, 35, 36, 40, 45, 46, 52, 70)

# First quartile
quantile(age, 0.25)

# Third quartile
quantile(age, 0.75)


