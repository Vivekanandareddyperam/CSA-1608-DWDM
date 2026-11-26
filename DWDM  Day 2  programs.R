1. Covariance and correlation
# Preferences for photographs
A <- c(18, 2, 20)
B <- c(22, 28, 10)
C <- c(20, 40, 40)

# 1. Sample covariance between B and C
cov(B, C)

# 2. Covariance matrix
preferences <- data.frame(A, B, C)
cov(preferences)

# 3. Sample correlation between B and C
cor(B, C)

# 4. Correlation matrix
cor(preferences)
2. Equal-frequency partitioning and data smoothing

data <- c(18,18,18,20,20,20,20,20,20,20,
          21,21,21,21,25,25,25,25,25,28,28,30,30,30)

# (i) Equal-frequency partitioning
sorted_data <- sort(data)
bins <- split(sorted_data,
              rep(1:3, each = 8))

print(bins)

# (ii-a) Smoothing by bin means
bin_means <- sapply(bins, mean)
smooth_mean <- unlist(
  lapply(bins, function(b) rep(mean(b), length(b)))
)
print(smooth_mean)

# (ii-b) Smoothing by bin boundaries
smooth_boundary <- unlist(lapply(bins, function(b) {
  low <- min(b)
  high <- max(b)
  ifelse(abs(b - low) <= abs(b - high), low, high)
}))
print(smooth_boundary)

# (iii) Histogram
hist(data, breaks = seq(17.5, 32.5, by = 5),
     main = "Histogram of Data",
     xlab = "Values", col = "lightblue")
#3. Compare the examination results of two Maths classes

A <- c(76, 35, 47, 64, 95, 66, 89, 36, 84)
B <- c(51, 56, 84, 60, 59, 70, 63, 66, 50)

# Mean, median and range
mean(A)
median(A)
diff(range(A))

mean(B)
median(B)
diff(range(B))

# Boxplot
boxplot(A, B,
        names = c("Class A", "Class B"),
        main = "Comparison of Maths Marks",
        ylab = "Marks",
        col = c("lightblue", "lightgreen"))
#4. Min-max and z-score normalization

x <- c(200, 300, 400, 600, 1000)

# Min-max normalization
min_max <- (x - min(x)) / (max(x) - min(x))
print(min_max)

# Z-score normalization
z_score <- (x - mean(x)) / sd(x)
print(z_score)
#5. Histogram of the AirPassengers dataset

# Load built-in dataset
data("AirPassengers")

# Histogram with bin width 150
hist(as.numeric(AirPassengers),
     breaks = seq(100, 700, by = 150),
     xlim = c(100, 700),
     main = "Histogram of AirPassengers",
     xlab = "Number of Passengers",
     ylab = "Frequency",
     col = "lightblue",
     include.lowest = TRUE)
#6. Obtain multiple lines in one plot using mtcars

data(mtcars)

# Plot two lines in a single plotting function
matplot(mtcars[, c("mpg", "qsec")],
        type = "l",
        lty = 1,
        col = c("blue", "red"),
        lwd = 2,
        main = "MPG and QSEC in mtcars",
        xlab = "Car Index",
        ylab = "Value")

legend("topright",
       legend = c("MPG", "QSEC"),
       col = c("blue", "red"),
       lty = 1)
#7. Linear relationship between mortality and water hardness

# Load water dataset from MASS package
library(MASS)
data(water)

# Scatter plot
plot(water$hardness, water$mortality,
     main = "Mortality vs Water Hardness",
     xlab = "Water Hardness",
     ylab = "Mortality",
     pch = 19,
     col = "blue")

# Linear regression
model <- lm(mortality ~ hardness, data = water)

# Add regression line
abline(model, col = "red", lwd = 2)

# Display model summary
summary(model)

# Predict mortality at hardness = 88
new_data <- data.frame(hardness = 88)
predict(model, newdata = new_data)
#8. Boxplot of MPG against number of cylinders

data(mtcars)

boxplot(mpg ~ factor(cyl),
        data = mtcars,
        main = "MPG by Number of Cylinders",
        xlab = "Number of Cylinders",
        ylab = "Miles per Gallon (MPG)",
        col = c("lightblue", "lightgreen", "lightpink"))
#9. Detect outliers in tennis players' scores using a boxplot

# Points scored by tennis players
points <- c(12, 15, 18, 20, 22, 24, 25, 27, 28, 30, 85)

# Draw boxplot
boxplot(points,
        main = "Tennis Players' Points",
        ylab = "Points Scored",
        col = "lightblue")

# Identify potential outliers
boxplot.stats(points)$out
#10. Analyze blood pressure by age using diabetes.csv

# Step 1: Load the dataset
diabetes <- read.csv(file.choose())

# Step 2: Display the column names and first six rows
print(names(diabetes))
print(head(diabetes))

# Step 3: Remove missing values
data_clean <- diabetes[
  complete.cases(diabetes[, c("Age", "BloodPressure")]), ]

# Step 4: Draw scatter plot
plot(data_clean$Age, data_clean$BloodPressure,
     main = "Blood Pressure vs Age",
     xlab = "Age",
     ylab = "Blood Pressure",
     pch = 19,
     col = "blue")

# Step 5: Create age groups
data_clean$AgeGroup <- cut(
  data_clean$Age,
  breaks = c(0, 20, 30, 40, 50, 60, Inf),
  labels = c("0-20", "21-30", "31-40",
             "41-50", "51-60", "Above 60"),
  include.lowest = TRUE
)

# Step 6: Calculate average blood pressure by age group
avg_bp <- aggregate(
  BloodPressure ~ AgeGroup,
  data = data_clean,
  FUN = mean
)

# Display the average blood pressure
print(avg_bp)

# Step 7: Draw bar chart
barplot(avg_bp$BloodPressure,
        names.arg = avg_bp$AgeGroup,
        main = "Average Blood Pressure by Age Group",
        xlab = "Age Group",
        ylab = "Average Blood Pressure",
        col = "lightgreen",
        border = "black")

# Step 8: Display the age group with the highest average
highest_group <- avg_bp$AgeGroup[
  which.max(avg_bp$BloodPressure)
]

print("Age group with highest average blood pressure:")
print(highest_group)
