#!/usr/bin/env Rscript
# =====================================================================
# SAVEETHA SCHOOL OF ENGINEERING
# DWDM Day 1 Lab Exercises - R Programs (Q1 - Q11)
# Output of every statement is shown in comments starting with "#>"
# Run:  Rscript dwdm_day1_lab_with_output.R
# =====================================================================

dir.create("plots", showWarnings = FALSE)


# =====================================================================
# Q1. Approximate median of grouped (interval) data
# =====================================================================
lower <- c(1, 5, 15, 20, 50, 80)
upper <- c(5, 15, 20, 50, 80, 110)
freq  <- c(200, 450, 300, 1500, 700, 44)

N        <- sum(freq)
N
#> [1] 3194

cum_freq <- cumsum(freq)
cum_freq
#> [1]  200  650  950 2450 3150 3194

k       <- which(cum_freq >= N / 2)[1]       # median class index
cf_prev <- if (k > 1) cum_freq[k - 1] else 0
width   <- upper[k] - lower[k]

cat("Median class:", lower[k], "-", upper[k], "\n")
#> Median class: 20 - 50

median_approx <- lower[k] + ((N / 2 - cf_prev) / freq[k]) * width
median_approx
#> [1] 32.94


# =====================================================================
# Q2. Mean, median, mode, midrange, Q1 and Q3
# =====================================================================
age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,
         35,36,40,45,46,52,70)

# (a) Mean and median
mean(age)
#> [1] 29.96296
median(age)
#> [1] 25

# (b) Mode and modality
mode_age <- names(table(age))[table(age) == max(table(age))]
mode_age
#> [1] "25" "35"
# Two modes (25 and 35)  ->  the data is BIMODAL

# (c) Midrange = (min + max) / 2
range(age)
#> [1] 13 70
(min(age) + max(age)) / 2
#> [1] 41.5

# (d) Quartiles
quantile(age, .25)
#>  25%
#> 20.5
quantile(age, .75)
#> 75%
#>  35


# =====================================================================
# Q3. Normalization: (a) min-max (0 to 1)  (b) z-score
# =====================================================================
x <- c(200, 300, 400, 600, 1000)

# (a) Min-max normalization
min_max_norm <- (x - min(x)) / (max(x) - min(x))
min_max_norm
#> [1] 0.000 0.125 0.250 0.500 1.000

# (b) Z-score normalization (sample SD)
z_score_norm <- (x - mean(x)) / sd(x)
z_score_norm
#> [1] -0.9486833 -0.6324555 -0.3162278  0.3162278  1.5811388


# =====================================================================
# Q4. Smoothing by binning (5 equal-width bins)
# =====================================================================
data <- c(11,13,13,15,15,16,19,20,20,20,21,21,22,23,24,30,40,45,45,45,
          71,72,73,75)
bins        <- 5
bin_indices <- cut(data, bins)

table(bin_indices)
#> bin_indices
#> (10.9,23.8] (23.8,36.6] (36.6,49.4] (49.4,62.2] (62.2,75.1]
#>          14           2           4           0           4

# (a) Smoothing by bin mean
mean_smooth <- tapply(data, bin_indices, mean)
print(mean_smooth)
#> (10.9,23.8] (23.8,36.6] (36.6,49.4] (49.4,62.2] (62.2,75.1]
#>    17.78571    27.00000    43.75000          NA    72.75000

# (b) Smoothing by bin median
median_smooth <- tapply(data, bin_indices, median)
median_smooth
#> (10.9,23.8] (23.8,36.6] (36.6,49.4] (49.4,62.2] (62.2,75.1]
#>        19.5        27.0        45.0          NA        72.5

# (c) Smoothing by bin boundaries (min and max of each bin)
min_max_smooth <- tapply(data, bin_indices,
                         function(v) if (length(v)) c(min(v), max(v)) else NULL)
print(min_max_smooth)
#> $`(10.9,23.8]`
#> [1] 11 23
#>
#> $`(23.8,36.6]`
#> [1] 24 30
#>
#> $`(36.6,49.4]`
#> [1] 40 45
#>
#> $`(49.4,62.2]`
#> NULL
#>
#> $`(62.2,75.1]`
#> [1] 71 75
# Note: bin (49.4,62.2] is empty because there is no value between 45 and 71.


# =====================================================================
# Q5. Age and %fat: statistics, boxplot, scatter plot, Q-Q plot
# =====================================================================
age5 <- c(23,23,27,27,39,41,47,49,50,52,54,54,56,57,58,58,60,61)
fat  <- c(9.5,26.5,7.8,17.8,31.4,25.9,27.4,27.2,31.2,34.6,42.5,28.8,
          33.4,30.2,34.1,32.9,41.2,35.7)

# (a) Mean, median, standard deviation
mean(age5)
#> [1] 46.44444
median(age5)
#> [1] 51
sd(age5)
#> [1] 13.21862
mean(fat)
#> [1] 28.78333
median(fat)
#> [1] 30.7
sd(fat)
#> [1] 9.254395

# (b) Boxplot
png("plots/q5_boxplot.png", 700, 500)
boxplot(age5, fat, names = c("age", "%fat"),
        main = "Q5 Boxplot of age and %fat", col = "lightgrey")
dev.off()
#> (saved to plots/q5_boxplot.png)

# (c) Scatter plot
png("plots/q5_scatter.png", 700, 500)
scatter.smooth(age5, fat, xlab = "age", ylab = "%fat",
               main = "Q5 Scatter plot: age vs %fat")
dev.off()
#> (saved to plots/q5_scatter.png)

# (c) Q-Q plot
png("plots/q5_qqplot.png", 700, 500)
qqplot(age5, fat, xlab = "age", ylab = "%fat",
       main = "Q5 Q-Q plot: age vs %fat")
dev.off()
#> (saved to plots/q5_qqplot.png)


# =====================================================================
# Q6. Normalize the value 35 for age
# =====================================================================
v   <- age5
val <- 35

# (i) Min-max normalization onto [0.0, 1.0]
min_max <- (val - min(v)) / (max(v) - min(v))
print(min_max)
#> [1] 0.3157895

# (ii) Z-score normalization (given SD = 12.94)
m <- mean(v)
s <- 12.94
z_score <- (val - m) / s
print(z_score)
#> [1] -0.8844238

# (iii) Decimal scaling: smallest j such that max(|v|) / 10^j < 1
j <- ceiling(log10(max(abs(v))))     # max age = 61  ->  j = 2
j
#> [1] 2
decimal_scaling <- val / 10^j
print(decimal_scaling)
#> [1] 0.35


# =====================================================================
# Q7. Pencils: mean, median and mode
# =====================================================================
pencils <- c(9,25,23,12,11,6,7,8,9,10)

mean(pencils)
#> [1] 12
median(pencils)
#> [1] 9.5
mode <- names(table(pencils))[table(pencils) == max(table(pencils))]
mode
#> [1] "9"


# =====================================================================
# Q8. Scatter plot: mobile phones sold (x) vs money (y)
# =====================================================================
x8 <- c(4,1,5,7,10,2,50,25,90,36)
y8 <- c(12,5,13,19,31,7,153,72,275,110)

png("plots/q8_scatter.png", 700, 500)
scatter.smooth(x8, y8, xlab = "Mobile phones sold (x)", ylab = "Money (y)",
               main = "Q8 Scatter plot")
dev.off()
#> (saved to plots/q8_scatter.png)

cor(x8, y8)
#> [1] 0.999772
# Points lie almost on a straight line -> very strong positive linear relation.


# =====================================================================
# Q9. Marks: equal-frequency and equal-width partitioning (3 bins)
# =====================================================================
marks    <- sort(c(55,60,71,63,55,65,50,55,58,59,61,63,65,67,71,72,75))
num_bins <- 3
n        <- length(marks)

# (a) Equal-frequency (equi-depth) partitioning
bin_ef <- cut(seq_len(n), breaks = num_bins, labels = FALSE)
for (b in 1:num_bins) cat("Bin", b, ":", marks[bin_ef == b], "\n")
#> Bin 1 : 50 55 55 55 58 59
#> Bin 2 : 60 61 63 63 65
#> Bin 3 : 65 67 71 71 72 75

# (b) Equal-width partitioning
bin_ew <- cut(marks, breaks = num_bins)
table(bin_ew)
#> bin_ew
#>   (50,58.3] (58.3,66.7]   (66.7,75]
#>           5           7           5

# Smoothing the equal-frequency bins
smooth_by_mean   <- ave(marks, bin_ef, FUN = mean)
smooth_by_median <- ave(marks, bin_ef, FUN = median)
smooth_by_bound  <- ave(marks, bin_ef, FUN = function(v)
                        ifelse(abs(v - min(v)) <= abs(v - max(v)), min(v), max(v)))

marks
#>  [1] 50 55 55 55 58 59 60 61 63 63 65 65 67 71 71 72 75
round(smooth_by_mean, 2)
#>  [1] 55.33 55.33 55.33 55.33 55.33 55.33 62.40 62.40 62.40 62.40 62.40 70.17 70.17 70.17 70.17 70.17 70.17
smooth_by_median
#>  [1] 55 55 55 55 55 55 63 63 63 63 63 71 71 71 71 71 71
smooth_by_bound
#>  [1] 50 59 59 59 59 59 60 60 65 65 65 65 65 75 75 75 75

# Histograms of both partitioning methods
png("plots/q9_histograms.png", 1000, 450)
par(mfrow = c(1, 2))
hist(marks, breaks = seq(min(marks), max(marks), length.out = num_bins + 1),
     col = "lightblue", xlab = "Marks", main = "Equal-width partitioning")
ef_edges <- c(min(marks), tapply(marks, bin_ef, max))
hist(marks, breaks = unique(ef_edges), col = "lightgreen", xlab = "Marks",
     main = "Equal-frequency partitioning")
dev.off()
#> (saved to plots/q9_histograms.png)


# =====================================================================
# Q10. Inter-quartile range and standard deviation of car speed
# =====================================================================
spd <- c(78.3,81.8,82,74.2,83.4,84.5,82.9,77.5,80.9,70.6)

IQR(spd)
#> [1] 4.975
sd(spd)
#> [1] 4.445835


# =====================================================================
# Q11. First and third quartile of age
# =====================================================================
age <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,
         35,36,40,45,46,52,70)

quantile(age, .25)
#>  25%
#> 20.5
quantile(age, .75)
#> 75%
#>  35

# ======================= END OF PROGRAM ==============================
