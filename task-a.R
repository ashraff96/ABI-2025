# Load the dataset
data <- read.csv("UQR.csv")

# Check data structure
str(data)

# Check the missing values
colSums(is.na(data))

# dataset summary
summary(data)

# Save the refined dataset.(Needed)
write.csv(data, "UQR_cleaned.csv", row.names = FALSE)

# Tests (Normality)

# Install the required - packages
install.packages(c("nortest", "ggplot2"))

# Load the required - libraries

# Install & Analysis using R commander
install.packages("Rcmdr")

library(Rcmdr)
library(nortest)   
library(ggplot2)
library(car)

install.packages("nortest")
library(nortest)

# First Variable- Student.Enrollment

# Anderson-Darling - Test
ad.test(data$Student.Enrollment)

# Lilliefors Test (Kolmogorov-Smirnov)
lillie.test(data$Student.Enrollment)

# Shapiro-Wilk Test
shapiro.test(data$Student.Enrollment)

# Generate a histogram with a normal curve.
hist(data$Student.Enrollment, main = "Student Enrollment Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Student Enrollment")
curve(dnorm(x, mean = mean(data$Student.Enrollment, na.rm = TRUE), 
            sd = sd(data$Student.Enrollment, na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot - (Q Plot)
qqPlot(data$Student.Enrollment,
       ylab = "Enrollment",
       main = "Quantile Comparison - Student Enrollment")

# second Variable- Institution.Type

# Convert Institution.Type to numeric (Public = 0, Private = 1)
data$Institution.Type.Numeric <- ifelse(data$Institution.Type == "Public", 0, 1)

# Anderson-Darling Test
ad.test(data$Institution.Type.Numeric)

# Lilliefors Test - (Kolmogorov-Smirnov)
lillie.test(data$Institution.Type.Numeric)

# Shapiro-Wilk Test
shapiro.test(data$Institution.Type.Numeric)

# Generate a histogram with a normal curve.
hist(data$Institution.Type.Numeric, main = "Institution Type Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Institution Type (Numeric)")
curve(dnorm(x, mean = mean(data$Institution.Type.Numeric, na.rm = TRUE), 
            sd = sd(data$Institution.Type.Numeric, na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Institution.Type.Numeric,
       ylab = "Institution Type",
       main = "Quantile Comparison - Institution Type (Numeric)")

# Thurd Variable- Faculty.Salary..Avg..

# Anderson-Darling Test
ad.test(data$Faculty.Salary..Avg..)

# Lilliefors Test (Kolmogorov-Smirnov)
lillie.test(data$Faculty.Salary..Avg..)

# Shapiro-Wilk Test
shapiro.test(data$Faculty.Salary..Avg..)

# Generate a histogram with a normal curve.
hist(data$Faculty.Salary..Avg.., main = "Faculty Salary (Avg.) Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Faculty Salary (Avg.)")
curve(dnorm(x, mean = mean(data$Faculty.Salary..Avg.., na.rm = TRUE), 
            sd = sd(data$Faculty.Salary..Avg.., na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Faculty.Salary..Avg..,
       ylab = "Faculty Salary",
       main = "Quantile Comparison - Faculty Salary (Avg.)")

# Forth Variable: Research.Funding..Million.USD.

# Anderson-Darling Test
ad.test(data$Research.Funding..Million.USD.)

# Lilliefors Test - (Kolmogorov-Smirnov) 
lillie.test(data$Research.Funding..Million.USD.)

# Shapiro-Wilk Test
shapiro.test(data$Research.Funding..Million.USD.)

# Generate a histogram with a normal curve.
hist(data$Research.Funding..Million.USD., main = "Research Funding Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Research Funding (Million USD)")
curve(dnorm(x, mean = mean(data$Research.Funding..Million.USD., na.rm = TRUE), 
            sd = sd(data$Research.Funding..Million.USD., na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Research.Funding..Million.USD.,
       ylab = "Research Funding",
       main = "Quantile Comparison - Research Funding (Million USD)")


# Fifth Variable: Graduation.Rate.

# Anderson-Darling Test
ad.test(data$Graduation.Rate....)

# Lilliefors Test - (Kolmogorov-Smirnov) 
lillie.test(data$Graduation.Rate....)

# Shapiro-Wilk Test
shapiro.test(data$Graduation.Rate....)

# Generate a histogram with a normal curve.
hist(data$Graduation.Rate...., main = "Graduation Rate Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Graduation Rate (%)")
curve(dnorm(x, mean = mean(data$Graduation.Rate...., na.rm = TRUE), 
            sd = sd(data$Graduation.Rate...., na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Graduation.Rate....,
       ylab = "Graduation Rate",
       main = "Quantile Comparison - Graduation Rate (%)")

# Sixth Variable- Student.Faculty.Ratio

# Anderson-Darling Test
ad.test(data$Student.Faculty.Ratio)

# Lilliefors Test - (Kolmogorov-Smirnov) 
lillie.test(data$Student.Faculty.Ratio)

# Shapiro-Wilk Test
shapiro.test(data$Student.Faculty.Ratio)

# Generate a histogram with a normal curve.
hist(data$Student.Faculty.Ratio, main = "Student-Faculty Ratio Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Student-Faculty Ratio")
curve(dnorm(x, mean = mean(data$Student.Faculty.Ratio, na.rm = TRUE), 
            sd = sd(data$Student.Faculty.Ratio, na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Student.Faculty.Ratio,
       ylab = "Student-Faculty Ratio",
       main = "Quantile Comparison - Student-Faculty Ratio")

# Seventh Variable: Tuition.Fees..USD.

# Anderson-Darling Test
ad.test(data$Tuition.Fees..USD.)

# Lilliefors Test -(Kolmogorov-Smirnov) 
lillie.test(data$Tuition.Fees..USD.)

# Shapiro-Wilk Test
shapiro.test(data$Tuition.Fees..USD.)

# Generate a histogram with a normal curve.
hist(data$Tuition.Fees..USD., main = "Tuition Fees Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Tuition Fees (USD)")
curve(dnorm(x, mean = mean(data$Tuition.Fees..USD., na.rm = TRUE), 
            sd = sd(data$Tuition.Fees..USD., na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Tuition.Fees..USD.,
       ylab = "Tuition Fees",
       main = "Quantile Comparison - Tuition Fees (USD)")

# 8th Variable: Employment.Rate....

# Anderson-Darling Test
ad.test(data$Employment.Rate....)

# Lilliefors Test-(Kolmogorov-Smirnov) 
lillie.test(data$Employment.Rate....)

# Shapiro-Wilk Test
shapiro.test(data$Employment.Rate....)

# Generate a histogram with a normal curve.
hist(data$Employment.Rate...., main = "Employment Rate Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "Employment Rate (%)")
curve(dnorm(x, mean = mean(data$Employment.Rate...., na.rm = TRUE), 
            sd = sd(data$Employment.Rate...., na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$Employment.Rate....,
       ylab = "Employment Rate",
       main = "Quantile Comparison - Employment Rate (%)")

# 9th Variable: University.Ranking.Score

# Anderson-Darling Test
ad.test(data$University.Ranking.Score)

# Lilliefors Test-(Kolmogorov-Smirnov) 
lillie.test(data$University.Ranking.Score)

# Shapiro-Wilk Test
shapiro.test(data$University.Ranking.Score)

# Generate a histogram with a normal curve.
hist(data$University.Ranking.Score, main = "University Ranking Score Distribution", 
     prob = TRUE, col = c("gray80", "gray90", "gray100"), xlab = "University Ranking Score")
curve(dnorm(x, mean = mean(data$University.Ranking.Score, na.rm = TRUE), 
            sd = sd(data$University.Ranking.Score, na.rm = TRUE)), add = TRUE, lwd = 2)

# Quantile Comparison Plot
qqPlot(data$University.Ranking.Score,
       ylab = "Ranking Score",
       main = "Quantile Comparison - University Ranking Score")

# Analyzing correlation

# Correlation between Student.Enrollment and University.Ranking.Score
cor.test(data$Student.Enrollment, data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Student.Enrollment,
     main = "Student Enrollment vs University Ranking Score",
     xlab = "Student Enrollment",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Student.Enrollment), col = "blue")

# Correlation between Institution Type (numeric) and University Ranking Score
cor.test(data$Institution.Type.Numeric, data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Institution.Type.Numeric,
     main = "Institution Type vs University Ranking Score",
     xlab = "Institution Type (Public = 0, Private = 1)",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Institution.Type.Numeric), col = "blue")

# Correlation between Faculty Salary (Avg.) and University Ranking Score
cor.test(data$Faculty.Salary..Avg.., data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Faculty.Salary..Avg..,
     main = "Faculty Salary vs University Ranking Score",
     xlab = "Faculty Salary (Avg.)",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Faculty.Salary..Avg..), col = "blue")

# Correlation between Research Funding and University Ranking Score
cor.test(data$Research.Funding..Million.USD., data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Research.Funding..Million.USD.,
     main = "Research Funding vs University Ranking Score",
     xlab = "Research Funding (Million USD)",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Research.Funding..Million.USD.), col = "blue")


# Correlation between Graduation Rate and University Ranking Score
cor.test(data$Graduation.Rate...., data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Graduation.Rate....,
     main = "Graduation Rate vs University Ranking Score",
     xlab = "Graduation Rate (%)",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Graduation.Rate....), col = "blue")

# Correlation between Student-Faculty Ratio and University Ranking Score
cor.test(data$Student.Faculty.Ratio, data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Student.Faculty.Ratio,
     main = "Student-Faculty Ratio vs University Ranking Score",
     xlab = "Student-Faculty Ratio",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Student.Faculty.Ratio), col = "blue")

# Correlation between Tuition Fees and University Ranking Score
cor.test(data$Tuition.Fees..USD., data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Tuition.Fees..USD.,
     main = "Tuition Fees vs University Ranking Score",
     xlab = "Tuition Fees (USD)",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Tuition.Fees..USD.), col = "blue")

# Correlation between Employment Rate and University Ranking Score
cor.test(data$Employment.Rate...., data$University.Ranking.Score, 
         method = "spearman", alternative = "two.sided")

# Graphical Analysis
plot(data$University.Ranking.Score ~ data$Employment.Rate....,
     main = "Employment Rate vs University Ranking Score",
     xlab = "Employment Rate (%)",
     ylab = "University Ranking Score")
abline(lm(data$University.Ranking.Score ~ data$Employment.Rate....), col = "blue")

# Predictive modeling

# Simple Linear Regression Model: Student Enrollment vs University Ranking Score
student_enroll_model <- lm(University.Ranking.Score ~ Student.Enrollment, data = data)

# Summary of the model
summary(student_enroll_model)

# Simple Linear Regression Model: Faculty Salary vs University Ranking Score
faculty_salary_model <- lm(University.Ranking.Score ~ Faculty.Salary..Avg.., data = data)

# Summary of the model
summary(faculty_salary_model)

# Multiple Linear Regression - All Variables -> University Ranking Score

# Build the model with all predictors
multiple_model <- lm(University.Ranking.Score ~ 
                       Student.Enrollment + 
                       Institution.Type.Numeric + 
                       Faculty.Salary..Avg.. + 
                       Research.Funding..Million.USD. + 
                       Graduation.Rate.... +
                       Student.Faculty.Ratio +
                       Tuition.Fees..USD. +
                       Employment.Rate....,
                     data = data, model = TRUE)

# View model summary
summary(multiple_model)

# Install the relevant package
install.packages("car")

# Load library
library(car)

# Scatterplot Matrix for University Dataset Variables
scatterplotMatrix(
  ~ University.Ranking.Score +
    Student.Enrollment +
    Institution.Type.Numeric +
    Faculty.Salary..Avg.. +
    Research.Funding..Million.USD. +
    Graduation.Rate.... +
    Student.Faculty.Ratio +
    Tuition.Fees..USD. +
    Employment.Rate....,
  regLine = FALSE,
  smooth = FALSE,
  diagonal = list(method = "density"),
  data = data,
  main = "Scatterplot Matrix of University Quality Variables",
  col = c("gray20", "gray30", "darkgray")
)
