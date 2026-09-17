library(dplyr)

data <- read.csv("data/dataset.csv", sep = ";")

print(dim(data))
str(data)

print(colSums(is.na(data)))

print(table(data$Target))

print(summary(data$Age.at.enrollment))
print(summary(data$Curricular.units.1st.sem..grade.))
print(summary(data$Tuition.fees.up.to.date))

png("hist_grade_overall.png", width = 800, height = 600)
hist(data$Curricular.units.1st.sem..grade., main = "1st Semester Grade Distribution",
     xlab = "Grade", col = "lightblue")
dev.off()

png("barchart_target_counts.png", width = 800, height = 600)
barplot(table(data$Target), main = "Number of Students by Target",
        xlab = "Target", ylab = "Count", col = "lightgreen")
dev.off()

data_clean <- subset(data, data$Curricular.units.1st.sem..enrolled. > 0)

data2 <- subset(data_clean, data_clean$Target == "Graduate" | data_clean$Target == "Dropout")
data2$Target <- factor(data2$Target, levels = c("Graduate", "Dropout"))

grad_grades <- data2$Curricular.units.1st.sem..grade.[data2$Target == "Graduate"]
drop_grades <- data2$Curricular.units.1st.sem..grade.[data2$Target == "Dropout"]

print(mean(grad_grades))
print(sd(grad_grades))
print(median(grad_grades))
print(length(grad_grades))

print(mean(drop_grades))
print(sd(drop_grades))
print(median(drop_grades))
print(length(drop_grades))

my_table <- table(data$Tuition.fees.up.to.date, data$Target)
print(my_table)

print(shapiro.test(grad_grades))
print(shapiro.test(drop_grades))

print(var.test(Curricular.units.1st.sem..grade. ~ Target, data = data2))

chi_result <- chisq.test(my_table)
print(chi_result)
print(chi_result$expected)

print(t.test(Curricular.units.1st.sem..grade. ~ Target, data = data2, var.equal = FALSE))

png("boxplot_grade_by_target.png", width = 800, height = 600)
boxplot(Curricular.units.1st.sem..grade. ~ Target, data = data_clean,
        main = "1st Semester Grade by Target",
        xlab = "Target", ylab = "Grade",
        col = c("lightblue", "salmon", "lightgreen"))
dev.off()

png("barchart_tuition_target.png", width = 800, height = 600)
barplot(my_table, beside = TRUE, legend.text = c("Not up to date", "Up to date"),
        main = "Tuition Fees vs Target", xlab = "Target", ylab = "Count",
        col = c("indianred", "seagreen"))
dev.off()
