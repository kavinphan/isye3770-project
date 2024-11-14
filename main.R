library(ggpubr)
library(cowplot)

data <- read.csv("./data.csv")

print("Applications:")
mean(data$applications)
median(data$applications)
sd(data$applications)
shapiro.test(data$applications)
t.test(data$applications)$conf.int
hist(data$applications, 
     breaks=10,
     main="Histogram of Applications",
     xlab="Applications")
boxplot(data$applications,
        main="Boxplot of Applications",
        horizontal=TRUE)


print("Interviews:")
mean(data$interviews)
median(data$interviews)
sd(data$interviews)
shapiro.test(data$interviews)
t.test(data$interviews)$conf.int
hist(data$interviews,
     breaks=10,
     main="Histogram of Interviews",
     xlab="Interviews")
boxplot(data$interviews,
        main="Boxplot of Interviews",
        horizontal=TRUE)


print("Offers:")
mean(data$offers)
median(data$offers)
sd(data$offers)
shapiro.test(data$offers)
t.test(data$offers)$conf.int
hist(data$offers,
     breaks=10,
     main="Histogram of Offers",
     xlab="Offers")
boxplot(data$offers,
        main="Boxplot of Offers",
        horizontal=TRUE)


plot(data$applications,
     data$interviews,
     main="Applications vs Interviews",
     xlab="Applications",
     ylab="Interviews")
plot(data$applications,
     data$offers,
     main="Applications vs Offers",
     xlab="Applications",
     ylab="Offers")

cor(data[, c("applications", "interviews", "offers")])

data$application_range <- cut(data$applications,
                              breaks = c(0, 199, 299, 400),
                              labels = c("0-199", "200-299", "300-400"),
                              right = TRUE)

boxplot(interviews ~ application_range, data = data,
        main = "Boxplot of Interviews by Application Range",
        xlab = "Number of Applications", ylab = "Number of Interviews",
        col = c("lightblue", "lightgreen", "lightcoral"))

boxplot(offers ~ application_range, data = data,
        main = "Boxplot of Offers by Application Range",
        xlab = "Number of Applications", ylab = "Number of Offers",
        col = c("lightblue", "lightgreen", "lightcoral"))
