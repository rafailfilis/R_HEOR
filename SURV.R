library(survival)
library(dplyr)
library(ggplot2)

dataA <- data.frame(
     id = seq(1:100),
    Time = sample(c(1:15) , 100 , replace = TRUE ),
     Treatment = sample(c("A", "B"), 100 , replace= TRUE),
     Event = sample(c(1, 0) ,100, replace = TRUE)
)

 

time_frame <- c(1,3,5,7,9,11,13,15,17)


km <- survfit(Surv(dataA$Time, dataA$Event) ~ dataA$Treatment)


p <- plot(km, xlab = "Time", ylab = "Survival Probability", main = "Kaplan-Meier Survival Curve by Treatment")
 


print(p)

