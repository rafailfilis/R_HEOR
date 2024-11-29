
library(ggplot2)
set.seed(123)
n_cohort <- 200
data_A <- data.frame(
     ID = seq(1:n_cohort),
     Treatment = rep("A", n_cohort),
     Cost_A = rgamma(n_cohort, shape = 1, scale = 3000/1),
     Effect_A = rnorm(n_cohort, mean = 2.5, sd = 0.5)
)

data_B <- data.frame(
     ID = seq(1:n_cohort),
     Treatment = rep("B", n_cohort),
     Cost_B = rgamma(n_cohort, shape = 1, scale = 4000/1),
     Effect_B = rnorm(n_cohort, mean = 3, sd = 0.5)
)
 
data <- cbind(data_A, data_B)
n_psa <- 1000
deterministic_costA <- mean(data$Cost_A)
deterministic_costB <- mean(data$Cost_B)
deterministic_effA <- mean(data$Effect_A)
deterministic_effB <- mean(data$Effect_B)

probabilistic_costA <- rgamma(n_psa, shape = 1, scale = deterministic_costA/1)
probabilistic_costB <- rgamma(n_psa, shape = 1, scale = deterministic_costB/1)
probabilistic_effA <- rnorm(n_psa, mean = 2.5, sd = 0.5)
probabilistic_effB <- rnorm(n_psa, mean = 2.5, sd = 0.5)


psa_data <- data.frame(
      Cost_A = probabilistic_costA,
      Effect_A = probabilistic_effA,
      Cost_B = probabilistic_costB,
      Effect_B = probabilistic_effB,
      delta_cost  = NA,
      delta_eff = NA
) 
 
df_icers <- data.frame(
     ID = seq(1:nrow(psa_data)),
     ICER = rep(NA, n_cohort)
)
  
for (i in 1:nrow(psa_data)) {
  
  # Extract the cost and effect data for the current simulation
  simulation_data <- psa_data[i,]
   
  
  # Calculate ICER for each patient in this simulation
  
    psa_data[i, "delta_cost"] <- simulation_data$Cost_B  - simulation_data$Cost_A 
    psa_data[i, "delta_eff"] <- simulation_data$Effect_B  - simulation_data$Effect_A 
       
    # Avoid division by zero or negative effects (if needed)
    if (delta_eff != 0) {
      df_icers$ICER[i] <- psa_data[i, "delta_cost"]  / psa_data[i, "delta_eff"]
    } else {
      df_icers$ICER[i] <- NA  # Assign NA if effect difference is zero
   
  }
  
 
   
}
 
wtp <- c(100, 500, 800, 1000, 5000, 10000, 15000)


# calculate probabilities of cost effective combinations
print(nrow(df_icers))
a_curv_data <- data.frame(
  wtp = wtp,
  probability = sapply(wtp, function(w) sum(df_icers$ICER < w) / nrow(df_icers))
)
 basic_wtp <- 4000
psa_data[,"cost_effective"] <-  ifelse(psa_data[, "delta_eff"] * basic_wtp > psa_data[, "delta_cost"]  , "Cost-Effective", "Not Cost-Effective")
 

 print(psa_data[1, ])
ce_plane <- ggplot(data = psa_data , aes(x= delta_eff , y = delta_cost ,  color= cost_effective ))+
              geom_point(alpha = 0.7 ) +  
              geom_abline(slope = basic_wtp , intercept = 0, colour= "red" ) +
              geom_hline(yintercept = 0) +
              geom_vline(xintercept = 0)+
              scale_color_manual(values = c("Cost-Effective" = "green" , "Not Cost-Effective" = "blue")) + 
              labs(
                  title = "Cost-Effectiveness Plane",
                  x = "Incremental Effect",
                  y = "Incremental Cost",
                  color = "Cost-Effective"
                ) +
                theme(
                     axis.title = element_text(size = 14),
                    axis.text = element_text(size = 12),
                    title = element_text(size = 16)
                )
              
acceptability_curve <- ggplot(data = a_curv_data,  aes(x = wtp, y = probability)) + geom_point(alpha = 0.7, size= 10 , colour = "green") +
                            geom_line(alpha = 5 ,size= 2, colour = "blue") + labs(
                                 title = "Acceptability Curve",
                                 x = "Willingness to pay [WTP]",
                                 y = "% of being cost-effective"
                            ) +
                             theme(
                                axis.title = element_text(size = 20),
                                title =  element_text(size = 22),
                                axis.text.x = element_text(size = 20),
                                axis.text.y = element_text(size = 20)
                             ) 

 
 print(acceptability_curve)
 