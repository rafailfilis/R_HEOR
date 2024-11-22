
library(dplyr)
library(ggplot2)
# markov chains (basic case)

n_cohort <- 1000  # num of patients at first

n_states <- 3 #healthy, diseased, dead
states <- c("healthy", "diseased", "dead")
n_cycle <- 40 
# transition probabilities table 

transition_probs <- c(0.85, 0.1, 0.05,
                      0, 0.85, 0.15,
                      0, 0, 1)

transition_matrix <- matrix(transition_probs, nrow = 3, ncol = 3, byrow = TRUE, dimnames = list(from=states, to= states) )

# create the data frame for results visualization 

markov_data <- data.frame(
    cycle = seq(1:n_cycle),
    healthy = NA,
    diseased = NA,
    dead = NA
)

# set the n_cohort in the first cycle

markov_data[1, -1] <- c(n_cohort, 0, 0) 

for(i in 2:nrow(markov_data)){
     
      markov_data[i,-1] <- as.matrix(markov_data[i-1 ,-1]) %*% transition_matrix
      
}



# accounting for the relative costs & QALYS of its state 

params<- list(
    # costs
     c_healthy = 500,
     c_diseased = 1500,
     c_dead = 0,

    #  QALYS

    q_healthy = 15,
    q_diseased = 10,
    q_dead = 0


)
payoff <- matrix(NA, nrow = 3, ncol = 2, dimnames = list(states, c("COST", "QALYS")))

payoff[,1] <- c(params$c_healthy, params$c_diseased, params$c_dead)
payoff[,2] <- c(params$q_healthy, params$q_diseased, params$q_dead)

# calculate costs and qalys based on the markov simulation 
payoff_trace <- data.frame(
     Cost = NA,
     QALYs = NA
     
)
payoff_trace[1,] <- as.matrix(markov_data[1,-1]) %*% payoff

total_costs <- numeric(nrow(markov_data))

total_qalys <- numeric(nrow(markov_data))
 

 calculate_payoff <- function(data){
for(i in 2:nrow(data)){
     
      payoff_trace[i,] <- as.matrix(data[i, -1]) %*% payoff
      
      # Calculate the total cost for this step
        total_costs[i] <- sum(payoff_trace[, 1])
        total_qalys[i] <- sum(payoff_trace[,2])
      
}

     payoff_trace <- cbind(payoff_trace, total_costs, total_qalys)
   }

payoff_trace <- calculate_payoff(markov_data)

#  total costs and qalys 
 c_total <- sum(payoff_trace[,1])/n_cohort
 q_total <- sum(payoff_trace[,2])/n_cohort


# plot the markov model simulation progress
p <- ggplot(data= markov_data , aes(x = cycle)) +
    geom_point(aes(y = healthy), colour = "blue") +
   geom_point(aes(y = diseased), colour = "green") + 
   geom_point(aes(y = dead), color = "red") +
   theme_light() +
  labs(
      x = "Cycle",
      y = "Different states",
      title = "Markov simulation progress" 
  )  +
  theme(
      title = element_text(size = 30, margin = margin(b = 10)),
      axis.title = element_text(size = 20)
  )
   
print(p)
# time-varying probabilities (what if transition_probs change when time passes by)

# e.x. from 1 to 10 cycle where qx = 0.05
#            healthy  diseased         dead 
#   healthy    #     0.10               qx
#   diseased   0        #           0.15 + qx
#   dead       0           0             1


transition_matrix1 <- array(NA, dim = c(n_states, n_states, n_cycle)) 

qx <- 0.04
 
transition_matrix1[3,3,] <- 1

transition_matrix1[1, 2, 1:10] <- 0.15
transition_matrix1[1, 3, 1:10] <- 0.04 #qx
transition_matrix1[2, 3, 1:10] <- 0.15 + transition_matrix1[1, 3, 1:10]
transition_matrix1[2, 2, 1:10] <- 1 - transition_matrix1[2, 3, 1:10]
transition_matrix1[1,1, 1:10] <- 1 - transition_matrix1[1, 2, 1:10] - transition_matrix1[1, 3, 1:10]

transition_matrix1[1, 2, 11:20] <- 0.15
transition_matrix1[1, 3, 11:20] <- 0.12 #qx
transition_matrix1[2, 3, 11:20] <- 0.15 + transition_matrix1[1, 3, 11:20]
transition_matrix1[2, 2, 11:20] <- 1 - transition_matrix1[2, 3, 11:20]
transition_matrix1[1,1, 11:20] <- 1 - transition_matrix1[1, 2, 11:20] - transition_matrix1[1, 3, 11:20]


transition_matrix1[1, 2, 21:30] <- 0.15
transition_matrix1[1, 3, 21:30] <- 0.13 #qx
transition_matrix1[2, 3, 21:30] <- 0.15 + transition_matrix1[1, 3, 21:30]
transition_matrix1[2, 2, 21:30] <- 1 - transition_matrix1[2, 3, 21:30]
transition_matrix1[1,1, 21:30] <- 1 - transition_matrix1[1, 2, 21:30] - transition_matrix1[1, 3, 21:30]

transition_matrix1[1, 2, 31:40] <- 0.15
transition_matrix1[1, 3, 31:40] <- 0.15 #qx
transition_matrix1[2, 3, 31:40] <- 0.15 + transition_matrix1[1, 3, 31:40]
transition_matrix1[2, 2, 31:40] <- 1 - transition_matrix1[2, 3, 31:40]
transition_matrix1[1,1, 31:40] <- 1 - transition_matrix1[1, 2, 31:40] - transition_matrix1[1, 3, 31:40]

transition_matrix1[is.na(transition_matrix1)] <- 0


markov_data1 <-  data.frame(
    cycle = seq(1:n_cycle),
    healthy = NA,
    diseased = NA,
    dead = NA
)
markov_data1[1,-1] <- c(n_cohort, 0, 0 )
for(i in 2:nrow(markov_data1)){
      markov_data1[i, -1] <- as.matrix(markov_data1[i-1, -1]) %*% transition_matrix1[, , i-1]
}
 
# calculate the costs and qualys 
payoff_trace <- calculate_payoff(markov_data1)

#  total costs and qalys - time-varying
 c_total <- sum(payoff_trace[,1])/n_cohort
 q_total <- sum(payoff_trace[,2])/n_cohort


 # plot the markov model simulation progress (time-varying)
p1 <- ggplot(data= markov_data1 , aes(x = cycle)) +
    geom_point(aes(y = healthy), colour = "blue") +
   geom_point(aes(y = diseased), colour = "green") + 
   geom_point(aes(y = dead), color = "red") +
   theme_light() +
  labs(
      x = "Cycle",
      y = "Different states",
      title = "Markov simulation progress (time-varying)" 
  )  +
  theme(
      title = element_text(size = 30, margin = margin(b = 10)),
      axis.title = element_text(size = 20)
  )
 
print(c_total)