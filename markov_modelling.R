
library(dplyr)

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
 
for(i in 2:nrow(markov_data)){
     
      payoff_trace[i,] <- as.matrix(markov_data[i, -1]) %*% payoff
      
      # Calculate the total cost for this step
        total_costs[i] <- sum(payoff_trace[, 1])
        total_qalys[i] <- sum(payoff_trace[,2])
       
}

payoff_trace<- cbind(payoff_trace, total_costs, total_qalys)

#  total costs and qalys 
 c_total <- sum(payoff_trace[,1])/n_cohort
 q_total <- sum(payoff_trace[,2])/n_cohort



# time-varying probabilities (what if transition_probs change when time passes by)