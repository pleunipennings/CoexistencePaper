##https://eriqande.github.io/2015/01/22/solving-differential-equations-in-R.html

## Gottesman 
## treatment went from 7000 to 5000 DDD per month per 167000 people. 

## 7000 / (167 * 30) = 1.4 DDD / 1000 preintervention
## 5000 / (167 * 30) = 1 DDD / 1000 during intervention

## how much is 1.4 in t? 

#t = (1.4 / 1000) * (365 /5) = 0.1 preintervention
#t = (1 / 1000) * (365 /5) = 0.073during intervention 


library(deSolve)
library(ggplot2)

parameters <- c(tx = 0.073, e = 0.1, c = 0.7)
#parameters <- c(r = 0.3, K = 10000, fom = 0)
state <- c(N = 0.12)
#state <- c(N = 150)
RAPS <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    # rate of change 
    #dN <- (1 - fom) * (r * N * (1 - N / K))
    dN <- tx*e*(1-N) + -(1-N)*N*c
    # return the result
    list(dN)
  }) # end with(as.list ...
}

times <- seq(0, 2, by = 0.1) ##We are only looking at half a year

costs <- seq(0.4, 0., by = 0.04)
#costs <- c(0.04, 0.08, 0.12)
names(costs) <- paste("cost =", costs)

parametersbefore <- c(tx = 0.1, e = 0.7, c = 0.08)
parametersafter <- c(tx = 0.073, e = 0.7, c = 0.08)
stateBefore <- c(N = 0.)
stateAfter <- c(N = 0.12)

list_results_before <- lapply(costs, function(x) {
  parametersafter["c"] <- x
  ode(y = stateAfter, times = times, func = RAPS, parms = parametersafter)
})

ldf <- do.call(rbind, lapply(names(list_results_before), function(x) data.frame(list_results_before[[x]], x, stringsAsFactors = FALSE)))

# plot with ggplot
ggplot(data = ldf, aes(x = time, y = N, color = x)) + geom_line()

