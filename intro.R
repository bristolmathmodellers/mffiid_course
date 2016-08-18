## http://sbfnk.github.io/mfiidd/ course
# 18 August 2016

## Set up course package
# install.packages("devtools")
library(devtools)
# install_github("sbfnk/fitR@fitcourse_2016")
library(fitR)

## Lecture 1: Introduction slides http://sbfnk.github.io/mfiidd/slides/intro_slides.pdf
data(SIR)
# example "fitmodel" object which summarizes a basic SIR model
names(SIR)
SIR$name
SIR$state.names
SIR$theta.names
# look at functions:
SIR$simulate
SIR$dprior
SIR$dPointObs
SIR$rPointObs

#2.1
# need to define values for the parameters and initial vals
theta <- c(R0 = 3, D_inf = 2)
init.state <- c(S = 999, I = 1, R = 0)
times <- 1:100
traj <- SIR$simulate(theta, init.state, times) 
# simulate uses solver "ode"

head(traj)
plotTraj(traj) # fitR built in function to facet plot

## Prior
SIR$dprior(theta)
SIR$dprior # dprior function specific to each model (so this one looks for R0 and D_inf within theta)
SIR$dprior(theta, log = TRUE)
# calculates the density of the uniform distribution for each, and returns the sum

## Likelihood
SIR$dPointObs(data.point = c(obs = 18), model.point = c(I = 31), theta, log = TRUE)
SIR$dPointObs
# answers the question - what is the probability of observing 18 cases when the prevalence is 31 assuming that observations 
# follow a Poisson distribution centred around the current prevalence

data(epi)
head(epi1)
plotTraj(data=epi1) # if specify data then it makes points instead of lines

dTrajObs # function simulates model, evaluates log likelihood at every input data point, returns sum of all log-likelihood values
dTrajObs(SIR, theta, init.state, epi1, log=TRUE)

# Generate Observations
# rPointObs generates a single random observation from the state of the model...
SIR$rPointObs(model.point = c(I = 31), theta)
# not actually based on theta though??
# so run rTrajObs to simulate the model, apply rPointObs at every tiem point and return simulated trajectory and observaitons
rTrajObs
obs.traj <- rTrajObs(SIR, theta, init.state, epi1$time)
head(obs.traj)


## Calculate the posterior
# "Code it yourself"

# This is a function that takes 4 arguments:
# - fitmodel, a fitmodel object that defines the model dynamics, prior and likelihoods.
# - theta, a named vector of parameters
# - init.state, a named vector of initial states
# - data, the data set we are fitting the model to 
# It should return the posterior for the given model, parameters, initial state and data.
my_dLogPosterior <- function(fitmodel, theta, init.state, data) {
  
  # calculate the `fitmodel` log-prior for parameter `theta`
  prior <- fitmodel$dprior(theta, log=TRUE)
  
  # calculate the `fitmodel` log-likelihood for parameter `theta` and
  # initial state `init.state`, with respect to the data set `data`.
  ll <- dTrajObs(fitmodel, theta, init.state, data, log=TRUE)
  
  # return the logged posterior probability
  post <- ll + prior # add because they are logged
  
  return(post)
}

my_dLogPosterior(SIR, theta, init.state, epi1)

## Assess model fit
plotFit(SIR, theta, init.state, epi1)
plotFit
?plotFit

## Explore the posterior





## Imperfect reporting of cases





## Including demographic processes and seasonal forcing

