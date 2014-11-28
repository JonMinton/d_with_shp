rm(list=ls())

#####################################################################################################

require(reshape2)
require(plyr)
require(stringr)
require(ggplot2)
require(maptools)
require(grid)
require(spdep)
require(Rcpp)
require(MASS)
require(CARBayes)


###################################################################################################
# scripts
source("scripts/functions/binomial_iar_car.r")
source("scripts/functions/D_compute.r")
sourceCpp("scripts/functions/cppfunctions.cpp")


#################################################################################################
# Specify clean theme
theme_clean <- function(base_size=12){
  theme_grey(base_size) %+replace%
    theme(
      axis.title=element_blank(),
      axis.text=element_blank(),
      panel.background=element_blank(),
      panel.grid=element_blank(),
      axis.ticks.length=unit(0, "cm"),
      axis.ticks.margin=unit(0, "cm"),
      panel.margin=unit(0, "lines"),
      plot.margin=unit(c(0,0,0,0), "lines"),
      complete=TRUE
    )
}


#################################################################################################
# DATA MANAGEMENT
# Load data for n/N

example_pop <- read.csv("data/working_age_people_1996.csv")
example_house <- read.csv("data/council_houses_2011.csv")


# Load shapefiles
datazones_shp <- readShapeSpatial(
  "shp/scotland_2001_datazones/scotland_dz_2001.shp"
)

# Create proportions
example_pop <- transform(example_pop, proportion=workingage_count/total_count)

# add example_pop as to data slot in datazone_shp here?!
# If so, how?

datazones_shp@data <- rename(datazones_shp@data, replace=c("zonecode"="datazone"))

# datazones_shp@data <- join(
#   datazones_shp@data,
#   example_pop,
#   type="inner"
#   )
 datazones_shp@data <- merge(
  datazones_shp@data,
  example_pop,
  by="datazone",
  all.y=T
  )
# > dim(datazones_shp)
# [1] 6610    9
# > dim(example_pop)
# [1] 6505    5
# > dim(datazones_shp@data)
# [1] 6610    9



##


# 
# zero_counts <- example_pop$datazone[example_pop$total_count == 0]
# 
# example_pop <- subset(
#   example_pop,
#   subset=total_count > 0
# )
# 
# # 
# example_house <- transform(example_house, proportion=councilhouse_count/total_count)
# 
# # fortify shapefiles
# datazones_shp@data$id <- rownames(datazones_shp@data)
# id_name <- subset(datazones_shp@data, select=c("id", "zonecode"))
# 
# # datazones_map <- fortify(datazones_shp)
# datazones_map <- join(datazones_map, id_name)
# datazones_map <- rename(datazones_map, replace=c("zonecode"="datazone"))
# 



# # Connect pop to dzs
# 
# pop_joined <- join(datazones_map, example_pop, by="datazone", type="full")
# pop_joined <- arrange(pop_joined, group, order)
# 
# # Connect house to dzs
# 
# house_joined <- join(datazones_map, example_house, by="datazone", type="full")
# house_joined <- arrange(house_joined, group, order)


##############

# Creating w matrix

# uses code from spdep

## Create the neighbourhood matrix
datazones_shp_ss <- datazones_shp[id_name$zonecode %in% example_pop$datazones,]
id_name_ss <- id_name[id_name$zonecode %in% example_pop$datazones,]

W_nb <- poly2nb(datazones_shp_ss)              
names(W_nb) <- id_name_ss[,2]
W_mat <- nb2mat(W_nb, style="B", zero.policy=TRUE)


#####################################################################################
#####################################################################################

## Run the Bayesian model

# in the latest version of CARBayes 
# the function 
# binomial.iarCAR
# has been replaced with
# iarCAR.re 
# with the argument
# family="binomial"

model <- iarCAR.re(
  formula = workingage_count  ~ 1,
  trials = example_pop$total_count,
  W=W_mat,
  data=example_pop,
  family="binomial"
)


##########################
#### Set up the simulation
##########################





seed_value <- 20
mean_value <- mean(example_pop$proportion)
sd_value <- sd(example_pop$proportion)
n_area <- nrow(example_pop)

# # to begin with assume no correlation 
# sigma <- diag(1, nrow=n_area, ncol=n_area)
# 
# 
# mean.logit <- log(mean_value / (1 - mean_value)) 
# 
# logit.probs <- mvrnorm(
#   n=1, 
#   mu=rep(mean.logit, n_area), 
#   Sigma=(sd_value*sigma) 
# )  
# 
# probs <- exp(logit.probs) / (1 + exp(logit.probs))
#   
#   
# y <- rbinom(
#   n=n.area, 
#   size=rep(example_pop$total_count, n_area), 
#   prob=probs
# )
#   

  
# N <- rep(input$n.population,n.area)
# x.true <- round(probs() * N, 0)
# probs.overall <- sum(x.true) / sum(N)
# Dtrue <- sum(N * abs(probs() - probs.overall)) / (2 * sum(N) * probs.overall * (1-probs.overall))    
    
## Run the classical method
D_classical <- Dissimilarity.compute(
  minority=example_pop$workingage_count+0.5,
  total=example_pop$total_count+0.5
  )
    


iarCAR.re
model <- binomial.iarCAR(formula=data()~1, trials=N, W=W, burnin=1000, n.sample=2000) 
posterior.D <- array(NA, c(1000))
for(k in 1:1000){
  p.current <- exp(model$samples$phi[k ,] + model$samples$beta[k,1])   / (1 + exp(model$samples$phi[k ,] + model$samples$beta[k,1])) 
  p.current.overall <- sum(p.current * rep(input$n.population,n.area)) / sum(rep(input$n.population,n.area))
  posterior.D[k] <- sum(rep(input$n.population,n.area) * abs(p.current - p.current.overall)) / (2 * sum(rep(input$n.population,n.area)) * p.current.overall * (1-p.current.overall))     
}

Dbayes <- round(quantile(posterior.D, c(0.5, 0.025, 0.975)),4)
    
## Save the results
results2 <- array(NA, c(2,3))
rownames(results2) <- c("Classical results", "Bayesian results")
colnames(results2) <- c("", "", "")
results2[1 , ] <- Dclassical
results2[2 , ] <- Dbayes
results2 <- round(results2, 4)
    
results1 <- Dtrue    
results <- list(results1, results2)
names(results) <- c("True value of D", "Estimated values of D")
results     
