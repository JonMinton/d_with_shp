#####################################################################################################
print("entered do_prereqs.r")

print("loading prereq packages")
require(reshape2)
require(plyr)
require(stringr)
require(ggplot2)
require(maptools)
require(grid)
require(spdep)

print("reading pop data")
example_pop <- read.csv("data/working_age_people_1996.csv")
print("reading house data")
example_house <- read.csv("data/council_houses_2011.csv")

print("reading shapefiles")
datazones_shp <- readShapeSpatial(
  "shapefiles/scotland_2001_datazones/scotland_dz_2001.shp"
)

print("Making proportions")
example_pop <- transform(example_pop, proportion=workingage_count/total_count)
example_house <- transform(example_house, proportion=councilhouse_count/total_count)

print("fortifying shapefiles")
datazones_shp@data$id <- rownames(datazones_shp@data)
id_name <- subset(datazones_shp@data, select=c("id", "zonecode"))

datazones_map <- fortify(datazones_shp)
datazones_map <- join(datazones_map, id_name)
datazones_map <- rename(datazones_map, replace=c("zonecode"="datazone"))


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

print("connecting pop to dzs")
pop_joined <- join(datazones_map, example_pop, by="datazone", type="full")
pop_joined <- arrange(pop_joined, group, order)

print("connecting house to dzs")
house_joined <- join(datazones_map, example_house, by="datazone", type="full")
house_joined <- arrange(house_joined, group, order)


##############

# Creating w matrix

# uses code from spdep

## Create the neighbourhood matrix
W_nb <- poly2nb(datazones_shp)              
names(W_nb) <- id_name[,2]
W_mat <- nb2mat(W_nb, style="B", zero.policy=TRUE)


#####################################################################################
#####################################################################################


##########################
#### Set up the simulation
##########################
#### Create the study region
# length <- 20
# x <- seq(0,1, length.out=length)
# y <- seq(0,1, length.out=length)
# grid <- expand.grid(x,y)
# colnames(grid) <- c("X", "Y")
# n.area <- nrow(grid)
 Dist <- as.matrix(dist(grid))
 Sigma <- exp(-Dist*0.9)

seed_value <- 20
mean_value <- mean(example_pop$proportion)
n_area <- nrow(example_pop)

# Define server logic for random distribution application
shinyServer(function(input, output) {
  output$sidebartitle <- renderText({"Input controls"})
  output$plottitle <- renderText({"Summary of the minority proportion data"})
  output$summarytitle <- renderText({"True and estimated dissimilarity indices and 95% uncertainty intervals"})
  
  
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  set.seed(seed_value)
  mean.logit <- log(mean_value / (1 - mean_value)) 
    logit.probs <- mvrnorm(n=1, 
                           mu=rep(mean.logit, n_area), 
                           Sigma=(input$sd*Sigma) # to sort out
                           )  
    probs <- exp(logit.probs) / (1 + exp(logit.probs))
  })
  
  
    y <- rbinom(
      n=n.area, 
      size=rep(example_pop$total_count, n_area), 
      prob=probs
      )
  

  
  #### Compute the true and estimated values of the dissimilarity index
  output$summary2 <- renderPrint({
    ## Compute the true value
    N <- rep(input$n.population,n.area)
    x.true <- round(probs() * N, 0)
    probs.overall <- sum(x.true) / sum(N)
    Dtrue <- sum(N * abs(probs() - probs.overall)) / (2 * sum(N) * probs.overall * (1-probs.overall))    
    
    ## Run the classical method
    Dclassical <- round(Dissimilarity.compute(data(), N, n.boot=1000)$D.estimate,4)
    
    ## Run the Bayesian model
    model <- binomial.iarCAR(formula=data()~1, trials=N, W=W, burnin=1000, n.sample=2000) 
    posterior.D <- array(NA, c(1000))
    for(k in 1:1000)
    {
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
  })
