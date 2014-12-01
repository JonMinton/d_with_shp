# #####################################################################################################
# 
# require(reshape2)
# require(plyr)
# require(stringr)
# require(ggplot2)
# require(maptools)
# require(grid)
# require(spdep)
# require(Rcpp)
# require(MASS)
# require(CARBayes)
# 
# 
# ###################################################################################################
# # scripts
# source("scripts/functions/binomial_iar_car.r")
# source("scripts/functions/D_compute.r")
# sourceCpp("scripts/functions/cppfunctions.cpp")
# 
# 
# new_results <- reactive(
#   {
#     data_option <- input$data_option
#     
#     #################################################################################################
#     # DATA MANAGEMENT
#     # Load data for n/N
#     
#     if (data_option=="working_age_population"){
#       attribute_data <- read.csv("data/working_age_people_1996.csv")
#       attribute_data <- rename(attribute_data, replace=c("workingage_count" = "numerator_count"))
#     } else {
#       if (data_option=="council_houses"){
#         attribute_data <- read.csv("data/council_houses_2011.csv")
#         attribute_data <- rename(attribute_data, replace=c("councilhouse_count"="numerator_count"))
#       }  
#     }
#     
#     
#     # Load shapefiles
#     datazones_shp <- readShapeSpatial(
#       "shp/scotland_2001_datazones/scotland_dz_2001.shp"
#     )
#     
#     # add example_pop as to data slot in datazone_shp here?!
#     # If so, how?
#     
#     datazones_shp@data <- rename(datazones_shp@data, replace=c("zonecode"="datazone"))
#     
#     datazones_shp@data <- join(
#       datazones_shp@data,
#       attribute_data,
#       type="inner"
#     )
#     
#     datazones_shp <- datazones_shp[duplicated(datazones_shp@data$datazone)==F,]
#     
#     datazones_shp <- datazones_shp[datazones_shp@data$total_count > 0,]
#     
#     
#     # uses code from spdep
#     
#     ## Create the neighbourhood matrix
#     
#     W_nb <- poly2nb(datazones_shp)              
#     W_mat <- nb2mat(W_nb, style="B", zero.policy=TRUE)
#     
#     
#     #####################################################################################
#     #####################################################################################
#     
#     ## Run the Bayesian model
#     
#     # in the latest version of CARBayes 
#     # the function 
#     # binomial.iarCAR
#     # has been replaced with
#     # iarCAR.re 
#     # with the argument
#     # family="binomial"
#     
#     D_classical <- Dissimilarity.compute(
#       minority=datazones_shp@data$numerator_count,
#       total=datazones_shp@data$total_count
#     )
#     
#     
#     
#     model <- iarCAR.re(
#       formula = numerator_count  ~ 1,
#       trials = datazones_shp@data$total_count,
#       W=W_mat,
#       data=datazones_shp@data,
#       family="binomial"
#     )
#     
#     posterior.D <- array(NA, c(1000))
#     for(k in 1:1000){
#       p.current <- exp(
#         model$samples$phi[k ,] + model$samples$beta[k,1]
#       )   / (
#         1 + exp(
#           model$samples$phi[k ,] + model$samples$beta[k,1]
#         )
#       )
#       
#       p.current.overall <- sum(
#         p.current * datazones_shp@data$total_count
#       ) / sum(
#         datazones_shp@data$total_count
#       )
#       
#       posterior.D[k] <- sum(
#         datazones_shp@data$total_count * abs(p.current - p.current.overall)
#       ) / (
#         2 * sum(datazones_shp@data$total_count) * p.current.overall * (1-p.current.overall))     
#       
#     }
#     
#     
#     Dbayes <- round(quantile(posterior.D, c(0.5, 0.025, 0.975)),4)
#     output <- list(
#       car_bayes = list(
#         all_values = posterior.D,
#         summaries= list(
#           lower= quantile(posterior.D, 0.025),
#           median= quantile(posterior.D, 0.5),
#           upper = quantile(posterior.D, 0.975)
#           )
#         ),
#       classical = list(
#         all_values= D_classical$D.boot,
#         summaries = list(
#             lower = D_classical$D.estimate[1],
#             median = D_classical$D.estimate[2],
#             upper = D_classical$D.estimate[3]
#           )
#         )      
#       )
#     
#     return(output)
#   }
# )
# 
# 
# 


shinyServer(
  function(input, output){
    # INPUTS
    ## data_option
    this_selection <- reactive({
      input$data_option      
    })
      
    #these_results <- new_results()
    
    # OUTPUTS
    ## Table with quantiles
    ## histogram
    output$text <- renderText({
      h2(paste("The vignette is ", this_selection() ))      
    })
      
    output$plot <- renderPlot({
          plot(runif(1000))
        }
      )
  }
)

