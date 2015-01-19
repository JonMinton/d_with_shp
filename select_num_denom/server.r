# Notes and to-dos

# Check the code in the posterior distribution generator section

# Warning in dta$denominator * abs(p.current - p.current.overall) :
#   longer object length is not a multiple of shorter object length

# #####################################################################################################
# 
# require(reshape2)
library(Rcpp)

# require(stringr)
# require(ggplot2)
 require(maptools)
# require(grid)
 require(spdep)
 require(Rcpp)
# require(MASS)
 require(CARBayes)
library(dplyr)
# 
# 
# ###################################################################################################
# # scripts
# source("scripts/functions/binomial_iar_car.r")
source("scripts/D_compute.r")
sourceCpp("scripts/cppfunctions.cpp")
# 

 

      
      
      #####################################################################################
      #####################################################################################
      



shinyServer(function(input, output){
  
  ###############################################################################################
  #####  REACTIVE FUNCTIONS #####################################################################
  ###############################################################################################
  summarise_posterior_distributions <- reactive ({
    bayes <- generate_posterior_distribution()
    classical <- calc_d_classical()
    n_digits <- 4
    
    if (!is.null(bayes) & !is.null(classical)){
      qi_bayes <- bayes %>% quantile(c(0.025, 0.5, 0.975)) %>% round(4)
      qi_classical <- classical %>% quantile(c(0.025, 0.5, 0.975)) %>% round(4)
      
      out <- data.frame(
        method=c("Classical", "Bayesian"),
        lower=c(qi_classical[1], qi_bayes[1]),
        middle=c(qi_classical[2], qi_bayes[2]),
        upper=c(qi_classical[3], qi_bayes[3])
      )
      
    } else {out <- NULL}
    return(out)        
  })
  
  generate_posterior_distribution <- reactive({
    model <- run_model()
    dta <- combine_input_table()
    K <- input$posterior_sample_size      
    go <- input$generate_posterior_button
    
    if (go){
      out <- array(NA, K)
      for(k in 1:K){
        p.current <- exp(model$samples$phi[k ,] + model$samples$beta[k,1])   / (1 + exp(model$samples$phi[k ,] + model$samples$beta[k,1]))
        p.current.overall <- sum(p.current *dta$denominator) / sum(dta$denominator)
        out[k] <- sum(dta$denominator * abs(p.current - p.current.overall)) / ( 2 * sum(dta$denominator) * p.current.overall * (1-p.current.overall))                 
      }
      
      
    } else {out <- NULL}
    return(out)
  })
  
  run_model <- reactive({
    
    w <- generate_w_matrix()
    dta <- combine_input_table()
    go <- input$run_model_button
    
    
    if (go & !is.null(w) & !is.null(dta)){      
      w_dz <- rownames(w)
      # remove duplicates
      w <- w[!duplicated(w_dz), !duplicated(w_dz)]
      w_dz <- rownames(w)
      dta_dz <- dta$datazone
      ss <- intersect(w_dz, dta_dz)
      tmp <- w_dz %in% ss
      w <- w[tmp, tmp]
      dta <- subset(dta, datazone %in% ss)
      
      
      out <- S.CARiar(
        formula= numerator  ~ 1,
        trials = dta$denominator,
        W=w,
        data=dta,
        family="binomial"
      )  
      
    } else {out <- NULL}
    
    return(out)
    
  })
  
  load_shapefiles <- reactive({
    go <- input$load_shapefile_button
    
    if (go){
      out <- readShapeSpatial(
        "shp/scotland_2001_datazones/scotland_dz_2001.shp"
      )      
      out@data <- rename(out@data, datazone=zonecode)
      
    } else {out <- NULL}
    return(out)
  })
  
  load_data <- reactive({
    file_name <- input$option
    data <- read.csv(paste0("data/", file_name, ".csv"))
    return(data)
    })
    
  summarise_data <- reactive({
    data <- load_data()
    out <- data %>% group_by(type) %>% summarise(count=sum(count))
    out <- data.frame(out)
    return(out)
  })
  
  get_labels <- reactive({
    labels <- levels(load_data()$type) %>% as.character() %>% sort()
    return(labels)
  })
  
  link_shp_with_attributes <- reactive({
    shp_data <- load_shapefiles()    
    att_data <- combine_input_table()
    
    if (!is.null(shp_data) & !is.null(att_data)){
      out <- shp_data
      # The data can be loaded
      out@data <- plyr::join(out@data, att_data)
    } else {
      # The data cannot be linked
      out <- NULL
    }
    return(out)
  })
  
  generate_w_matrix <- reactive({
    go <- input$make_w_matrix_button
    if (go){
      dta <- link_shp_with_attributes()
      if (!is.null(dta)){
        w_nb <- poly2nb(dta)
        out <- nb2mat(w_nb, style="B", zero.policy=TRUE)
        rownames(out) <- colnames(out) <- dta@data$datazone        
      } else { out <- NULL}      
    } else {out <- NULL}
    return(out)
  })
  
  combine_input_table <- reactive({

    
    go <- input$ok_num_denom
    
    numerators <- isolate(input$numerator_selection)
    denominators <- isolate(input$denominator_selection)
    
    if (go & !is.null(numerators) & !is.null(denominators)){
      cat("numerators: ")
      for (i in 1:length(numerators)) {cat(numerators[i], "\n")}
      cat("\n\n")

      cat("denominators: ")
      for (i in 1:length(denominators)) {cat(denominators[i], "\n")}
      cat("\n\n")
      
      
      data_raw <- load_data()
      data_raw <- data_raw %>% dplyr::select(datazone, type, count)
      
      data_numerator <- data_raw %>% 
        filter(type %in% numerators) %>%
        group_by(datazone) %>% summarise(numerator=sum(count))
      
      data_denominator <- data_raw %>% 
        filter(type %in% denominators) %>%
        group_by(datazone) %>% summarise(denominator=sum(count))
      
      
      data_out <- inner_join(data_denominator, data_numerator)
      out <- data_out                  
    } else {out <- NULL}

    return(out)
  })
  
  calc_d_classical <- reactive({
    dta <- combine_input_table()
    out <- Dissimilarity.compute(
      minority=dta$numerator,
      total=dta$denominator
      ) 
    return(out)
  })

  #############################################################################################
  ### REACTIVE UIS ############################################################################
  #############################################################################################
    output$numerator <- renderUI({
      selections <- get_labels()
      selectInput("numerator_selection", "Select numerator", choices=selections, multiple=T)
    })
  
    output$denominator <- renderUI({
      selections <- get_labels()
      selectInput("denominator_selection", "select denominator", choices=selections, multiple=T)
    })
  
  
  ##############################################################################################
  ### OUTPUTS ##################################################################################
  ##############################################################################################
    output$text01 <- renderText({
      shapefiles <- load_shapefiles()
      if (is.null(shapefiles)){
        out <- "Shapefiles not yet loaded"
      } else {
        out <- paste("The length of the shapefile object is ", length(shapefiles))
      }
      return(out)
    })
  
  
    output$table01 <- renderTable({
      out <- combine_input_table()
      out <- as.data.frame(out)
      out <- head(out)
      return(out)
    })
  
    output$text02 <- renderText({
      dta <- link_shp_with_attributes() 
      if (is.null(dta)){
        out <- "The data cannot be merged yet"
      } else {
        out <- "the data have been merged"
      }
      return(out)
    })
  
    output$table02 <- renderTable({
      dta <- link_shp_with_attributes()
      if (!is.null(dta)){
        out <- head(dta@data)        
      } else {out <- NULL}
      return(out)
    })
  
    output$text03 <- renderText({
      tmp <- generate_w_matrix()
      if (is.null(tmp)){
        out <- "The w matrix cannot be generated"
      } else {
        out <- paste("The w matrix has been generated and has dimensions", 
                     dim(tmp)[1], " by ", dim(tmp)[2])
      }
      return(out)
    })
  
    output$plot01 <- renderPlot({
      samples <- generate_posterior_distribution() 
      
      if (!is.null(samples)){
        out <- plot(density(samples))
      } else {out <- NULL}
      
      return(out)
    }) 
})