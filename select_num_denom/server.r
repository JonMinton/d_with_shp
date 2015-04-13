# Notes and to-dos

# Check the code in the posterior distribution generator section

# Warning in dta$denominator * abs(p.current - p.current.overall) :
#   longer object length is not a multiple of shorter object length

# #####################################################################################################
library(Rcpp)

# require(stringr)
 require(ggplot2)
 require(maptools)
# require(grid)
 require(spdep)
 require(Rcpp)
# require(MASS)
require(plyr)
require(tidyr)
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
# find local authorities

la_to_dz <- read.csv("data/la_to_dz.csv") 

las <- c("all",
    la_to_dz %>%
    group_by(local_authority) %>%
    summarise %>%
    .$local_authority %>%
    as.vector
  )
     
      #####################################################################################
      #####################################################################################

shinyServer(function(input, output, session){
  
  ###############################################################################################
  #####  Observer FUNCTIONS #####################################################################
  ###############################################################################################
  run_model <- observe({
    input$run_model_button # this is the trigger
    
    out <- NULL
    input$run_model_button
    
    w <- generate_w_matrix()
    dta <- combine_input_table()
    
    if (!is.null(w) & !is.null(dta)){      
      w_dz <- rownames(w)
      # remove duplicates
      w <- w[!duplicated(w_dz), !duplicated(w_dz)]
      w_dz <- rownames(w)
      dta_dz <- dta$datazone
      ss <- intersect(w_dz, dta_dz)
      tmp <- w_dz %in% ss
      w <- w[tmp, tmp]
      dta <- subset(dta, datazone %in% ss)
      
      #Adding try as workaround to issue that on some machines 
      # S.CARiar works but on others iarCAR.re works, even 
      # though the version of CARBayes reported is the same (4.0)
      out <- try(S.CARiar(
        formula= numerator  ~ 1,
        trials = dta$denominator,
        W=w,
        data=dta,
        family="binomial"
      ))
      if (class(out)=="try-error"){
        out <- iarCAR.re(
          formula= numerator  ~ 1,
          trials = dta$denominator,
          W=w,
          data=dta,
          family="binomial"          
        )}
    } 
    return(out)
  })
  
  generate_posterior_distribution <- observe({
    input$generate_posterior_button # this is the trigger
    
    model <- run_model()
    dta <- combine_input_table()
    K <- input$posterior_sample_size      
    
    out <- array(NA, K)
    for(k in 1:K){
      p.current <- exp(model$samples$phi[k ,] + model$samples$beta[k,1])   / (1 + exp(model$samples$phi[k ,] + model$samples$beta[k,1]))
      p.current.overall <- sum(p.current *dta$denominator) / sum(dta$denominator)
      out[k] <- sum(dta$denominator * abs(p.current - p.current.overall)) / ( 2 * sum(dta$denominator) * p.current.overall * (1-p.current.overall))                 
    }
    return(out)
  })
  
  
  summarise_posterior_distributions <- observe ({
    input$generate_posterior_button # this is the trigger
    
    
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
  
  
  
  
  load_shapefiles <- observe({
    input$load_shapefile_button # this is the trigger
     
    out <- readShapeSpatial(
      "shp/scotland_2001_datazones/scotland_dz_2001.shp"
    )      
    out@data <- rename(out@data, datazone=zonecode)
        
    return(out)
  })
  
  load_data <- reactive({
    file_name <- input$option
    data <- read.csv(paste0("data/", file_name, ".csv"))
    if (input$option_la!=""){
      dzs <-  la_to_dz  %>% 
        filter(local_authority==input$option_la)  %>%
        .$datazone %>%
        as.vector
      data <- data %>%
        filter(datazone %in% dzs)
    }
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
  
  generate_w_matrix <- observe({
    input$make_w_matrix_button # this is the trigger
    
    out <- NULL
    dta <- link_shp_with_attributes()
    if (!is.null(dta)){
      w_nb <- poly2nb(dta)
      out <- nb2mat(w_nb, style="B", zero.policy=TRUE)
      rownames(out) <- colnames(out) <- dta@data$datazone        
      }  
    return(out)
  })
  
  combine_input_table <- observe({
    input$ok_num_denom # this is the trigger
    
    numerators <- isolate(input$numerator_selection)
    denominators <- isolate(input$denominator_selection)
    if (!is.null(numerators) & !is.null(denominators)){
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
      } 
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
      out <- combine_input_table() %>%
        as.data.frame %>%
        head
      return(out)
    })
  
    output$text02 <- renderText({
      dta <- link_shp_with_attributes() 
      if (is.null(dta)){
        out <- "The data has not been merged yet"
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
        out <- "The w matrix has not been generated"
      } else {
        out <- paste("The w matrix has been generated and has dimensions", 
                     dim(tmp)[1], " by ", dim(tmp)[2])
      }
      return(out)
    })
  
    output$plot01 <- renderPlot({
      samples <- generate_posterior_distribution() 

      if (!is.null(samples)){
        samples <- data.frame(value=samples)
        out <- samples %>% ggplot( aes(x=value)) + 
          geom_density +
          geom_vline(
            xintercept=isolate(input$seg_k),
            linetype="dotted",
            linewidth=2.5
            ) + 
          coord_cartesian(xlim=c(0,1))
          
      } else {out <- NULL}
      return(out)
    })
  
  output$text04 <- renderText({
    
    samples <- generate_posterior_distribution() 
    if (!is.null(samples)){
      k_lower <- isolate(input$seg_k[1])
      k_higher <- isolate(input$seg_k[2])
      tmp <- samples[samples > k_lower & samples < k_higher]
      tmp <- length(tmp) / length(samples)
      tmp <- round(tmp, 2)
      out <- paste("The probability that the true values falls within the thresholds is", tmp) 
    } else {
      out <- "The model has not yet been run."
    }
    return(out)
  }) 
})