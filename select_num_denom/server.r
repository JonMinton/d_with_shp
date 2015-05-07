
# Notes and to-dos


# 1) include checks for if numerators are greater than denominators
# 2) include feedback to tell user to wait when performing large calculations
# 3) change order of US outputs to make more intuitive sense
# 4) 

# server - load prerequisities --------------------------------------------

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
require(shiny)
require(dplyr)



 

# Server - load scripts  --------------------------------------------------
source("scripts/D_compute.r")
sourceCpp("scripts/cppfunctions.cpp")
# 


# server - load data  -----------------------------------------------------


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

shinyServer(function(input, output, server){
  
  ###############################################################################################
  #####  Observer FUNCTIONS #####################################################################
  ###############################################################################################
  run_model <- reactive(
    {    
      cat("entered run_model\n")
      out <- NULL
      w <- generate_w_matrix()
      dta <- combine_input_table()
    
      if (!is.null(w) & !is.null(dta)){     
        cat("in inner of run_model\n")
        w_dz <- rownames(w)
        # remove duplicates
        w <- w[!duplicated(w_dz), !duplicated(w_dz)]
        w_dz <- rownames(w)
        dta_dz <- dta$datazone
        ss <- intersect(w_dz, dta_dz)
        tmp <- w_dz %in% ss
        w <- w[tmp, tmp]
        dta <- subset(dta, datazone %in% ss)
        
        mdl <- S.CARiar(
          formula= numerator  ~ 1,
          trials = dta$denominator,
          W=w,
          data=dta,
          family="binomial"
        )
        out <- list(
          datazones = dta_dz,
          denominator = dta$denominator,
          beta = mdl$samples$beta[,1],
          phi = mdl$samples$phi
          )
      }
      return(out)
  })
   
  generate_posterior_distribution <- eventReactive(
    input$generate_posterior_button,
    {
      cat("entered generate_posterior_distribution function\n")
      model_outputs <- run_model()
      cat("returned to generate_posterior_distribution. Browsing\n")
      
      K <- input$posterior_sample_size      
      phi <- model_outputs$phi
      beta <- model_outputs$beta
      denominator <- model_outputs$denominator
      
      out <- array(NA, K)
      for(k in 1:K){
        p.current <- exp(phi[k ,] + beta[k])   / (1 + exp(phi[k ,] + beta[k]))
        p.current.overall <- sum(p.current * denominator) / sum(denominator)
        out[k] <- sum(denominator * abs(p.current - p.current.overall)) / 
          ( 2 * sum(denominator) * p.current.overall * (1-p.current.overall))                 
      }
      return(out)
  })
  
  
  summarise_posterior_distributions <- eventReactive (
    input$generate_posterior_button,
    {
      cat("entered summarise_posterior_distributions function\n")
      bayes <- generate_posterior_distribution()
      classical <- calc_d_classical()$D.boot
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
  
  
  load_shapefiles <- eventReactive(
    input$load_shapefile_button, 
    {
     
      out <- readShapeSpatial(
        "shp/scotland_2001_datazones/scotland_dz_2001.shp"
      )      
      out@data <- rename(out@data, datazone=zonecode)
          
      return(out)
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
  
  load_data <- reactive({
    file_name <- input$option
    data <- read.csv(paste0("data/", file_name, ".csv"))
    if (input$option_la!="all"){
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
    cat("Entered server:get_labels\n")
    labels <- levels(load_data()$type) %>% as.character() %>% sort()
    return(labels)
  })
  

  
  generate_w_matrix <- eventReactive(
    input$make_w_matrix_button, 
    {
      out <- NULL
      dta <- link_shp_with_attributes()
      if (!is.null(dta)){
        w_nb <- poly2nb(dta)
        out <- nb2mat(w_nb, style="B", zero.policy=TRUE)
        rownames(out) <- colnames(out) <- dta@data$datazone        
        }  
      return(out)
  })
  
  combine_input_table <- eventReactive(
    input$ok_num_denom,
    {
        
      numerators <- input$numerator_selection
      denominators <- input$denominator_selection
      out <- NULL
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
      cat("in server:numerator\n")

      selections <- get_labels()
      selectInput("numerator_selection", "Select numerator", choices=selections, multiple=T)
    })
  
    output$denominator <- renderUI({
      cat("in server:denominator\n")
      selections <- get_labels()
      selectInput("denominator_selection", "select denominator", choices=selections, multiple=T)
    })
  
  
  ##############################################################################################
  ### OUTPUTS ##################################################################################
  ##############################################################################################

  
  
    output$show_combined_input_table <- renderTable({
      out <- combine_input_table() %>%
        as.data.frame %>%
        head
      return(out)
    })
  
    output$report_attributes_linked <- renderText({
      dta <- link_shp_with_attributes() 
      if (is.null(dta)){
        out <- "The data has not been merged yet"
      } else {
        out <- "the data have been merged"
      }
      return(out)
    })
  
    output$all_checks <- renderUI({
      # This will replace a number of other renderText functions, creating a 
      # single dynamic paragraph that will report on the state of various 
      # prerequisites.
      
      # The structure of the output will read
      
      # (1)"The numerator and denominator [have/have not] been selected"
      # (2)"[The numerator and denominator selection is valid as no numerators are greater
      # than their denominators] / 
      # [The numerator and denominator selection is not valid because 
      # [[XXX]] numerators are greater than their denominators. Please choose again.]"
      # (3) "The numerators/denominators [have/have not] been merged to the shapefiles."
      # (4) "The shapefile [has/has not] been loaded"
      # (5)"[The shapefile is needed to calculate the neighbourhood matrix]/
      # [The shapefile has been loaded but the neighbourhood matrix has not yet been calculated]/
      # [The neighbourhood matrix has been calculated]
      # (6)"[All areal units are included. Warning: This may take some time"] /
      # [Only one local authority has been selected]
      # (7) [The model [has not]/[has] been run"
      
      # (1)"The numerator and denominator [have/have not] been selected"
      tmp <- combine_input_table()
      out_01 <- if (is.null(tmp)){
        return("The numerator and denominator have not been selected")
      } else {
        return(
          paste(
            "The numerator and denominator have been selected and",
            "has", dim(tmp)[1], "rows")
          )
      }
      # (2)"[The numerator and denominator selection is valid as no numerators are greater
      # than their denominators] / 
      tmp2 <- tmp$denominator - tmp$numerator
      out_02 <- if (any(tmp2 < 0)){
          return(
            paste(
              "The numerator/denominator selection is invalid as",
              length(tmp2[tmp2 < 0]), "numerators are greater than",
              "the corresponding denominators. <strong>Please choose again</strong>"
              )
            )
        } else {
          return(
            paste("The numerator/denominator selection is valid as",
                 "no denominators are smaller than the corresponding",
                  "numerators"
                 )
          )
        }
      # (3) "[The shapefile has not been loaded]/
      # [The shapefile has been loaded and has length [[XXX]]"
      tmp3 <- load_shapefiles()
      
      
      out_03 <- if (is.null(shapefiles)){
          return("Shapefiles not yet loaded")
        } else {
          return(
            paste(
              "The shapefiles have been loaded and have length", length(tmp3)
            )
          )
        }
      
      
      return(HTML(
        paste(
          "<p>Data report:</p>",
          "<ul>",
            "<li>", out_01, "</li>",
            "<li>", out_02, "</li>",
            "<li>", out_03, "</li>",
          "</ul>"
        )
    })
  
  
    output$report_w_matrix_generated <- renderText({
      cat("Entered report_w_matrix_generated\n")
      tmp <- generate_w_matrix()
      if (is.null(tmp)){
        out <- "The w matrix has not been generated"
      } else {
        out <- paste("The w matrix has been generated and has dimensions", 
                     dim(tmp)[1], " by ", dim(tmp)[2])
      }
      return(out)
    })
  
    output$show_posterior_distribution <- renderPlot({
      samples <- generate_posterior_distribution() 

      if (!is.null(samples)){
        samples <- data.frame(value=samples)
        thresholds <- input$seg_k
        samples$filled <- "yes"
        samples$filled[samples$value <= thresholds[1] | samples$value >= thresholds[2]] <- "no"
        prop_in_band <- length(samples$filled[samples$filled=="yes"])/length(samples$filled) %>%
          round(., 2)
        
        out <- samples %>% ggplot(data=., aes(x=value, fill=filled)) + 
          geom_histogram(binwidth=0.01) +
          scale_fill_manual(values=c("yes"="black", "no"= "lightgray"), guide=FALSE) +
          geom_vline(
            xintercept=thresholds,
            linetype="dotted",
            linewidth=2.5
            ) + 
          coord_cartesian(xlim=c(0,1)) +
          annotate("text", x=mean(thresholds), y=-1, col="red", fontface="bold", label=prop_in_band)
                  
      } else {out <- NULL}
      return(out)
    })
  
    output$tabulate_posterior <- renderTable({
      out <- summarise_posterior_distributions()
      
      return(out)
      
    })
  
 })