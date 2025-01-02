############################################################################################
# 
#   Clean server script
# 
#
############################################################################################

#necessary packages
library(shiny)
library(ggplot2)
library(readxl)
library(DT)
library(shinyjs)

#import functions to be used
source("PolyRegression.R")
source("QuadraticSplineInterpolation.R")
source("TableauConstruction.R")
source("SimplexSolution.R")
source("SimplexMethod.R")

server <- function(input, output, session){
  
  
  ############################################################################################
  # 
  #   POLYNOMIAL REGRESSION 
  #   To work on:    
  #   (1) Clear whenever there is a new file input
  #
  #
  ############################################################################################
  
  #function that reads the file input when the load button is clicked
  pr_data <- eventReactive(input$pr_fileLoad,{
    #require that the user has selected a file
    req(input$pr_input)
    
    #error catching mechanism
    tryCatch({
      df <- read.csv(input$pr_input$datapath, header = input$pr_header)
      
      #check if the file input has two columns
      if(ncol(df) != 2){
        stop("File input does not have two columns.")
        df <- NULL
      }else{
        x <- df[,1]
        y <- df[,2]
        
        #check if both x and y columns are of equal length
        if(length(x) != length(y)){
          stop("Length of x and y values are not equal.")
          df <- NULL
        }else{
          x_class <- is.numeric(x)
          y_class <- is.numeric(y)
          
          #check each element of x and y columns are numeric
          if(!x_class || !y_class){
            stop("Either x or y or both x and y are not of numeric type.")
            df <- NULL
          }
        }
      }
      return(df)
    }, error = function(e){
      return(NULL)
    })
  })
  
  
  #Generate an HTML table view of the data input
  output$fileInput <- renderDataTable({
    datatable(pr_data(),
              options = list(
                scrollX = TRUE,
                scrollY = "300px",
                pageLength = 10,
                info = TRUE
              ))  
  })
  
  
  #function that outputs a message for whether the file is loaded successfully.
  output$pr_fileLoading <- renderText({
    
    #Check if if the output of the pr_data() function is null or otherwise.
    if(is.null(pr_data())){
      return("File loaded unsuccessfully. Acceptable files have two columns with equal number of rows and are both of numeric type.")
    }else{
      return("File loaded successfully.")
    }
  })
  
  
  #function that fits a polynomial regression model based on the user file input and specified order
  #will be executed only when the "Fit Model" button is clicked
  #returns a list
  polyreg <- eventReactive(input$pr_modelFitting, {
    
    #check if the output of pr_data() function is not null
    if(!is.null(pr_data())){
      
      #input data
      df <- pr_data()
      
      #x column
      x <- df[,1]
      
      #user-specified function order
      order <- input$pr_order
      
      #check if specified order is valid
      #if order is less than 0 or greater than (n-1) or is not an integer, input is invalid
      if((order > (length(x) - 1)) || (order < 0) || (!is.integer(order))){
        return(0)
      }else{
        #if user input is valid, fit a model using the PolynomialRegression() function
        fitted <- PolynomialRegression(input$pr_order, pr_data())
        
        #return the function output
        return(fitted)
      }
    }else{
      return(NULL)
    }
  })
  
  
  #function that returns the polynomial string of the fitted model
  output$fittedModel <- renderText({
    
    #require that the button "Fit Model" is clicked before executing the function
    req(input$pr_modelFitting)
    
    polyreg_result <- polyreg()
    
    #check the function polyreg() outputs
    if(is.null(polyreg_result)){
      return("File input is invalid, hence no model can be fit.")
    }else if(is.double(polyreg_result)){
      return("You have entered an invalid function order, hence a model was not fitted.")
    }else{
      #obtain the polynomial string output from the list output
      fitted_string <- polyreg_result$polynomial_string
      
      #return the polynomial string
      return(fitted_string)
    }
  })
  
  
  #function that returns the plot of the observations and the fitted model
  output$modelPlot <- renderPlot({
    
    #require that the button "Fit Model" is clicked before executing the function
    req(input$pr_modelFitting)
    
    polyreg_result <- polyreg()
    
    #check if polyreg_result is a list output
    if(is.list(polyreg_result)){
      
      #get the dataframe used by polyreg()
      #ensures that the plot doesnt change when a new file is loaded
      df <- polyreg_result$original_data
      
      #obtain the polynomial function output form the list output
      fitted_model <- polyreg_result$polynomial_function
      
      #get the variable names from the file input
      df_vars <- colnames(df)
      
      #plot of the observations and fitted line
      p <- ggplot(df, aes(x = df[,1], y = df[,2])) +
        #plot the observations as points
        geom_point(aes(color = "Observed Data")) +
        
        #plot the fitted model
        stat_function(fun = fitted_model, aes(color = "Fitted Model")) +
        
        #plot title and x and y axis labels
        labs(title = "Observed Data vs. Fitted Model", x = df_vars[1], y = df_vars[2]) +
        
        #set the colors for the points and line/curve in the plot
        scale_color_manual(values = c("Observed Data" = "red", "Fitted Model" = "blue"), name = "Legend") +
        
        #modify the legend positioning and plot background
        theme(legend.position = c(0.01, 0.99), legend.justification = c(0, 1), legend.text = element_text(size = 8), legend.title = element_text(size = 8),  panel.background = element_rect(fill = "#F5F5F5")) +
        
        #change the label for the legend, set a circle for the observed data and a line for the fitted model
        guides(color = guide_legend(override.aes = list(shape = c(NA, 16), linetype = c(1, 0), size = c(1, 2))))
      
      #check if the button pr_runEstimate is clicked (indicating that an estimate was derived or the value of the estimate has changed)
      if (!is.null(input$pr_runEstimate) && input$pr_runEstimate > 0) {
        
        #add a point to the current plot to indicate where does it fall in the fitted model
        
        #create a dataframe containing the user input and the estimated value
        estimate_point <- data.frame(x = inputEstimate(), y = polyreg_result$polynomial_function(inputEstimate()))
        
        #add a point in the existing plot for the estimated value
        p <- p + geom_point(data = estimate_point, aes(x = x, y = y), color = "forestgreen", size = 2, shape = 16) +
          
          #add a label indicating the value of the point
          geom_text(data = estimate_point, aes(x = x, y = y,label = sprintf("%.2f", y)), color = "forestgreen", vjust = -0.5, size = 3)
      }
      
      #return the plot
      return(p)
      
    }else{
      print("Invalid function order or file input is invalid.")
      
      #display a message in the plot when the function order/file input is invalid
      
      #create an empty plot
      ggplot() +
        
        #add a text to the empty plot
        annotate("text", x = 0.5, y = 0.5, label = "No plot to be shown.", size = 8, color = "red", fontface = 2) +
        
        #remove axes, labels, titles in the plot
        theme_void()
    }
  }, res = 90)
  
  
  #function that outputs the estimate at the specified user input
  #changes automatically w/o clicking the "Estimate" button when the function order is changed and "Fit Model" button is clicked
  #estimate is updated when user input is changed and "Estimate" button is clicked
  observeEvent(input$pr_modelFitting,{
    
    #the following code is executed either when the button "Fit Model" is clicked or when "Estimate" is clicked
    output$estimatedVal <- eventReactive(input$pr_runEstimate,{
      
      #require that the button "Fit Model" is clicked before executing the function
      req(input$pr_modelFitting)
      
      polyreg_result <- polyreg()
      
      #check if polreg_result is a list output
      if(is.list(polyreg_result)){
        
        #obtain the fitted model output
        fitted_model <- polyreg_result$polynomial_function
        
        #determine the function output at the user-specified value
        estimate <- fitted_model(input$pr_estimate)
        inputEstimate(input$pr_estimate)
        
        return(estimate)
      }else{
        print("Invalid function order.")
        return("No estimate can be derived.")
      }
    })
  })
  
  
  #create a reactive value that will be updated everytime the button pr_runEstimate is clicked
  inputEstimate <- reactiveVal(NULL)
  
  #function that executes only when the button pr_runEstimate is clicked
  observeEvent(input$pr_runEstimate,{
    
    #Updates inputEstimate when pr_runEstimate is clicked
    if (!is.null(input$pr_runEstimate) && input$pr_runEstimate > 0) {
      
      #set the value of inputEstimate as the user input pr_estimate
      inputEstimate(input$pr_estimate)
    }
  })
  
  #functions that change the active tab in the tabset depending on the button clicked
  observeEvent(input$pr_fileLoad,{
    updateTabsetPanel(session, "PR_tabs", selected = "Input")
  })
  
  observeEvent(input$pr_modelFitting,{
    updateTabsetPanel(session, "PR_tabs", selected = "Output")
  })
  
  observeEvent(input$pr_runEstimate,{
    updateTabsetPanel(session, "PR_tabs", selected = "Output")
  })
  
  
  
  ############################################################################################
  # 
  #   QUADRATIC SPLINE INTERPOLATION
  #   To work on:    
  #   (1) Clear whenever there is a new file input
  #   (2) Point of the interpolated value in the plot
  #
  ############################################################################################
  
  
  #function that reads the file input when the load button is clicked
  qsi_data <- eventReactive(input$qsi_fileLoad,{
    #require that the user has selected a file
    req(input$qsi_input)
    
    #error catching mechanism
    tryCatch({
      df <- read.csv(input$qsi_input$datapath, header = input$qsi_header)
      
      #check if the file input has two columns
      if(ncol(df) != 2){
        stop("File input does not have two columns.")
        df <- NULL
      }else{
        x <- df[,1]
        y <- df[,2]
        
        #check if both x and y columns are of equal length
        if(length(x) != length(y)){
          stop("Length of x and y values are not equal.")
          df <- NULL
        }else{
          x_class <- is.numeric(x)
          y_class <- is.numeric(y)
          
          #check each element of x and y columns are numeric
          if(!x_class || !y_class){
            stop("Either x or y or both x and y are not of numeric type.")
            df <- NULL
          }
        }
      }
      return(df)
    }, error = function(e){
      return(NULL)
    })
  })
  
  
  #Generate an HTML table view of the data input
  output$qsi_fileInput <- renderDataTable({
    datatable(qsi_data(),
              options = list(
                scrollX = TRUE,
                scrollY = "400px",
                pageLength = 10,
                info = TRUE
              )  
    )
  })
  
  
  #function that outputs a message for whether the file is loaded successfully.
  output$qsi_fileLoading <- renderText({
    
    #Check if if the output of the pr_data() function is null or otherwise.
    if(is.null(qsi_data())){
      return("File loaded unsuccessfully. Acceptable files have two columns with equal number of rows and are both of numeric type.")
    }else{
      return("File loaded successfully.")
    }
  })
  
  
  #function that obtains the fitted quadratic spline interpolation
  #will be executed only when qsi_run is clicked
  #returns a list if input is valid
  qsi <- eventReactive(input$qsi_run, {
    if(!is.null(qsi_data())){
      
      df <- qsi_data()
      n <- nrow(df)
      
      #check if data input is valid for QSI
      if(n < 3){
        return(0)
      }else{
        #fit QSI on the data
        qsi_fit <- QuadraticSplineInterpolation(df)
        return(qsi_fit)
      }
    }else{
      return(NULL)
    }
  })
  
  
  #function to print the dataframe containing the lower and upper bounds and the interval functions of the fitted QSI
  output$fittedQSI <- renderDataTable({
    
    #require that qsi_run is clicked before executing
    req(input$qsi_run)
    
    #get the result of qsi()
    qsi_result <- qsi()
    
    #conditional statements to check whether file input is valid
    if(is.null(qsi_result)){
      qsi_df <- "File input is invalid, hence quadratic spline interpolation cannot be performed."
    }else if(is.double(qsi_result)){
      qsi_df <- "Data points in file input is not enough to perform quadratic spline interpolation."
    }else if (any(is.na(qsi_result$dataframe))){
      qsi_df <- "Quadratic spline interpolation was not fitted in the data points provided."
    }else{
      
      #get the dataframe output of qsi_result
      qsi_df <- qsi_result$dataframe
    }
    
    
    #transform qsi_df as a dataframe (to handle error when file input is invalid)
    qsi_df <- as.data.frame(qsi_df)
    
    #setup the datatable to be shown
    datatable(
      qsi_df,
      options = list(
        scrollX = TRUE,
        scrollY = "300px",
        pageLength = 10,
        info = TRUE
      )  
    )
  })
  
  
  #function to determine the interpolated value at given user input
  #requires qsi_run to be clicked before execution (QSI functions should be generated already)
  observeEvent(input$qsi_run, {
    
    #requires run_Interpolation to be clicked before further execution
    output$estimatedQSIVal <- eventReactive(input$qsi_runInterpolation, {
      
      #further require that qsi_run is clicked
      req(input$qsi_run)
      
      #obtain result of qsi()
      qsi_result <- qsi()
      
      #conditional statements to check if there are QSI functions generated
      if(is.null(qsi_result)){
        return("No interpolated value can be derived.")
      }else if(is.double(qsi_result)){
        return("No interpolated value can be derived.")
      }else if (any(is.na(qsi_result$dataframe))){
        return("No interpolated value can be derived.")
      }else{
        
        #get the dataframe output of qsi()
        qsi_df <- qsi_result$dataframe
        
        #set the upper and lower bounds as numeric (to ensure consistency)
        min_limit <- as.numeric(qsi_df[1,1])
        max_limit <- as.numeric(qsi_df[nrow(qsi_df),2])
        
        #get the user input
        qsi.estimate.input <- as.numeric(input$qsi_interpolate)
        
        #conditional statement if the input value is within bounds of the dataset
        if((qsi.estimate.input < min_limit) || (qsi.estimate.input > max_limit)){
          return("Input value is out of data bounds, hence value is invalid.")
        }else{
          
          #dummy variable
          function_index <- 0
          
          #find the interval where the input falls in
          for(i in 1:nrow(qsi_df)){
            interval.lower.limit <- as.numeric(qsi_df[i,1])
            interval.upper.limit <- as.numeric(qsi_df[i,2])            
            
            if((qsi.estimate.input >= interval.lower.limit) && (qsi.estimate.input <= interval.upper.limit)){
              function_index <- i
              break
            }
          }
          
          #get the function corresponding to the interval
          qsi_function_list <- qsi_result$functionlist
          interval_function <- qsi_function_list[[function_index]]
          
          #compute for the estimate
          estimate <- interval_function(qsi.estimate.input)
          return(estimate)
        }
      }
    })
  })
  
  
  #function that returns the plot of the interval functions and the observations
  output$QSIPlot <- renderPlot({
    
    #require that qsi_run is clicked
    req(input$qsi_run)
    
    qsi_result <- qsi()
    
    #conditional statements to determine the plot output
    if(is.null(qsi_result) || is.double(qsi_result) || any(is.na(qsi_result$dataframe))){
      #create an empty plot
      ggplot() +
        
        #add a text to the empty plot
        annotate("text", x = 0.5, y = 0.5, label = "No plot to be shown.", size = 8, color = "red", fontface = 2) +
        
        #remove axes, labels, titles in the plot
        theme_void()
    }else{
      
      #get the interval functions
      qsi_function_list <- qsi_result$functionlist
      
      #get the dataframe containing the lower and upper bounds per interval
      qsi_df <- qsi_result$dataframe
      
      #get the data used as basis for the QSI
      qsi_data <- qsi_result$data
      
      #set the column names of the dataframe
      colnames(qsi_data) <- c("x", "y")
      
      #plot of the observations (scatter plot)
      p <- ggplot(qsi_data, aes(x = x, y = y)) +
        #plot the observations as opints
        geom_point(size = 2, color = "red") +
        
        #add plot labels
        labs(
          title = "Observations vs. Fitted Quadratic Spline Interpolation",
          x = colnames(qsi_data)[1],
          y = colnames(qsi_data)[2],
          color = "Function No."
        ) +
        
        #add varying colors to the interval functions
        scale_color_discrete(name = "Function No. ", limits = 1:length(qsi_function_list)) + 
        theme(panel.background = element_rect(fill = "#F5F5F5"))
      
      #add each interval function to the plot
      #lapply executes the function to every index
      p <- p + lapply(1:length(qsi_function_list), function(j){
        #get the interval function at current index
        interval_func <- qsi_function_list[[j]]
        
        #get the range for this interval
        x_range <- c(qsi_df[j,1], qsi_df[j,2])
        
        #create the stat function plot
        stat_function(
          fun = interval_func,
          geom = "line",
          aes(color = factor(j)),
          #to limit the plot of the function to the interval it belongs
          xlim = x_range
        )
      })
      p
    }
  }, res = 90)
  
  
  #conditional statements to change the active tab in the tabset depending on the button clicked
  observeEvent(input$qsi_fileLoad,{
    updateTabsetPanel(session, "QSI_tabs", selected = "Input")
  })
  
  observeEvent(input$qsi_run,{
    updateTabsetPanel(session, "QSI_tabs", selected = "Output")
  })
  
  observeEvent(input$qsi_runInterpolation,{
    updateTabsetPanel(session, "QSI_tabs", selected = "Output")
  })
  
  
  ############################################################################################
  # 
  #   SIMPLEX IMPLEMENTATION
  #   To work on:    
  #   (1) 
  #   
  #
  ############################################################################################
  
  #reactive value that updates when food items are selected, or when the buttons select all and reset are clicked
  selected_items <- reactiveVal(character(0))
  
  #action when select all button is clicked
  observeEvent(input$select_all, {
    #update the checkbox group input so that all items are selected
    updateCheckboxGroupInput(session, "checkboxes", selected = food_items)
    
    #update the reactive value
    selected_items(food_items)
  })
  
  #action when reset button is clicked
  observeEvent(input$reset, {
    #update checkbox group input so that no food item is selected
    updateCheckboxGroupInput(session, "checkboxes", selected = character(0))
    
    #update the reactive value
    selected_items(character(0))
  })
  
  #action when food items in the checkbox group input is clicked
  observeEvent(input$checkboxes, {
    #update reactive value with the food items selected
    selected_items(input$checkboxes)
  })
  
  #function to print the food details of the selected food items
  #dynamically updates, every time a food is selected/unselected, the table updates
  output$selected_food_data <- renderDataTable({
    #get the chosen food items
    selected <- selected_items()
    
    #get the dataframe containing the chosen food items
    choices <- selectedFoods(selected)
    
    #setup the dataframe to be shown
    datatable(
      choices,
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        pageLength = 10,
        info = TRUE
      )  
    )
  })
  
  
  #function that solves for the optimum diet given user input
  #runs only when solve_diet button is clicked
  solveDiet <- eventReactive(input$solve_diet, {
    
    #obtain the chosen food items
    selected <- selected_items()
    
    #determine if there are any selected foods in the vector
    if(length(selected) != 0){
      
      #subset the data 
      foodData <- selectedFoods(selected)
      
      #obtain initial matrix
      initialMat <- initializeMatrix(foodData)
      
      #obtain dual of the matrix 
      dualMat <- dualization(initialMat$init_mat, initialMat$no_of_foods)
      
      #run simplex method and obtain solution vector
      tempSoln <- SimplexMethod(dualMat, "minimize")$solution
      
      if(any(is.na(tempSoln))){
        return(0)
      }else{
        #determine optimal diet
        optDiet <- DeriveSolution(tempSoln, selected)
        
        #conditional to determine if there is an optimum diet in the selected food items
        if(class(optDiet) == "character"){
          return(0)
        }else if(class(optDiet) == "list"){
          return(optDiet)
        }
      }
    }else{
      #return NULL if there is no food items selected
      return(NULL)
    }
  })
  
  #function to output the initial tableau
  output$initTableau <- renderDataTable({
    #require that solve_diet is clicked before execution
    req(input$solve_diet)
    
    #obtain the chosen food items
    selected <- selected_items()
    
    #determine if there are any selected foods in the vector
    if(length(selected) != 0){
      
      #subset the data 
      foodData <- selectedFoods(selected)
      
      #obtain initial matrix
      initialMat <- initializeMatrix(foodData)
      
      #obtain dual of the matrix 
      dualMat <- dualization(initialMat$init_mat, initialMat$no_of_foods)
      
    }else{
      #return NULL if there is no food items selected
      dualMat <- "You havent selected any food items yet."
    }
    
    dualMat <- as.data.frame(dualMat)
    
    #setup the dataframe to be shown
    datatable(
      dualMat,
      options = list(
        scrollX = TRUE,
        scrollY = "400px",
        pageLength = 10,
        info = TRUE
      )  
    )
  })

  
  #function to print the optimum diet
  output$optimumDiet <- renderDataTable({
    
    #require that solve_diet is clicked before execution
    req(input$solve_diet)
    
    #get the result of solveDiet()
    opt.diet <- solveDiet()
    
    #conditional statements to determine the opt.diet return value
    if(is.null(opt.diet)){
      opt.diet <- "You haven't selected any food items yet."
    }else if(is.double(opt.diet)){
      opt.diet <- "It is not possible to meet the nutritional constraints with the foods that you have selected."
    }else{
      opt.diet <- opt.diet$optimal.diet
    }
    
    #transform opt.diet as a dataframe (to handle errors when there is no optimal diet)
    opt.diet <- as.data.frame(opt.diet)
    
    #setup the datatable to be shown
    datatable(
      opt.diet,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        pageLength = 10,
        info = TRUE
      )  
    )
  })
  
  
  #function to print the console outputs for simplex
  output$dietComputation <- renderPrint({
    
    #code is the same as with solveDiet(), but derivation of optimal diet is cut off
    req(input$solve_diet)
    selected <- selected_items()
    
    if(length(selected) == 0){
      print("You haven't selected any food items yet.")
    }else{
      foodData <- selectedFoods(selected)
      initialMat <- initializeMatrix(foodData)
      dualMat <- dualization(initialMat$init_mat, initialMat$no_of_foods)
      dietSolve <- SimplexMethod(dualMat, "minimize")
      dietSolve
    }
  })
  
  #conditional statements to change the active tab in the tabset depending on the button clicked
  observeEvent(input$select_all,{
    updateTabsetPanel(session, "Simplex_tabs", selected = "Inputs")
  })
  
  observeEvent(input$reset,{
    updateTabsetPanel(session, "Simplex_tabs", selected = "Inputs")
  })
  
  observeEvent(input$checkboxes,{
    updateTabsetPanel(session, "Simplex_tabs", selected = "Inputs")
  })
  
  observeEvent(input$solve_diet,{
    updateTabsetPanel(session, "Simplex_tabs", selected = "Output")
  })
  
  
  ############################################################################################
  # 
  #   HOME PAGE
  #     
  #   
  #
  ############################################################################################
  
  #functions to change the active tab in the navbarPage depending on the button clicked
  observeEvent(input$QSI, {
    updateTabsetPanel(session, "main", selected = "Quadratic Spline Interpolation")
  })
  
  observeEvent(input$PR, {
    updateTabsetPanel(session, "main", selected = "Polynomial Regression")
  })
  
  observeEvent(input$SM, {
    updateTabsetPanel(session, "main", selected = "Simplex Implementation")
  })
  
  #functions for the bottom panel buttons functionality
  observeEvent(input$About, {
    updateTabsetPanel(session, "main", selected = "About")
  })
  
  shinyjs::useShinyjs()
  
  #create javascript script
  shinyjs::runjs("
    shinyjs.openLink = function(link) {
      window.open(link, '_blank');
    }
  ")
}