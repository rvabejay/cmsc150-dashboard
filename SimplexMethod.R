############################################################################################
# Notes: 12/11 Modifications:
#
# (1) finding smallest test ratio algorithm is changed
#     (a) test_ratios_temp <- test_ratios[!is.na(test_ratios) & test_ratios > 0]
#     (b) if(length(test_ratios_temp) > 0)
#
# (2) commented some blocks of printing codes
#       
#       12/14 Modifications: 
# (1) updated basic solution depending on the type of objective
# (2) moved computing for basic solution upwards
# (3) added option to print everything in dataframes
#
############################################################################################
options(max.print = 99999)

SimplexMethod <- function(dataframe, objective = "maximize"){
  
  #initialize solution matrix
  solution <- matrix(data = NA, nrow = 1)
  solution <- as.data.frame(solution)
  
  #initialize tableau matrix
  tableau <- matrix(data = NA, nrow = nrow(dataframe), ncol = ncol(dataframe))
  
  #counter variable for the number of iterations
  iteration <- 0
  
  #check if input for objective variable is valid
  if((objective == "minimize") || (objective == "maximize")){
    
    df <- as.data.frame(dataframe)
    
    #boolean dummy to indicate whether the iteration should stop
    halt <- FALSE
    
    #set maximum no. of iterations
    max_iter <- 100000
    
    #iterate while indicator variable is false
    while(!halt){
      
      #print iteration number and dataframe
      cat("Iteration: ", iteration, "\n")
      print(df)
      cat("\n")

      n <- nrow(df)
      m <- ncol(df)
      
      #basic sol after
      #compute for the basic solution
      basic_solution <- c()
      
      if(objective == "maximize"){
        for(k in 1:(m - 1)){
          #get the current column
          currentCol <- df[ ,k]
          
          #dummies to count the number of ones and zeroes in the column
          ones <- 0
          zeroes <- 0
          
          #dummy for the row index of the row with one as element
          one_index <- 0
          
          for(l in 1:n){
            if(currentCol[l] == 1){
              ones <- ones + 1
              one_index <- l
            }else if(currentCol[l] == 0){
              zeroes <- zeroes + 1
            }
          }
          
          if((ones == 1) && (zeroes == (n-1))){
            basic_solution <- append(basic_solution, df[one_index, m])
          }else{
            basic_solution <- append(basic_solution, 0)
          }
        }
      }else{
        basic_solution <- df[n, c((1 : (m - 2)), m)]
      }
      
      cat("Basic Solution: \n")
      print(basic_solution)
      cat("\n")
      
      #get the last row of the dataframe
      df_lastrow <- df[n, ]
      
      #get the constants column
      const_col <- as.vector(df[ ,m])
      
      #get the index of the minimum value in the last row
      pivotCol_index <- which.min(df_lastrow)
      
      #check if the minimum is zero or is positive, which indicates that the process should stop
      if((df[n, pivotCol_index] == 0) || (df[n, pivotCol_index] > 0)){
        halt <- TRUE
        tableau <- df
        break
      }else{
        
        #get the pivot column
        pivot_col <- as.vector(df[ ,pivotCol_index])
        
        #create a vector for the test ratios
        test_ratios <- c()
        
        # cat("Pivot Column: \n")
        # print(pivot_col)
        # cat("\n")
        
        # cat("Pivot Column Index: ", colnames(dataframe)[pivotCol_index],"\n")
        
        #compute for the test ratios
        for(i in 1:(n-1)){
          if(pivot_col[i] == 0){
            test_ratios <- append(test_ratios, NA)
          }else{
            temp <- const_col[i]/pivot_col[i]
            
            if(temp < 0){
              test_ratios <- append(test_ratios, NA)
            }else{
              test_ratios <- append(test_ratios, temp)
            }
          }
        }
        
        # cat("Test Ratios: \n")
        # print(test_ratios)
        # cat("\n")
        
        #find the smallest test ratio
        # test_ratios_temp <- test_ratios[!is.na(test_ratios)]
        # test_ratios_temp <- test_ratios_temp[test_ratios_temp > 0]
        test_ratios_temp <- test_ratios[!is.na(test_ratios) & test_ratios > 0]
        # min_tr <- min(test_ratios_temp)
        
        #check if there is any positive minimum test ratio found
        if(length(test_ratios_temp) > 0){
          min_tr <- min(test_ratios_temp)
          pivotRow_index <- which(test_ratios == min_tr)
          
          #if there are two rows that have the smallest test ratios
          if(length(pivotRow_index) > 1){
            pivotRow_index <- pivotRow_index[1]
          }
          
          #get the pivot row
          pivot_row <- df[pivotRow_index, ]
          
          #get the pivot element
          pivot_element <- df[pivotRow_index, pivotCol_index]
          
          # cat("Minimum Test Ratio: \n")
          # print(min_tr)
          # cat("\n")
          # 
          # cat("Pivot Row: \n")
          # print(pivot_row)
          # cat("\n")
          # 
          # cat("Pivot Element: \n")
          # print(pivot_element)
          # cat("\n")
          
          #normalize the pivot row
          normalized <- pivot_row/pivot_element
          
          #eliminate remaining columns
          for (j in 1:n){
            
            if(j == pivotRow_index){
              df[j, ] <- normalized
            }else{
              #get the current row
              currentRow <- df[j, ]
              
              # cat("Current Row: \n")
              # print(currentRow)
              
              #get the element to be set to zero
              to_elim <- df[j, pivotCol_index]
              
              # cat("To Eliminate: \n")
              # print(to_elim)
              
              #update the current row
              currentRow <- currentRow - (normalized * to_elim)
              
              # cat("Updated Row: \n")
              # print(currentRow)
              
              #set the updated row to the jth row of df
              df[j, ] <- currentRow
              
              # cat("\n")
            }
          }
          
          #basic sol b4
          
          #modify solution vector depending on whether the objective is to minimize or maximize
          if(objective == "maximize") {
            solution <- as.data.frame(t(basic_solution))
          }else{
            solution <- df[n, c((1 : (m - 2)), m)]
          }
          
          varnames <- c(colnames(dataframe)[1: (m - 2)], "Z")
          colnames(solution) <- varnames
          
          #check if the current iteration exceeds or equals maximum iterations
          if(iteration == max_iter){
            halt <- TRUE
            tableau <- df
            break
          }
          
          #increment iteration variable
          iteration <- iteration + 1
          cat("\n")
          cat("=====================================================================")
          cat("\n")
          cat("\n")
          cat("\n")
        }else{
          #if the minimum test ratio is not found due to the test ratios being either zero or negative
          halt <- TRUE
          tableau <- df
          break
        }
      }
    }
  }else{
    solution <- "Invalid objective."
  }
  
  cat("\n")
  cat("=====================================================================")
  cat("\n")
  cat("\n")
  output <- list(initial_tableau = dataframe, iterations = iteration, final_tableau = tableau, solution = solution)
  return(output)
}
