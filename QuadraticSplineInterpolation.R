QuadraticSplineInterpolation <- function(data){
  
  #Sort the data ascendingly
  x_var <- colnames(data)[1]
  data <- data[order(data[[x_var]]), ]
  
  #Create vectors for the x and y values
  x <- data[[1]]
  y <- data[[2]]
  
  #Number of equations for QSI
  n <- nrow(data) - 1
  
  #Stop proceeding with computation if data provided is not enough
  if((n+1) < 3){
    stop("Quadratic spline interpolation requires at least 3 data points.")
  }
  
  #Number of rows and columns for the equations
  rows <- 3 * n
  cols <- (3 * n) + 1
  
  #Create matrix for the system of equations to be generated
  system.mat <- matrix(data = NA, ncol = cols, nrow = rows, byrow = TRUE, dimnames = NULL)
  
  #Dummy variable for the row in the matrix to be updated
  curr_row <- 1
  
  #First Condition for QSI
  for(i in 2:n){
    
    #Values of X and Y at i
    a.term <- (x[i])^2
    b.term <- x[i]
    y.val <- y[i]
    
    for(j in 1:2){
      
      #Create a vector of the same length as the number of columns
      temp <- vector("numeric", length = cols)
      
      #Set the last element as the ith y value
      temp[cols] <- y.val
      
      #Create the sequence 1 4 4 7 7 10 10... or the index where the matrix is non zero for each row
      start.index <- (i - j) * 3 + 1
      
      #Change elements at the range
      temp[start.index:(start.index + 2)] <- c(a.term, b.term, 1)
      
      #Modify the current row
      system.mat[curr_row, ] <- temp
      
      #Increment counter
      curr_row <- curr_row + 1
    }
  }
  
  
  #Second Condition for QSI
  for(k in c(1, (n+1))){
    
    #Values of X and Y at K
    a.term <- (x[k])^2
    b.term <- x[k]
    y.val <- y[k]
    
    #Create a vector of the same length as the number of columns
    temp <- vector("numeric", length = cols)
        
    #Set the last element as the ith y value
    temp[cols] <- y.val
    
    #Change elements at the range
    if(k == 1){
      temp[1: 3] <- c(a.term, b.term, 1)
    }else{
      temp[(cols-3):(cols-1)] <- c(a.term, b.term, 1)
    }
    
    #Modify the current row
    system.mat[curr_row, ] <- temp
    
    #Increment counter
    curr_row <- curr_row + 1
  }
  
  #Third Condition for QSI
  for(l in 2:n){
    
    a.term <- x[l] * 2
    
    #Create a vector of the same length as the number of columns
    temp <- vector("numeric", length = cols)
    
    #Dummy variable for the sequence 1 4 7 10... or the index where the matrix is nonzero
    start.index <- (l - 2) * 3 + 1
    
    #Change elements at the range
    temp[(start.index):(start.index + 1)] <- c(a.term, 1)
    temp[(start.index + 3): (start.index + 4)] <- c(-a.term, -1)
    
    #Modify the current row
    system.mat[curr_row, ] <- temp
    
    #Increment counter
    curr_row <- curr_row + 1
  }
  
  #Create vector for the coefficient names
  base <- c("a", "b", "c")
  coefnames <- c() 
  
  for(m in 1:n){
    coefnames <- c(coefnames, paste(base, m, sep = ""))
  }
  
  #Set the column names of system.mat as coefnames
  colnames(system.mat) <- c(coefnames, "const")
  
  #Remove first column and last row of the matrix
  #Since a1 = 0
  system.mat <- system.mat[, -1]
  system.mat <- system.mat[-rows, ]
  
  #Update coefnames
  coefnames <- coefnames[-1]
  
  #Get the function output of Gauss Jordan Method
  solution <- GaussJordanMethod(coefnames, system.mat)
  
  if(any(!is.na(solution$solution))){
    #Create vectors for the function return values
    varnames <- c("a1", coefnames)
    coeff.value <- c(0, solution$solution)
    
    #Create the function strings
    
    #Vector to store all functions 
    function.strings <- c()
    reference <- c("x^2", "x")
    
    for(o in 1:n){
      #Starting string
      interval.func <- "function(x) "
      
      #Starting index
      index <- (3 * o) - 2
      
      #Retrieve coefficients for the interval function
      coefficients <- coeff.value[index:(index + 2)]
      
      terms <- c()
      
      #Create individual term strings
      for(p in 1:3){
        if(p != 3){
          term <- paste(coefficients[p], reference[p], sep = " * ")
        }else{
          term <- coefficients[p]
        }
        #Append to vector
        terms <- append(terms, term)
      }
      
      #Build the function strings
      for(r in 1:3){
        if(r != 1){
          interval.func <- paste(interval.func, terms[r], sep = " + ")
        }else{
          interval.func <- paste(interval.func, terms[r], sep = "")
        }
      }
      
      #Append to vector
      function.strings <- append(function.strings, interval.func)
    }
    
    #Create dataframe for the lower limit, upper limit, and the interval function
    qsi.data <- matrix(data = NA, nrow = n, ncol = 3, dimnames = NULL)
    colnames(qsi.data) <- c("Lower Limit", "Upper Limit", "Interval Function")
    qsi.data <- as.data.frame(qsi.data)
    
    for(s in 1:n){
      lower.limit <- x[s]
      upper.limit <- x[s + 1]
      interval.func <- function.strings[s]
      
      qsi.data[s, ] <- c(lower.limit, upper.limit, interval.func)
    }
    
    #Create a list for the interval functions, parsed
    qsi.functions <- list()
    
    for(t in 1:n){
      interval.func <- eval(parse(text = function.strings[t]))
      qsi.functions <- append(qsi.functions, interval.func)
    }
  }else{
    qsi.data <- NA
    qsi.functions <- NA
  }
  
  #Function return
  output <- list(dataframe = qsi.data, functionlist = qsi.functions, data = data)
  return(output)
}


#Gauss-Jordan Elimination
GaussJordanMethod <- function(variables, augcoeffmatrix){
  input <- augcoeffmatrix
  n <- length(variables)
  
  #boolean checker that checks if the system has a unique solution
  solvable <- TRUE
  
  for(i in 1:n){
    
    if(i != n){
      
      #Pivoting Mechanism
      
      #get the vector of values in the ith column
      pivot.col <- input[,i]
      
      #find the maximum absolute value in the pivot column
      maximum <- max(abs(pivot.col)[i:n])
      
      #initialize the vector to store the row number containing the maximum absolute value
      max.index <- i
      
      #loop to find the row containing the maximum absolute value
      for(j in i:n){
        if(abs(pivot.col[j]) == maximum){
          max.index <- j
          break
        }
      }
      
      #Conditional statement to check if the pivot element is zero
      #Implies that the system does not have a unique solution
      if(input[max.index, i] == 0){
        solution <- NA
        solvable <- FALSE
        break
      }
      
      #Swapping Mechanism
      
      #Partial Pivoting
      temp <- input[i,]
      input[i,] <- input[max.index,]
      input[max.index, ] <- temp
      
      
    }
    
    #Store the values of the pivot element and pivot row
    pivot.element <- input[i,i]
    pivot.row <- input[i,]
    
    #Normalize the pivot row
    input[i,] <- pivot.row / pivot.element
    
    for(k in 1:n){
      if(i == k){
        next
      }else{
        #Store the normalized row
        pivot.row <- input[i,]
        
        #Store the element we want to be 0
        target <- input[k,i]
        
        #Create a temporary vector for the target element x normalized row
        temp.vector <- target * pivot.row
        
        #Resulting vector is the current row minus the temporary vector
        result.vector <- input[k, ] - temp.vector
        
        #Update the kth row of the input matrix to the resulting vector
        input[k, ] <- result.vector
      }
    }
  }
  
  #Round off the matrix elements to four decimal places
  input <- round(input, 6)
  
  #Check if the matrix is solvable
  if(solvable){
    
    #Create a solution vector, which is the (n+1)th column of the transformed augmented coefficient matrix
    solution <- input[,(n+1)]
  }
  
  #Function return
  output <- list(variables = variables, augcoeffmatrix = input,  solution = solution)
  return(output)
}