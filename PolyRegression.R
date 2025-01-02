PolynomialRegression <- function(order, datalist){
  
  #check if function inputs are valid
  if(ValidInput(order, datalist)){
    degree <- order
    x <- datalist[[1]]
    y <- datalist[[2]]
    
    #initialize the augmented coefficient matrix 
    augcoeffmatrix <- matrix(data = NA, nrow = (degree + 1), ncol = (degree + 2))
    
    #compute for the summations of x
    x.powers <- c()
    
    for(i in 0:(2*degree)){
      summation <- sum(x^i)
      x.powers <- append(x.powers, summation)
    }
    
    #compute for the summations of x*y
    xy.powers <- c()
    for(j in 0:degree){
      summation <- sum((x^j)*y)
      xy.powers <- append(xy.powers, summation)
    }
    
    #set the values for the augmented coefficient matrix
    
    #add the right hand side of the augcoeffmatrix
    rhs <- ncol(augcoeffmatrix)
    augcoeffmatrix[ ,rhs] <- xy.powers
    
    #create a temporary vector for the "coefficient" matrix
    temp <- matrix(data = NA, nrow = (degree + 1), ncol = (degree + 1))
    start <- 1
    
    #loop to iteratively set the value of every column of augcoeffmatrix based on 
    #a range of values from the x.powers vector
    
    for(k in 1:ncol(temp)){
      temp[ ,k] <- x.powers[start:(start + degree)]
      start <- start + 1
    }
    
    #add the created matrix to the remaining columns of the augcoeffmatrix
    augcoeffmatrix[, 1:(rhs - 1)] <- temp
    
    #create variable names for the coefficients to be estimated
    variables = c()
    
    #for the constant term
    variables[1] <- ""
    
    #for degrees 1 to degree
    for (l in 1:(degree)){
      varname <- paste("x^", (l), sep = "")
      variables <- append(variables, varname)
    }
    
    #create the coefficient vector using the output from the GaussJordanMethod
    coefficients <- GaussJordanMethod_PR(variables, augcoeffmatrix)$solution
    
    #create the polynomial string
    
    #initialize the variable
    polynomial_string <- "function(x) "
    
    #combine the coefficient-variables
    terms <- c()
    
    for(m in 1:length(coefficients)){
      if(m != 1){
        term <- paste(coefficients[m], variables[m], sep = " * ")
      }else{
        term <- paste(coefficients[m], variables[m], sep = "")
      }
      terms <- append(terms, term)
    }
    
    #combine all the terms
    combined_terms <- ""
    
    for(n in 1:length(terms)){
      if(n != 1){
        combined_terms <- paste(combined_terms, terms[n], sep = " + ")
      }else{
        combined_terms <- paste(combined_terms, terms[n], sep = "")
      }
    }
    
    polynomial_string <- paste(polynomial_string, combined_terms, sep = "")
    
    #create the polynomial function
    polynomial_function <- eval(parse(text = polynomial_string))
    #return function outputs
    output <- list(augcoeffmatrix = augcoeffmatrix, coefficients = coefficients, polynomial_string = polynomial_string, polynomial_function = polynomial_function, original_data = datalist)
    return(output)
  }
}


#function that checks if the function inputs for PolynomialRegression are valid
ValidInput <- function(order, datalist){
  if(order >= 1){
    if(length(datalist) == 2){
      x <- datalist[[1]]
      y <- datalist[[2]]
      if(length(x) == length(y)){
        if(order <= (length(x)-1)){
          return(TRUE)
        }else{
          print("Function degree exceeds allowable degree for the size of data.")
          return(FALSE)
        }
      }else{
        print("Input vectors are of different length.")
        return(FALSE)
      }
    }else{
      print("Invalid data input. Please enter a list of two vectors containing the values for the independent and dependent variables.")
      return(FALSE)
    }
  }else{
    print("Invalid function order.")
    return(FALSE)
  }
}


#Gauss-Jordan Elimination
GaussJordanMethod_PR <- function(variables, augcoeffmatrix){
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
  
  #Check if the matrix is solvable
  if(solvable){
    
    #Create a solution vector, which is the (n+1)th column of the transformed augmented coefficient matrix
    solution <- input[,(n+1)]
  }
  
  #Function return
  output <- list(variables = variables, augcoeffmatrix = input,  solution = solution)
  return(output)
}