############################################################################################
# Notes: 12/11 Updates
# 
# (1) Updated initializeMatrix algorithm based on Mr. Doria's suggestions
# (2) Modified Calcium_U's upper bounds (-1600 instead of 1600) in initializeMatrix
# 
#       12/14 Modifications
# (1) Added conditional statement in initializeMatrix to check whether the current data
#     is a vector (one food item chosen)
#
############################################################################################

library(readxl)

#Vector of the food item choices
food_items <- c("Broccoli", "Carrots", "Celery", "Corn", "Lettuce", "Sweet Peppers", "Potatoes", "Tomato",
                "Apple", "Banana", "Grapes", "Kiwifruit", "Oranges",
                "Bagels", "Wheat Bread", "White Bread", "Oatmeal Cookies", "Apple Pie", "Chocolate Chip Cookies",
                "Couscous", "White Rice", "Macaroni", 
                "Cap N' Crunch", "Cheerios", "Corn Flakes", "Raisin Bran", "Rice Krispies", "Oatmeal", "Malt-O-Meal", "Special K",
                "Whole Milk", "Low-Fat Milk", "Skim Milk", "Poached Eggs", "Scrambled Eggs", "Butter", "Cheddar Cheese", "Peanut Butter",
                "Roasted Chicken", "Turkey Bologna", "Beef Frankfurter", "Sliced Ham", "Pork Kielbasa", "Pork", "Sardines in Oil", "White Tuna in Water", "Tofu",
                "Spaghetti w/ Sauce", "Pepperoni Pizza", "Hamburger", "Hotdog", "Taco",
                "Vegetable Beef Soup", "Chicken Noodle Soup", "Split Pea & Ham Soup", "New England Clam Chowder", "Tomato Soup", "New England Clam Chowder with Milk", "Cream of Mushroom Soup with Milk", "Bean and Bacon Soup",
                "Popcorn", "Potato Chips", "Pretzels", "Tortilla Chip")

food <- read_excel("food.xlsx")

#Function to subset rows in the food dataset
selectedFoods <- function(selected){
  
  selectedData <- subset(food, Formal_Name %in% selected)
  selectedData <- selectedData[, !colnames(selectedData) %in% c("Foods", "Code")]
  colnames(selectedData)[1] <- "Food"
  return(selectedData)
}

#Function to create initial matrix, objective is still minimization
initializeMatrix <- function(dataset){
  
  df <- dataset
  
  #get the non-matrix details of food
  foodInfo <- df[, colnames(df) %in% c("Food", "Serving Size")]
  foods <- as.vector(df[, 1])
  foods <- unlist(foods)
  
  #setup initial matrix
  init_mat <- df[, !colnames(df) %in% c("Food", "Serving Size")]
  
  #create new columns for the upper bounds of nutrients
  upperBoundNutri <- init_mat[, -1]
  upperBoundNutri <- -1 * upperBoundNutri
  
  #insert the columns after
  temp <- init_mat[, 1]
  
  for(i in 1:ncol(upperBoundNutri)){
    temp <- cbind(temp, init_mat[, (i + 1)])
    temp <- cbind(temp, upperBoundNutri[, i])
  }
  
  init_mat <- temp
  
  #rename columns
  varnames <- c("Price/Serving")
  
  nutrients <- colnames(df)[4:ncol(df)]
  nutri_colnames <- c()
  
  for(j in 1:length(nutrients)){
    lower.name <- paste(nutrients[j], "_L", sep = "")
    upper.name <- paste(nutrients[j], "_U", sep = "")
    nutri_colnames <- append(nutri_colnames, c(lower.name, upper.name))
  }
  
  varnames <- append(varnames, nutri_colnames)
  
  colnames(init_mat) <- varnames
  
  #transpose matrix
  init_mat <- t(init_mat)
  
  #rename columns
  colnames(init_mat) <- c(foods)
  
  #add servings constraints
  servings_constraints <- diag(length(foods))
  servings_constraints <- -1 * servings_constraints
  init_mat <- rbind(init_mat, servings_constraints)
  
  servings_varnames <- paste0(foods, "_ServeConst")
  rownames(init_mat) <- c(varnames, servings_varnames)
  
  #put the price/servings to the last row
  init_mat <- rbind(init_mat, init_mat[1, ])
  init_mat <- init_mat[-1, ]
  
  if(!is.vector(init_mat)){
    rownames(init_mat)[nrow(init_mat)] <- "Price/Serving"
  }
  
  
  #create the constraints
  max_servings <- rep(-10, times = length(foods))
  nutri_const <- c(2000, -2250, 
                   0, -300, 
                   0, -65, 
                   0, -2400,
                   0, -300, 
                   25, -100, 
                   50, -100, 
                   5000, -50000, 
                   50, -20000, 
                   800, -1600, 
                   10, -30)
  
  constraints <- c(nutri_const, max_servings, 1)
  
  #add the constraints to the matrix
  init_mat <- cbind(init_mat, constraints)
  
  #update column names
  colnames(init_mat)[ncol(init_mat)] <- "const"
  
  #return initial matrix and number of selected food items
  output <- list(init_mat = init_mat, no_of_foods = length(foods))
  return(output)
}


#Fucntion to create the matrix for the dual of the problem
dualization <- function(initialMatrix, noOfFoods){
  
  df <- initialMatrix
  count <- noOfFoods
  
  #transpose the original matrix
  dual_matrix <- t(df)
  
  #modify the objective function of the dual of the problem
  newObjective_func <- dual_matrix[nrow(dual_matrix), ]
  newObjective_func <- - 1 * newObjective_func
  newObjective_func[ncol(dual_matrix)] <- 0
  
  dual_matrix[nrow(dual_matrix), ] <- newObjective_func
  
  #create matrix for the variables
  variables <- diag((count + 1))
  
  #modify dual matrix
  dual_matrix <- cbind(dual_matrix[,1:(ncol(dual_matrix) - 1)], variables, dual_matrix[,ncol(dual_matrix)])
  
  #modify column names of matrix
  slacks <- paste0("S", 1:(nrow(df) - 1))
  vars <- rownames(dual_matrix)[1:(nrow(dual_matrix) - 1)]
  columns <- c(slacks, vars, "Z", "const")
  
  colnames(dual_matrix) <- columns
  
  return(dual_matrix)
    
}






