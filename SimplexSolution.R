# install.packages("dplyr")
library(dplyr)

DeriveSolution <- function(simplexsolution, foodchoices){
  
  #check if the solution vector is feasible 
  #infeasible if there are negative elements in the vector
  infeasible <- any(simplexsolution < 0)
  
  if(!infeasible){
    
    #subset the food, price/serving, serving size of food items selected
    selectedFoodDetails <- subset(food, Formal_Name %in% foodchoices)
    selectedFoodDetails <- selectedFoodDetails[, colnames(selectedFoodDetails) %in% c("Formal_Name", "Price/Serving", "Serving Size")]
    colnames(selectedFoodDetails)[1] <- "Food"
    
    #get the number of slack variables
    noOfFoods <- length(foodchoices)
    constraints.nutri <- 11 * 2
    constraints.serve <- noOfFoods
    
    constraints <- constraints.nutri + constraints.serve
    # print(noOfFoods)
    # print(constraints)
    
    #subset the solution vector to only get the servings number of each food selected
    rownames(simplexsolution) <- NULL
    
    food.servings <- simplexsolution[-c(1:constraints)]
    food.servings <- t(food.servings)
    
    temp <- matrix(data = NA, nrow = (noOfFoods + 1), ncol = 2)
    temp <- as.data.frame(temp)
    
    # print(ncol(simplexsolution))
    # print(nrow(temp))
    # print(length(rownames(food.servings)))
    # print(rownames(food.servings))
    # 
    temp[, 1] <- rownames(food.servings)
    temp[, 2] <- round(food.servings, 2)
    
    food.servings <- temp
    colnames(food.servings) <- c("Food", "Servings")
    
    optimal.diet.cost <- food.servings[nrow(food.servings), 2]
    
    optimal.diet <- left_join(selectedFoodDetails, food.servings, by = "Food")
    
    cost <- optimal.diet[,2] * optimal.diet[,4]
    cost <- round(cost, 2)
  
    optimal.diet <- cbind(optimal.diet, cost)
    colnames(optimal.diet) <- c("Food", "Price/Serving", "Serving Size", "Servings", "Total Cost")
    optimal.diet <- optimal.diet[optimal.diet$Servings > 0,]
    
    optimal.diet.cost <- c("Cost of Optimal Diet per day", NA, NA, NA, optimal.diet.cost)
    optimal.diet <- rbind(optimal.diet, optimal.diet.cost)
    
    output <- list(optimal.diet = optimal.diet)
  }else{
    output <- "It is not possible to meet the nutritional constraints with the foods that you have selected."
  }
  
  return(output)
}

