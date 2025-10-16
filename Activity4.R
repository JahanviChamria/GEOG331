#use built in iris dataset
#take a look at it 
head(iris)

#load in some tidyverse packages
#install.packages("tidyverse")
library(tidyverse)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# hint: consider using a list, and also new vectors for regression variables

versicolor <- iris[iris$Species == "versicolor", ]
regression_results <- list()

#for full model summaries and a short summary table
regression_results <- list()          
short_table <- data.frame(             
  Relationship = character(0),
  Intercept = numeric(0),
  Slope = numeric(0),
  R_squared = numeric(0),
  stringsAsFactors = FALSE
)

#x and y column names for each regression (in the same order)
x_names <- c("Sepal.Width", "Petal.Width", "Petal.Length")
y_names <- c("Sepal.Length", "Petal.Length", "Sepal.Length")
rel_names <- c("Sepal.Length vs Sepal.Width",
               "Petal.Length vs Petal.Width",
               "Sepal.Length vs Petal.Length")

#for loop that runs the three regressions
for (i in 1:3) {
  xvec <- versicolor[, x_names[i]] 
  yvec <- versicolor[, y_names[i]]
  
  model <- lm(yvec ~ xvec)
  
  #store the full summary in the list
  regression_results[[ rel_names[i] ]] <- summary(model)
  
  #intercept, slope, and R-squared
  coeffs <- coef(summary(model))    
  intercept <- coeffs[1, "Estimate"]
  slope <- coeffs[2, "Estimate"]
  r2 <- summary(model)$r.squared
  
  #add one row to the table
  short_table[nrow(short_table) + 1, ] <- list(rel_names[i], intercept, slope, r2)
}

#view the short summary table
print(short_table)

#full output for the models:
print(regression_results[[1]])
print(regression_results[[2]])
print(regression_results[[3]])

#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
                     Height.cm = c(60,100,11.8))

#join it to the iris data using tidyverse (dplyr)
iris_with_height <- left_join(iris, height, by = "Species")

#new data
head(iris_with_height)


#####################################
##### Part 3: plots in ggplot2  #####
#####################################

#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point()


#3b. make a scatter plot with ggplot and get rid of  busy grid lines (all)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point() +
  theme(panel.grid = element_blank())

#3c. make a scatter plot with ggplot, remove grid lines, add a title and axis labels, 
#    show species by color, and make the point size proportional to petal length
ggplot(iris, aes(x = Sepal.Length, 
                 y = Sepal.Width, 
                 color = Species, 
                 size = Petal.Length)) +
  geom_point() +
  theme(panel.grid = element_blank()) +
  labs(title = "Iris Flower Measurements",
       x = "Sepal Length (cm)",
       y = "Sepal Width (cm)")

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################		

#answered in Google Doc

