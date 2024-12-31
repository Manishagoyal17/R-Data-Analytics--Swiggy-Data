# Load necessary libraries
library(ggplot2)

# Load the dataset 
swiggy_data <- read.csv("C:/Users/MANISHA GOYAL/Downloads/swiggy dataset.csv)

# Clean and prepare the dataset
swiggy_data <- data %>%
  mutate(Category = as.character(Category),
         city = as.factor(city),
         Veg = as.logical(Veg))

# Summary of the dataset
summary(data)