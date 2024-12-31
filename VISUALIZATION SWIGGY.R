# Insight 1: Distribution of Ratings
# Visualization: Histogram - Distribution of Ratings
 ggplot(swiggy_data, aes(x = Rating)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "white", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Ratings", x = "Rating", y = "Count") +
  theme_minimal()

# Insight 2: Average Cost for Two by City
# Visualization: Bar Chart - Average Cost for Two by City
avg_cost <- swiggy_data %>% group_by(city) %>% summarize(Average_Cost = mean(Cost.for.two, na.rm = TRUE))
 ggplot(avg_cost, aes(x = reorder(city, -Average_Cost), y = Average_Cost)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(Average_Cost, 1)), vjust = -0.5) +
  labs(title = "Average Cost for Two by City", x = "City", y = "Average Cost") +
  theme_minimal() +
  coord_flip()

# Insight 3: Proportion of Veg and Non-Veg Restaurants
# Visualization: Pie Chart - Proportion of Veg and Non-Veg Restaurants
veg_proportion <- swiggy_data %>% group_by(Veg) %>% summarize(Count = n())
 ggplot(veg_proportion, aes(x = "", y = Count, fill = Veg)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = scales::percent(Count / sum(Count), accuracy = 0.1)), position = position_stack(vjust = 0.5)) +
  labs(title = "Proportion of Veg and Non-Veg Restaurants", x = "", y = "") +
  theme_void()

# Insight 4: Top 10 Localities with Most Restaurants
# Visualization: Bar Chart - Top 10 Localities with Most Restaurants
top_localities <- swiggy_data %>%
  group_by(Locality) %>%
  summarize(Restaurant_Count = n()) %>%
  top_n(10, Restaurant_Count)
 ggplot(top_localities, aes(x = reorder(Locality, -Restaurant_Count), y = Restaurant_Count)) +
  geom_bar(stat = "identity", fill = "purple") +
  geom_text(aes(label = Restaurant_Count), vjust = -0.5) +
  labs(title = "Top 10 Localities with Most Restaurants", x = "Locality", y = "Number of Restaurants") +
  theme_minimal() +
  coord_flip()

# Insight 5: Distribution of Cost for Two
# Visualization: Histogram - Distribution of Cost for Two
 ggplot(swiggy_data, aes(x = Cost.for.two)) +
  geom_histogram(binwidth = 50, fill = "orange", color = "white", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Cost for Two", x = "Cost for Two", y = "Count") +
  theme_minimal()

# Insight 6: Ratings vs. Cost for Two
# Visualization: Scatter Plot - Ratings vs. Cost for Two
 ggplot(swiggy_data, aes(x = Rating, y = Cost.for.two)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Ratings vs. Cost for Two", x = "Rating", y = "Cost for Two") +
  theme_minimal()

# Insight 7: Long Distance Delivery Proportion
# Visualization: Bar Chart - Proportion of Long Distance Delivery
long_delivery <- swiggy_data %>% group_by(Long.Distance.Delivery) %>% summarize(Count = n())
 ggplot(long_delivery, aes(x = Long.Distance.Delivery, y = Count)) +
  geom_bar(stat = "identity", fill = "pink") +
  geom_text(aes(label = Count), vjust = -0.5) +
  labs(title = "Proportion of Long Distance Delivery", x = "Long Distance Delivery", y = "Count") +
  theme_minimal()

# Insight 8: Cost for Two by Area
# Visualization: Bar Chart - Top 10 Areas by Average Cost for Two
area_cost <- swiggy_data %>% group_by(Area) %>% summarize(Average_Cost = mean(Cost.for.two, na.rm = TRUE)) %>% top_n(10, Average_Cost)
 ggplot(area_cost, aes(x = reorder(Area, -Average_Cost), y = Average_Cost)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = round(Average_Cost, 1)), vjust = -0.5) +
  labs(title = "Top 10 Areas by Average Cost for Two", x = "Area", y = "Average Cost") +
  theme_minimal() +
  coord_flip()

# Insight 9: Distribution of Cities
# Visualization: Bar Chart - Distribution of Cities
 ggplot(swiggy_data, aes(x = city)) +
  geom_bar(fill = "lightgreen", alpha = 0.7) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Cities", x = "City", y = "Count") +
  theme_minimal()

 # Insight 10: Average Rating by City
 # Visualization: Bar Chart - Average Rating by City
 avg_rating <- swiggy_data %>% group_by(city) %>% summarize(Average_Rating = mean(Rating))
  ggplot(avg_rating, aes(x = reorder(city, -Average_Rating), y = Average_Rating)) +
   geom_bar(stat = "identity", fill = "coral") +
   geom_text(aes(label = round(Average_Rating, 1)), vjust = -0.5) +
   labs(title = "Average Rating by City", x = "City", y = "Average Rating") +
   theme_minimal() +
   coord_flip()
 
 # Insight 11: Cost for Two vs. Rating (Density)
 # Visualization: Density Plot - Cost for Two vs. Rating
  ggplot(swiggy_data, aes(x = Cost.for.two, y = Rating)) +
   geom_bin2d(bins = 30) +
   scale_fill_gradient(low = "yellow", high = "red") +
   labs(title = "Cost for Two vs. Rating (Density)", x = "Cost for Two", y = "Rating") +
   theme_minimal()
 
 # Insight 12: Count of Restaurants by Area
 # Visualization: Bar Chart - Count of Restaurants by Area
 area_restaurant_count <- swiggy_data %>% group_by(Area) %>% summarize(Count = n()) %>% top_n(10, Count)
  ggplot(area_restaurant_count, aes(x = reorder(Area, -Count), y = Count)) +
   geom_bar(stat = "identity", fill = "purple") +
   geom_text(aes(label = Count), vjust = -0.5) +
   labs(title = "Top 10 Areas by Restaurant Count", x = "Area", y = "Count") +
   theme_minimal() +
   coord_flip()
  
  # Insight 13: Cost for Two Distribution by Veg/Non-Veg
  # Visualization: Box Plot - Cost for Two by Veg/Non-Veg
   ggplot(swiggy_data, aes(x = Veg, y = Cost.for.two, fill = Veg)) +
    geom_boxplot(alpha = 0.7) +
    labs(title = "Cost for Two Distribution by Veg/Non-Veg", x = "Veg/Non-Veg", y = "Cost for Two") +
    theme_minimal()
   