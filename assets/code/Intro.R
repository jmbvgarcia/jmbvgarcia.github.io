# R Basics: A Helper Script for Beginners

# ---- 1. Basic Operations ----
# Arithmetic operations
x <- 5
y <- 2
sum_xy <- x + y  # Addition
prod_xy <- x * y  # Multiplication
exp_xy <- x^y     # Exponentiation
sqrt_x <- sqrt(x) # Square root

# Logical operations
is_greater <- x > y  # TRUE
is_equal <- x == y    # FALSE

# ---- 2. Vectors and Sequences ----
# Creating vectors
vec <- c(1, 2, 3, 4, 5)  # Combine elements
seq_vec <- seq(1, 10, by = 2)  # Sequence from 1 to 10 by steps of 2
rep_vec <- rep(3, times = 5)  # Repeat 3 five times

# Indexing vectors
third_element <- vec[3]  # Access third element
subset_vec <- vec[vec > 2]  # Elements greater than 2

# ---- 3. Data Frames ----
# Creating a data frame
my_data <- data.frame(Name = c("Alice", "Bob", "Charlie"),
                      Age = c(25, 30, 35),
                      Score = c(85, 90, 88))

# Accessing data
head(my_data)  # First few rows
tail(my_data)  # Last few rows
my_data$Age    # Accessing a column

# ---- 4. Functions ----
# Creating a simple function
square_num <- function(x) {
  return(x^2)
}
square_num(4)  # Output: 16

# ---- 5. Control Structures ----
# If-else statement
x <- 10
if (x > 5) {
  message("x is greater than 5")
} else {
  message("x is 5 or less")
}

# For loop
for (i in 1:5) {
  print(paste("Iteration", i))
}




# ---- 6. Introduction to Tidyverse ----
# Install (if not installed) and load tidyverse
install.packages("tidyverse")
library(tidyverse)

# Creating a tibble (a modern version of data frames)
tib <- tibble(Name = c("Alice", "Bob", "Charlie"),
              Age = c(25, 30, 35),
              Score = c(85, 90, 88))

# Data manipulation with dplyr
filtered_data <- tib %>%
  filter(Age > 25) %>%  # Keep rows where Age > 25
  mutate(Score_Adjusted = Score * 1.1)  # Add a new column

# Summarizing data
grouped_summary <- tib %>%
  group_by(Age) %>%
  summarise(Average_Score = mean(Score))

# ---- 7. Data Visualization with ggplot2 ----
# Simple scatter plot
ggplot(tib, aes(x = Age, y = Score)) +
  geom_point() +
  labs(title = "Age vs Score")

# Bar chart
ggplot(tib, aes(x = Name, y = Score)) +
  geom_col(fill = "steelblue") +
  labs(title = "Scores by Name")
