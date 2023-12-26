## Housing Characteristics 

data <- read.csv("C:/efrayer/Python_files/mera_insurance_data/MERA_Insurance_Home_Owner_Data.csv")


# Creating a contingency table
contingency_table <- table(data$Claims_History, data$Local_Weather_Conditions)

# Performing the Chi-square test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

# Function to calculate Cramer's V
cramer_v <- function(chisq, n, r, k) {
  phi_sq <- chisq / n
  min_dim <- min(r - 1, k - 1)
  return(sqrt(phi_sq / min_dim))
}

# Applying the function
n <- nrow(data)
r <- nrow(contingency_table)
k <- ncol(contingency_table)
cramer_v_value <- cramer_v(chi_square_test$statistic, n, r, k)
print(cramer_v_value)

install.packages("glue")


library(ggplot2)

ggplot(data, aes(x = Claims_History, fill = Local_Weather_Conditions)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Normal" = "#336699", "Severe" = "#003366")) +
  labs(title = "Stacked Bar Chart of Policies by Claims History and Local Weather Conditions",
       x = "Claims History", y = "Proportion") +
  coord_flip()



library(ggplot2)

# Assuming data is already loaded as 'data'
# Creating a stacked bar chart
ggplot(data, aes(x = Claims_History, fill = Local_Weather_Conditions)) +
  geom_bar(position = "stack") +  # Stacking the bar
  scale_fill_manual(values = c("#003366", "#336699")) +  # Setting custom colors
  labs(title = "Number of Policies by Claims History and Local Weather Conditions",
       x = "Claims History",
       y = "Number of Policies") +
  theme_minimal() +  # Minimal theme for a cleaner look
  theme(axis.text.x = element_text(angle = 0, hjust = 1))  # Horizontal x-axis labels

# Note: This will create a stacked bar chart with the number of policies for each combination of Claims History and Local Weather Conditions.

