Plane.Price <- read.csv("~/Airplane Price Predictor/Plane Price.csv", header=FALSE)
View(Plane.Price)

head(Plane.Price)

# Set the first row as column names
colnames(Plane.Price) <- as.character(Plane.Price[1, ])

# Remove the first row
Plane.Price <- Plane.Price[-1, ]

View(Plane.Price)

# Print the first few rows to verify
head(Plane.Price)

# Remove rows with NA values
Plane.Price <- na.omit(Plane.Price)

str(Plane.Price)

# Convert specific columns to numeric
numeric_columns <- c("HP or lbs thr ea engine", "Max speed Knots", "Rcmnd cruise Knots", 
                     "Stall Knots dirty", "Fuel gal/lbs", "All eng rate of climb",
                     "Eng out rate of climb", "Takeoff over 50ft", "Landing over 50ft", 
                     "Empty weight lbs", "Range N.M.", "Price")

# Check unique values in numeric columns
for (col in numeric_columns) {
  print(unique(Plane.Price[[col]]))
}



# Load necessary library
library(stringr)

# Remove spaces and convert the first letter of each word in Engine.Type to uppercase
Plane.Price$`Engine Type` <- sapply(strsplit(Plane.Price$`Engine Type`, " "), function(x) paste(toupper(substring(x, 1, 1)), substring(x, 2), collapse=""))

# Remove spaces between words
Plane.Price$`Engine Type` <- gsub("\\s", "", Plane.Price$`Engine Type`)

# Check the unique values in the "Engine Type" column
unique(Plane.Price$`Engine Type`)

str(Plane.Price)

# Load the dplyr package
library(dplyr)

# Convert selected columns to numeric
Plane.Price <- Plane.Price %>%
  mutate(`Max speed Knots` = as.numeric(gsub(",", "", `Max speed Knots`)),
         `Rcmnd cruise Knots` = as.numeric(gsub(",", "", `Rcmnd cruise Knots`)),
         `Stall Knots dirty` = as.numeric(gsub(",", "", `Stall Knots dirty`)),
         `Fuel gal/lbs` = as.numeric(gsub(",", "", `Fuel gal/lbs`)),
         `All eng rate of climb` = as.numeric(gsub(",", "", `All eng rate of climb`)),
         `Eng out rate of climb` = as.numeric(gsub(",", "", `Eng out rate of climb`)),
         `Takeoff over 50ft` = as.numeric(gsub(",", "", `Takeoff over 50ft`)),
         `Landing over 50ft` = as.numeric(gsub(",", "", `Landing over 50ft`)),
         `Empty weight lbs` = as.numeric(gsub(",", "", `Empty weight lbs`)),
         `Length ft/in` = as.numeric(gsub("/", ".", gsub(",", "", `Length ft/in`))),
         `Wing span ft/in` = as.numeric(gsub("/", ".", gsub(",", "", `Wing span ft/in`))),
         `Range N.M.` = as.numeric(gsub(",", "", `Range N.M.`)),
         `HP or lbs thr ea engine` = as.numeric(gsub(",", "", `HP or lbs thr ea engine`)),
         Price = as.numeric(Price))

str(Plane.Price)

# Load the ggplot2 package
library(ggplot2)

# Histogram of Price
ggplot(Plane.Price, aes(x = Price)) +
  geom_histogram(binwidth = 50000, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Price",
       x = "Price",
       y = "Frequency")

# Box plot of Max speed Knots
ggplot(Plane.Price, aes(y = `Max speed Knots`)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Max speed Knots",
       x = "",
       y = "Max speed Knots")

# Bar plot of Engine Type
ggplot(Plane.Price, aes(x = `Engine Type`)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Engine Type",
       x = "Engine Type",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Scatter plot of Max speed Knots vs Price
ggplot(Plane.Price, aes(x = `Max speed Knots`, y = Price)) +
  geom_point(color = "skyblue") +
  labs(title = "Max speed Knots vs Price",
       x = "Max speed Knots",
       y = "Price")

# Scatter plot of Rcmnd cruise Knots vs Price
ggplot(Plane.Price, aes(x = `Rcmnd cruise Knots`, y = Price)) +
  geom_point(color = "skyblue") +
  labs(title = "Rcmnd cruise Knots vs Price",
       x = "Rcmnd cruise Knots",
       y = "Price")

# Box plot of Price by Engine Type
ggplot(Plane.Price, aes(x = `Engine Type`, y = Price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Price Distribution by Engine Type",
       x = "Engine Type",
       y = "Price")

