# Load required libraries
library(tidyverse)
library(janitor)

# Read raw data
sales <- read.csv("C:/Users/senpa/Documents/Sales.csv", stringsAsFactors = FALSE)

# Clean column names
sales <- sales %>% clean_names()

# Explore the dataset
str(sales)
dim(sales)
head(sales)
tail(sales)

# Descriptive statistics
summary(sales)

# Clean data
convert_to_mb <- function(x) {
  if (is.na(x) || x == "") return(NA)
  num <- as.numeric(str_extract(x, "\\d+\\.?\\d*"))
  if (is.na(num)) return(NA)
  unit <- str_extract(tolower(x), "gb|mb")
  if (is.na(unit)) return(num * 1024) 
  if (unit == "gb") return(num * 1024)
  if (unit == "mb") return(num)
  return(NA)
}
sales$memory <- sapply(sales$memory, convert_to_mb)
sales$storage <- sapply(sales$storage, convert_to_mb)

# Handle missing values
sales$memory[is.na(sales$memory)] <- median(sales$memory, na.rm = TRUE)
sales$storage[is.na(sales$storage)] <- median(sales$storage, na.rm = TRUE)
sales$rating[is.na(sales$rating)] <- median(sales$rating, na.rm = TRUE)

get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
sales$brands[is.na(sales$brands)] <- get_mode(sales$brands)
sales$colors[is.na(sales$colors)] <- get_mode(sales$colors)
sales$camera[is.na(sales$camera)] <- get_mode(sales$camera)
sales$mobile[is.na(sales$mobile)] <- get_mode(sales$mobile)

# Create uniformity
sales$brands <- str_to_upper(str_trim(sales$brands))
sales$colors <- str_to_title(str_trim(sales$colors))
sales$camera <- str_to_title(str_trim(sales$camera))
sales$mobile <- str_squish(sales$mobile)

# Transform data types
sales$brands <- factor(sales$brands, ordered = FALSE)
sales$colors <- factor(sales$colors, ordered = FALSE)
sales$camera <- factor(sales$camera, ordered = FALSE)
sales$models <- factor(sales$models, ordered = FALSE)
sales$mobile <- factor(sales$mobile, ordered = FALSE)
sales$selling_price <- as.numeric(sales$selling_price)
sales$original_price <- as.numeric(sales$original_price)
#sales$discount <- as.numeric(sales$discount_percentage)

# Min-Max normalization
minmax_norm <- function(x, new_min = 0, new_max = 1) {
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  if (mx == mn) return(rep(0, length(x)))
  ((x - mn) / (mx - mn)) * (new_max - new_min) + new_min
}

sales$MinMax_SellingPrice <- minmax_norm(sales$selling_price, 0, 100)
sales$norm_selling_price  <- minmax_norm(sales$selling_price)
sales$norm_original_price <- minmax_norm(sales$original_price)
sales$norm_memory         <- minmax_norm(sales$memory)
sales$norm_storage        <- minmax_norm(sales$storage)
sales$norm_discount       <- minmax_norm(sales$discount_percentage)
sales$Discount_Scale      <- sales$discount_percentage / 100

# Remove duplicates
sales <- distinct(sales)

# If-Else statement for comparison of Anrdoid and iOS
sales$os <- ifelse(str_detect(str_to_lower(sales$brands), "apple|iphone"), "iOS", "Android")
sales$discount_percentage <- round(((sales$original_price - sales$selling_price) / sales$original_price) * 100, 2)
sales$price_category <- case_when(
  sales$selling_price < 10000 ~ "Budget",
  sales$selling_price >= 10000 & sales$selling_price < 25000 ~ "Mid-Range",
  sales$selling_price >= 25000 ~ "Flagship"
)

# Handling outliers on selling price
# Outlier Before Removal
boxplot(sales$selling_price, main = "Before Removing Outliers")

sales$os <- factor(sales$os, ordered = FALSE)
sales$price_category <- factor(sales$price_category, ordered = FALSE)

# Outlier removal (selling price)
Q1 <- quantile(sales$selling_price, 0.25, na.rm = TRUE)
Q3 <- quantile(sales$selling_price, 0.75, na.rm = TRUE)
IQR_value <- IQR(sales$selling_price, na.rm = TRUE)
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value
sales_clean <- sales %>%
  filter(selling_price >= lower_bound & selling_price <= upper_bound)

# Outlier After Removal
boxplot(sales_clean$selling_price, main = "After Removing Outliers")

# Put the dataset on a new variable
final_dataset <- sales_clean

# Save the dataset
write.csv(final_dataset, "group2_data_preprocess.csv", row.names = FALSE)

summary(sales)
str(sales)
