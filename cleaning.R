# 1. Loads raw data.
# 2. Cleans basic formatting (Phone, Text).
# 3. Filters out invalid logic (Negative money, impossible ages).
# 4. DROPS all rows with missing data (No Imputation).
# 5. Organizes columns to match 'complete_data.csv' exactly.

library(dplyr)
library(stringr)

options(scipen = 999) # No scientific notation

# Load Data
if(file.exists("retail_data.csv")) {
  data <- read.csv("retail_data.csv", stringsAsFactors = FALSE)
} else {
  stop("Error: 'retail_data.csv' not found.")
}

zip_lengths <- nchar(str_trim(as.character(data$Zipcode)))
cat("Zipcode Length Distribution:\n")
print(table(zip_lengths))

empty_country <- sum(data$Country == "" | is.na(data$Country))
empty_gender  <- sum(data$Gender == "" | is.na(data$Gender))

cat(paste("Rows with missing/empty Country:", empty_country, "\n"))
cat(paste("Rows with missing/empty Gender: ", empty_gender, "\n"))

raw_ages <- suppressWarnings(as.numeric(data$Age))

cat("Minimum Age found:", min(raw_ages, na.rm = TRUE), "\n")
cat("Maximum Age found:", max(raw_ages, na.rm = TRUE), "\n")

phones_digits <- gsub("[^0-9]", "", as.character(data$Phone))
phone_lens <- nchar(phones_digits)

cat("Phone Number Length Distribution:\n")
print(table(phone_lens))

# Cleaning Pipeline
clean_data <- data %>%
  mutate(
    # Clean Phone: Digits only
    Phone_clean = gsub("[^0-9]", "", as.character(Phone)),
    
    # Text Formatting: Title Case & Trim
    Country = str_to_title(str_squish(Country)),
    City = str_to_title(str_squish(City)),
    State = str_to_title(str_squish(State)),
    Income = str_trim(Income),
    
    # Numeric Safety (Ensure numbers are numbers)
    Age = as.numeric(Age),
    Amount = as.numeric(Amount),
    Total_Purchases = as.numeric(Total_Purchases),
    Total_Amount = as.numeric(Total_Amount),
    
    # Convert empty strings "" to NA so na.omit() catches them
    across(where(is.character), ~na_if(., ""))
  ) %>%
  
  filter(
    # Transaction & Phone Validation
    !is.na(Transaction_ID) & Transaction_ID != "",
    !grepl("[a-zA-Z]", Transaction_ID),
    !is.na(Phone_clean) & Phone_clean != "",
    nchar(Phone_clean) >= 7 & nchar(Phone_clean) <= 12,
    
    # Demographics Validation
    !is.na(City) & City != "",
    !is.na(State) & State != "",
    !is.na(Zipcode) & Zipcode != "",
    nchar(as.character(Zipcode)) >= 4,
    !is.na(Country) & Country != "",
    !is.na(Gender) & Gender != "",
    !is.na(Age) & Age >= 18 & Age <= 100,
    !is.na(Income) & Income != "",
    
    # Financial Validation
    !is.na(Total_Purchases) & Total_Purchases > 0,
    !is.na(Amount) & Amount > 0,
    !is.na(Total_Amount) & Total_Amount > 0,
    
    # Date Validation (Ensuring all time components exist) 
    !is.na(Year), !is.na(Month), !is.na(Date), !is.na(Time),
    
    # Categorical Completeness 
    !is.na(Customer_Segment), !is.na(Product_Category),
    !is.na(Product_Brand), !is.na(Product_Type),
    !is.na(Feedback), !is.na(Shipping_Method),
    !is.na(Payment_Method), !is.na(Order_Status),
    !is.na(Ratings), !is.na(products)
  ) %>%
  
  # Deduplicate Transactions
  distinct(Transaction_ID, .keep_all = TRUE) %>%
  
  # Sort
  arrange(Transaction_ID)

# Verification Checks
cat("\n DATA QUALITY CHECKS \n")

# Check ID Consistency
inconsistent_customers <- clean_data %>%
  group_by(Customer_ID) %>%
  summarise(Unique_Names = n_distinct(Name)) %>%
  filter(Unique_Names > 1)

if(nrow(inconsistent_customers) == 0) {
  cat("[PASS] Customer IDs are consistent.\n")
} else {
  cat(paste("[WARN]", nrow(inconsistent_customers), "IDs have multiple names.\n"))
}

# Math Logic
math_errors <- clean_data %>%
  mutate(Calc = Amount * Total_Purchases, Diff = abs(Total_Amount - Calc)) %>%
  filter(Diff > 1.00)

cat(paste("Rows with Math Discrepancies:", nrow(math_errors), "\n"))

cat("\n SAVING DATA \n")

final_output <- clean_data %>%
  select(-Transaction_ID, -Customer_ID, -Name, -Email, -Phone, -Phone_clean, -Address, -Zipcode)%>%
  mutate(across(where(is.character), ~na_if(., ""))) %>%
  na.omit()

cat("Original Rows:", nrow(data), "\n")
cat("Cleaned Rows: ", nrow(final_output), "\n")

write.csv(final_output, "complete_data_recreated.csv", row.names = FALSE)
cat("File saved successfully: complete_data_recreated.csv\n")