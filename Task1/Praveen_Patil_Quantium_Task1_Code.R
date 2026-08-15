# ==========================================================
# PROJECT : Quantium Virtual Internship - Task 1
# AUTHOR  : Praveen Patil
# PURPOSE : Retail Strategy and Analytics - Exploratory Data Analysis
# ==========================================================

# ==========================================================
# 1. LOAD REQUIRED LIBRARIES
# ==========================================================

library(readxl)
library(data.table)
library(readr)
library(ggplot2)
library(stringr)

# ==========================================================
# 2. LOAD DATASETS
# ==========================================================

# Load transaction dataset
transactionData <- read_excel("QVI_transaction_data.xlsx")
transactionData <- as.data.table(transactionData)

# Load purchase behaviour dataset
customerData <- fread("QVI_purchase_behaviour.csv")


# ==========================================================
# 3. INITIAL DATA EXPLORATION
# ==========================================================

# Check data type
class(transactionData)

# View structure
str(transactionData)

# View first 10 rows
head(transactionData, 10)

# Summary statistics
summary(transactionData)

# Dataset dimensions
dim(transactionData)

# Column names
names(transactionData)

# ==========================================================
# 4. CONVERT DATE FORMAT
# ==========================================================

# Convert Excel serial dates to Date format
transactionData[, DATE := as.Date(DATE, origin = "1899-12-30")]

# Verify conversion
class(transactionData$DATE)

str(transactionData$DATE)


# ==========================================================
# 5. EXPLORE PRODUCT NAMES
# ==========================================================

# Summary of product names
summary(transactionData$PROD_NAME)

# View first 20 product names
head(transactionData$PROD_NAME, 20)

# Count unique products
unique_products <- length(unique(transactionData$PROD_NAME))
unique_products


# ==========================================================
# 6. TEXT ANALYSIS OF PRODUCT NAMES
# ==========================================================

# Split product names into individual words
productWords <- data.table(
  words = unlist(strsplit(unique(transactionData$PROD_NAME), " "))
)

# Remove words containing numbers
productWords <- productWords[!grepl("[0-9]", words)]

# Keep only alphabetic words
productWords <- productWords[grepl("^[A-Za-z]+$", words)]

# Count frequency of each word
wordFrequency <- productWords[, .N, by = words]

# Sort by frequency
setorder(wordFrequency, -N)

# Display the 30 most frequent words
head(wordFrequency, 30)


# ==========================================================
# 7. REMOVE NON-CHIP PRODUCTS (SALSA)
# ==========================================================

# Identify salsa products
transactionData[, SALSA := grepl("salsa", tolower(PROD_NAME))]

# Count salsa and non-salsa products
salsa_count <- table(transactionData$SALSA)
salsa_count

# Verify number of salsa products
sum(grepl("salsa", tolower(transactionData$PROD_NAME)))

# Remove salsa products
transactionData <- transactionData[SALSA == FALSE]

# Remove temporary column
transactionData[, SALSA := NULL]

# Verify removal
sum(grepl("salsa", tolower(transactionData$PROD_NAME)))

# Result:
# Successfully removed all salsa products from the dataset.


# ==========================================================
# 8. DETECT AND REMOVE OUTLIER CUSTOMER
# ==========================================================

# Summary before removing outlier
summary_stats <- summary(transactionData)
summary_stats

# Investigate unusually large purchases
transactionData[PROD_QTY == 200]

transactionData[
  PROD_QTY == 200,
  .(
    LYLTY_CARD_NBR,
    DATE,
    STORE_NBR,
    TXN_ID,
    PROD_NAME,
    PROD_QTY,
    TOT_SALES
  )
]

# View all transactions for the customer
transactionData[LYLTY_CARD_NBR == 226000]

# Remove commercial customer
transactionData <- transactionData[
  LYLTY_CARD_NBR != 226000
]

# Verify removal
transactionData[LYLTY_CARD_NBR == 226000]

# Remove outlier

# Summary after removing outlier
summary(transactionData)



# ==========================================================
# 9. DAILY TRANSACTION ANALYSIS
# ==========================================================

# Count transactions per day
transactions_by_day <- transactionData[, .N, by = DATE]

# Sort by date
setorder(transactions_by_day, DATE)

# View first few rows
head(transactions_by_day)

# Count unique dates
nrow(transactions_by_day)



# ==========================================================
# 10. CHECK FOR MISSING TRANSACTION DATES
# ==========================================================

# Create complete sequence of dates
all_dates <- data.table(
  DATE = seq(
    as.Date("2018-07-01"),
    as.Date("2019-06-30"),
    by = "day"
  )
)

# Merge with daily transaction counts
transactions_by_day <- merge(
  all_dates,
  transactions_by_day,
  by = "DATE",
  all.x = TRUE
)

# Replace missing values with zero
transactions_by_day[is.na(N), N := 0]

# Display missing dates
transactions_by_day[N == 0]



# ==========================================================
# 11. CREATE PACK SIZE FEATURE
# ==========================================================

# Extract pack size (grams) from the product name
transactionData[, PACK_SIZE := parse_number(PROD_NAME)]

# View first few records
head(transactionData[, .(PROD_NAME, PACK_SIZE)])

# Display all unique pack sizes
transactionData[, .N, by = PACK_SIZE][order(PACK_SIZE)]



# ==========================================================
# 12. CREATE BRAND FEATURE
# ==========================================================

# Extract the first word of each product name as the brand
transactionData[, BRAND := word(PROD_NAME, 1)]

# Display first few records
head(transactionData[, .(PROD_NAME, BRAND)])

transactionData[, .N, by = BRAND][order(-N)]


# ==========================================================
# 13. CLEAN BRAND NAMES
# ==========================================================

# Standardize inconsistent brand names
transactionData[BRAND == "Dorito", BRAND := "Doritos"]
transactionData[BRAND == "Red", BRAND := "RRD"]
transactionData[BRAND == "Smith", BRAND := "Smiths"]
transactionData[BRAND == "Infzns", BRAND := "Infuzions"]
transactionData[BRAND == "Grain", BRAND := "GrnWves"]
transactionData[BRAND == "Snbts", BRAND := "Sunbites"]
transactionData[BRAND == "Natural", BRAND := "NCC"]

# Verify cleaned brand names
transactionData[, .N, by = BRAND][order(-N)]

transactionData[BRAND == "Dorito", BRAND := "Doritos"]


# ==========================================================
# 14. CUSTOMER DATA EXPLORATION
# ==========================================================

# View the structure of the customer dataset
str(customerData)

# Display the first 10 rows
head(customerData, 10)

# Generate summary statistics
summary(customerData)

# Check dataset dimensions
dim(customerData)

# View column names
names(customerData)

# Check for missing values in each column
colSums(is.na(customerData))


# Count unique loyalty card numbers
length(unique(customerData$LYLTY_CARD_NBR))

# Total number of rows
nrow(customerData)


# Customer count by lifestage
customerData[, .N, by = LIFESTAGE]

# Customer count by premium segment
customerData[, .N, by = PREMIUM_CUSTOMER]



# ==========================================================
# 15. MERGE TRANSACTION AND CUSTOMER DATA
# ==========================================================

# Merge transaction and customer datasets
data <- merge(
  transactionData,
  customerData,
  by = "LYLTY_CARD_NBR"
)

# View merged dataset
head(data)

# Check dimensions
dim(data)

str(data)

# ==========================================================
# 16. VERIFY MERGED DATA
# ==========================================================

# Check for missing customer information after the merge
sum(is.na(data$LIFESTAGE))

sum(is.na(data$PREMIUM_CUSTOMER))


# ==========================================================
# 17. TOTAL SALES BY CUSTOMER SEGMENT
# ==========================================================

# Calculate total sales for each customer segment
sales_by_segment <- data[
  ,
  .(TOTAL_SALES = sum(TOT_SALES)),
  by = .(LIFESTAGE, PREMIUM_CUSTOMER)
]

# Sort by total sales
setorder(sales_by_segment, -TOTAL_SALES)

# Display results
sales_by_segment


# ==========================================================
# 18. NUMBER OF CUSTOMERS BY CUSTOMER SEGMENT
# ==========================================================

# Count unique customers in each segment
customers_by_segment <- data[
  ,
  .(
    TOTAL_CUSTOMERS = uniqueN(LYLTY_CARD_NBR)
  ),
  by = .(
    LIFESTAGE,
    PREMIUM_CUSTOMER
  )
]

# Sort by number of customers
setorder(customers_by_segment, -TOTAL_CUSTOMERS)

# Display results
customers_by_segment


# ==========================================================
# 19. AVERAGE UNITS PURCHASED PER CUSTOMER
# ==========================================================

# Calculate average units purchased per customer
avg_units_per_customer <- data[
  ,
  .(
    AVG_UNITS_PER_CUSTOMER = sum(PROD_QTY) / uniqueN(LYLTY_CARD_NBR)
  ),
  by = .(
    LIFESTAGE,
    PREMIUM_CUSTOMER
  )
]

# Sort by average units purchased
setorder(avg_units_per_customer, -AVG_UNITS_PER_CUSTOMER)

# Display results
avg_units_per_customer


# ==========================================================
# 20. AVERAGE PRICE PER UNIT
# ==========================================================

# Calculate average price per unit for each customer segment
avg_price_per_unit <- data[
  ,
  .(
    AVG_PRICE_PER_UNIT = sum(TOT_SALES) / sum(PROD_QTY)
  ),
  by = .(
    LIFESTAGE,
    PREMIUM_CUSTOMER
  )
]

# Sort by average price
setorder(avg_price_per_unit, -AVG_PRICE_PER_UNIT)

# Display results
avg_price_per_unit


# ==========================================================
# 21. STATISTICAL ANALYSIS (T-TEST)
# ==========================================================

# Calculate price per unit
data[, PRICE_PER_UNIT := TOT_SALES / PROD_QTY]

# Select Mainstream Young Singles/Couples
mainstream_young <- data[
  LIFESTAGE == "YOUNG SINGLES/COUPLES" &
    PREMIUM_CUSTOMER == "Mainstream",
  PRICE_PER_UNIT
]

# Select all remaining customers
other_customers <- data[
  !(LIFESTAGE == "YOUNG SINGLES/COUPLES" &
      PREMIUM_CUSTOMER == "Mainstream"),
  PRICE_PER_UNIT
]

# Perform Welch Two Sample t-test
t_test_result <- t.test(
  mainstream_young,
  other_customers
)

# Display results
t_test_result


# ==========================================================
# 22. BRAND AFFINITY ANALYSIS
# ==========================================================

# Target customer segment
target_segment <- data[
  LIFESTAGE == "YOUNG SINGLES/COUPLES" &
    PREMIUM_CUSTOMER == "Mainstream"
]

# Calculate brand proportions for target segment
target_brand <- target_segment[, .N, by = BRAND]
target_brand[, TARGET_PROPORTION := N / sum(N)]

# Calculate brand proportions for all other customers
other_segment <- data[
  !(LIFESTAGE == "YOUNG SINGLES/COUPLES" &
      PREMIUM_CUSTOMER == "Mainstream")
]

other_brand <- other_segment[, .N, by = BRAND]
other_brand[, OTHER_PROPORTION := N / sum(N)]

# Calculate brand affinity
brand_affinity <- merge(
  target_brand[, .(BRAND, TARGET_PROPORTION)],
  other_brand[, .(BRAND, OTHER_PROPORTION)],
  by = "BRAND"
)

brand_affinity[
  ,
  AFFINITY := TARGET_PROPORTION / OTHER_PROPORTION
]

setorder(brand_affinity, -AFFINITY)

brand_affinity


# ==========================================================
# 23. PACK SIZE AFFINITY ANALYSIS
# ==========================================================

# Calculate pack size proportions for the target segment
target_packsize <- target_segment[, .N, by = PACK_SIZE]
target_packsize[, TARGET_PROPORTION := N / sum(N)]

# Calculate pack size proportions for all other customers
other_packsize <- other_segment[, .N, by = PACK_SIZE]
other_packsize[, OTHER_PROPORTION := N / sum(N)]

# Merge both tables
packsize_affinity <- merge(
  target_packsize[, .(PACK_SIZE, TARGET_PROPORTION)],
  other_packsize[, .(PACK_SIZE, OTHER_PROPORTION)],
  by = "PACK_SIZE"
)

# Calculate affinity ratio
packsize_affinity[
  ,
  AFFINITY := TARGET_PROPORTION / OTHER_PROPORTION
]

# Sort by affinity
setorder(packsize_affinity, -AFFINITY)

# Display results
packsize_affinity


# ==========================================================
# 24. FINAL BUSINESS INSIGHTS & STRATEGIC RECOMMENDATIONS
# ==========================================================

# Key Findings:
#
# 1. Older Families (Budget) generated the highest total chip sales.
#
# 2. Young Singles/Couples (Mainstream) represent the largest customer segment.
#
# 3. Older Families purchase the highest average number of chip packets per customer.
#
# 4. Young Singles/Couples (Mainstream) pay the highest average price per unit.
#
# 5. The t-test confirmed that the higher price paid by
#    Young Singles/Couples (Mainstream) is statistically significant.
#
# 6. Preferred brands for Young Singles/Couples (Mainstream):
#    - Tyrrells
#    - Twisties
#    - Doritos
#    - Tostitos
#    - Kettle
#    - Pringles
#
# 7. Preferred pack sizes:
#    - 270g
#    - 380g
#    - 330g
#    - 134g
#
# Strategic Recommendations:
#
# - Target Mainstream Young Singles/Couples with premium brands
#   such as Tyrrells, Doritos, Kettle, Twisties, Tostitos, and Pringles.
#
# - Promote the preferred pack sizes (270g, 330g, and 380g)
#   through in-store displays and promotional campaigns.
#
# - Continue supporting Older Families through value-focused
#   promotions, as they purchase the highest quantity of chips.
#
# - Use customer segmentation insights to optimize future
#   category planning, product assortment, and promotional strategies.


# ==========================================================
# 25. Data Visualization
# ==========================================================

# ==========================================================
# 25.1 TOTAL SALES BY CUSTOMER SEGMENT
# ==========================================================

ggplot(
  sales_by_segment,
  aes(
    x = reorder(
      paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "),
      TOTAL_SALES
    ),
    y = TOTAL_SALES
  )
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Total Sales by Customer Segment",
    x = "Customer Segment",
    y = "Total Sales ($)"
  ) +
  theme_minimal()


# ==========================================================
# 25.2 AVERAGE UNITS PURCHASED PER CUSTOMER
# ==========================================================

ggplot(
  avg_units_per_customer,
  aes(
    x = reorder(
      paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "),
      AVG_UNITS_PER_CUSTOMER
    ),
    y = AVG_UNITS_PER_CUSTOMER
  )
) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Average Units Purchased per Customer",
    x = "Customer Segment",
    y = "Average Units"
  ) +
  theme_minimal()


# ==========================================================
# 25.3 AVERAGE PRICE PER UNIT
# ==========================================================

ggplot(
  avg_price_per_unit,
  aes(
    x = reorder(
      paste(LIFESTAGE, PREMIUM_CUSTOMER, sep = " - "),
      AVG_PRICE_PER_UNIT
    ),
    y = AVG_PRICE_PER_UNIT
  )
) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Average Price per Unit",
    x = "Customer Segment",
    y = "Average Price ($)"
  ) +
  theme_minimal()


# ==========================================================
# 25.4 BRAND AFFINITY
# ==========================================================

top_brand_affinity <- head(brand_affinity, 10)

ggplot(
  top_brand_affinity,
  aes(
    x = reorder(BRAND, AFFINITY),
    y = AFFINITY
  )
) +
  geom_col(fill = "purple") +
  coord_flip() +
  labs(
    title = "Top 10 Brand Affinity",
    x = "Brand",
    y = "Affinity Ratio"
  ) +
  theme_minimal()


# ==========================================================
# 25.5 PACK SIZE AFFINITY
# ==========================================================

top_pack_affinity <- head(packsize_affinity, 10)

ggplot(
  top_pack_affinity,
  aes(
    x = reorder(as.factor(PACK_SIZE), AFFINITY),
    y = AFFINITY
  )
) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 10 Pack Size Affinity",
    x = "Pack Size (g)",
    y = "Affinity Ratio"
  ) +
  theme_minimal()


