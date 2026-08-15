# ==========================================================
# QUANTIUM VIRTUAL INTERNSHIP - TASK 2
# Retail Strategy and Analytics - Store Trial Analysis
# Author: Praveen Patil
# ==========================================================

# ==========================================================
# 1. LOAD LIBRARIES AND DATA
# ==========================================================

library(data.table)
library(ggplot2)

# Load QVI dataset
data <- fread("QVI_data.csv")


# ==========================================================
# 2. CREATE MONTHLY STORE METRICS
# ==========================================================

# Create month identifier
data[, MONTH_ID := format(DATE, "%Y%m")]

# Calculate monthly metrics for each store
measureOverTime <- data[
  ,
  .(
    TOT_SALES = sum(TOT_SALES),
    NUM_CUSTOMERS = uniqueN(LYLTY_CARD_NBR),
    NUM_TRANSACTIONS = uniqueN(TXN_ID),
    NUM_CHIPS = sum(PROD_QTY),
    AVG_PRICE_PER_UNIT =
      sum(TOT_SALES) / sum(PROD_QTY)
  ),
  by = .(STORE_NBR, MONTH_ID)
]

# Calculate customer behaviour metrics
measureOverTime[
  ,
  `:=`(
    AVG_TRANSACTIONS_PER_CUSTOMER =
      NUM_TRANSACTIONS / NUM_CUSTOMERS,
    
    AVG_CHIPS_PER_TRANSACTION =
      NUM_CHIPS / NUM_TRANSACTIONS
  )
]

# ==========================================================
# 3. IDENTIFY STORES WITH COMPLETE OBSERVATIONS
# ==========================================================

storesWithFullObs <- measureOverTime[
  ,
  .(MONTHS = uniqueN(MONTH_ID)),
  by = STORE_NBR
][
  MONTHS == 12,
  STORE_NBR
]

# Create pre-trial dataset
# Trial period begins in February 2019
preTrialMeasures <- measureOverTime[
  MONTH_ID < "201902" &
    STORE_NBR %in% storesWithFullObs
]

# ==========================================================
# 4. CONTROL STORE SELECTION FUNCTIONS
# ==========================================================

# ----------------------------------------------------------
# 4A. Pearson Correlation Function
# ----------------------------------------------------------

calculateCorrelation <- function(inputTable, metricCol, trialStore) {
  
  stores <- unique(inputTable$STORE_NBR)
  results <- data.table()
  
  trialData <- inputTable[
    STORE_NBR == trialStore,
    .(MONTH_ID, TRIAL = get(metricCol))
  ]
  
  for (controlStore in stores) {
    
    if (controlStore == trialStore)
      next
    
    controlData <- inputTable[
      STORE_NBR == controlStore,
      .(MONTH_ID, CONTROL = get(metricCol))
    ]
    
    mergedData <- merge(
      trialData,
      controlData,
      by = "MONTH_ID"
    )
    
    results <- rbind(
      results,
      data.table(
        Store1 = trialStore,
        Store2 = controlStore,
        correlation =
          cor(
            mergedData$TRIAL,
            mergedData$CONTROL,
            method = "pearson"
          )
      )
    )
  }
  
  results
}
# ----------------------------------------------------------
# 4B. Magnitude Similarity Function
# ----------------------------------------------------------

calculateMagnitude <- function(inputTable, metricCol, trialStore) {
  
  stores <- unique(inputTable$STORE_NBR)
  results <- data.table()
  
  trialData <- inputTable[
    STORE_NBR == trialStore,
    .(MONTH_ID, TRIAL = get(metricCol))
  ]
  
  for (controlStore in stores) {
    
    if (controlStore == trialStore)
      next
    
    controlData <- inputTable[
      STORE_NBR == controlStore,
      .(MONTH_ID, CONTROL = get(metricCol))
    ]
    
    mergedData <- merge(
      trialData,
      controlData,
      by = "MONTH_ID"
    )
    
    distance <- sum(
      abs(
        mergedData$TRIAL -
          mergedData$CONTROL
      )
    )
    
    results <- rbind(
      results,
      data.table(
        Store1 = trialStore,
        Store2 = controlStore,
        magnitude_distance = distance
      )
    )
  }
  
  results[
    ,
    magnitude_similarity :=
      1 -
      (
        magnitude_distance -
          min(magnitude_distance)
      ) /
      (
        max(magnitude_distance) -
          min(magnitude_distance)
      )
  ]
  
  results
}

# ----------------------------------------------------------
# 4C. Combined Control-Store Scoring Function
# ----------------------------------------------------------

selectControlStore <- function(trialStore) {
  
  # Sales similarity
  salesCorrelation <- calculateCorrelation(
    preTrialMeasures,
    "TOT_SALES",
    trialStore
  )
  
  salesMagnitude <- calculateMagnitude(
    preTrialMeasures,
    "TOT_SALES",
    trialStore
  )
  
  # Customer similarity
  customerCorrelation <- calculateCorrelation(
    preTrialMeasures,
    "NUM_CUSTOMERS",
    trialStore
  )
  
  customerMagnitude <- calculateMagnitude(
    preTrialMeasures,
    "NUM_CUSTOMERS",
    trialStore
  )
  
  # Combine sales scores
  salesScore <- merge(
    salesCorrelation,
    salesMagnitude,
    by = c("Store1", "Store2")
  )
  
  salesScore[
    ,
    sales_score :=
      (
        correlation +
          magnitude_similarity
      ) / 2
  ]
  
  # Combine customer scores
  customerScore <- merge(
    customerCorrelation,
    customerMagnitude,
    by = c("Store1", "Store2")
  )
  
  customerScore[
    ,
    customer_score :=
      (
        correlation +
          magnitude_similarity
      ) / 2
  ]
  
  # Combine sales and customer scores
  finalScores <- merge(
    salesScore[
      ,
      .(Store1, Store2, sales_score)
    ],
    customerScore[
      ,
      .(Store1, Store2, customer_score)
    ],
    by = c("Store1", "Store2")
  )
  
  finalScores[
    ,
    final_score :=
      (sales_score + customer_score) / 2
  ]
  
  # Rank control stores
  setorder(
    finalScores,
    -final_score
  )
  
  return(finalScores)
}

# ==========================================================
# 5. STORE 77 - CONTROL STORE SELECTION
# ==========================================================

scores_77 <- selectControlStore(77)

# Display top control candidates
head(scores_77, 10)

# Select highest scoring control store
control_77 <- scores_77[1, Store2]

control_77

# ==========================================================
# 6. STORE 86 - CONTROL STORE SELECTION
# ==========================================================

scores_86 <- selectControlStore(86)

head(scores_86, 10)

control_86 <- scores_86[1, Store2]

control_86


# ==========================================================
# 7. STORE 88 - CONTROL STORE SELECTION
# ==========================================================

scores_88 <- selectControlStore(88)

head(scores_88, 10)

control_88 <- scores_88[1, Store2]

control_88


# ==========================================================
# 8. TRIAL ANALYSIS FUNCTION
# ==========================================================

analyseTrial <- function(trialStore, controlStore) {
  
  # --------------------------------------------------------
  # Trial-period data
  # --------------------------------------------------------
  
  trialData <- measureOverTime[
    MONTH_ID >= "201902" &
      MONTH_ID <= "201904" &
      STORE_NBR %in% c(
        trialStore,
        controlStore
      )
  ]
  
  # --------------------------------------------------------
  # Basic trial-period metrics
  # --------------------------------------------------------
  
  summary <- trialData[
    ,
    .(
      TOTAL_SALES = sum(TOT_SALES),
      TOTAL_CUSTOMERS = sum(NUM_CUSTOMERS),
      AVG_TRANSACTIONS_PER_CUSTOMER =
        mean(AVG_TRANSACTIONS_PER_CUSTOMER)
    ),
    by = STORE_NBR
  ]
  
  print(summary)
  
  # --------------------------------------------------------
  # Scale control-store sales
  # --------------------------------------------------------
  
  salesScale <-
    preTrialMeasures[
      STORE_NBR == trialStore,
      sum(TOT_SALES)
    ] /
    preTrialMeasures[
      STORE_NBR == controlStore,
      sum(TOT_SALES)
    ]
  
  salesComparison <- merge(
    measureOverTime[
      STORE_NBR == controlStore,
      .(
        MONTH_ID,
        CONTROL_SALES =
          TOT_SALES * salesScale
      )
    ],
    measureOverTime[
      STORE_NBR == trialStore,
      .(
        MONTH_ID,
        TRIAL_SALES = TOT_SALES
      )
    ],
    by = "MONTH_ID"
  )
  
  # Calculate percentage difference
  salesComparison[
    ,
    percentageDiff :=
      abs(
        CONTROL_SALES -
          TRIAL_SALES
      ) /
      CONTROL_SALES
  ]
  
  # Pre-trial standard deviation
  sdSales <- sd(
    salesComparison[
      MONTH_ID < "201902",
      percentageDiff
    ]
  )
  
  # Calculate t-values
  salesComparison[
    ,
    tValue :=
      percentageDiff / sdSales
  ]
  
  # --------------------------------------------------------
  # Customer-driver analysis
  # --------------------------------------------------------
  
  customerScale <-
    preTrialMeasures[
      STORE_NBR == trialStore,
      sum(NUM_CUSTOMERS)
    ] /
    preTrialMeasures[
      STORE_NBR == controlStore,
      sum(NUM_CUSTOMERS)
    ]
  
  customerComparison <- merge(
    measureOverTime[
      STORE_NBR == controlStore,
      .(
        MONTH_ID,
        CONTROL_CUSTOMERS =
          NUM_CUSTOMERS * customerScale
      )
    ],
    measureOverTime[
      STORE_NBR == trialStore,
      .(
        MONTH_ID,
        TRIAL_CUSTOMERS =
          NUM_CUSTOMERS
      )
    ],
    by = "MONTH_ID"
  )
  
  customerComparison[
    ,
    percentageDiff :=
      abs(
        CONTROL_CUSTOMERS -
          TRIAL_CUSTOMERS
      ) /
      CONTROL_CUSTOMERS
  ]
  
  sdCustomers <- sd(
    customerComparison[
      MONTH_ID < "201902",
      percentageDiff
    ]
  )
  
  customerComparison[
    ,
    tValue :=
      percentageDiff / sdCustomers
  ]
  
  # --------------------------------------------------------
  # Return trial-period results
  # --------------------------------------------------------
  
  list(
    summary = summary,
    
    sales_test =
      salesComparison[
        MONTH_ID >= "201902" &
          MONTH_ID <= "201904",
        .(
          MONTH_ID,
          TRIAL_SALES,
          CONTROL_SALES,
          tValue
        )
      ],
    
    customer_test =
      customerComparison[
        MONTH_ID >= "201902" &
          MONTH_ID <= "201904",
        .(
          MONTH_ID,
          TRIAL_CUSTOMERS,
          CONTROL_CUSTOMERS,
          tValue
        )
      ],
    
    tCritical = qt(0.95, df = 7)
  )
}

# ==========================================================
# 9. STORE 77 TRIAL ANALYSIS
# ==========================================================

results_77 <- analyseTrial(
  77,
  control_77
)

results_77$summary
results_77$sales_test
results_77$customer_test
results_77$tCritical


# ==========================================================
# STORE 86 TRIAL ANALYSIS
# ==========================================================

results_86 <- analyseTrial(
  86,
  control_86
)

results_86$summary
results_86$sales_test
results_86$customer_test
results_86$tCritical


# ==========================================================
# STORE 88 TRIAL ANALYSIS
# ==========================================================

results_88 <- analyseTrial(
  88,
  control_88
)

results_88$summary
results_88$sales_test
results_88$customer_test
results_88$tCritical



# ==========================================================
# 10. PRE-TRIAL VISUALISATIONS
# ==========================================================

plotPreTrial <- function(trialStore, controlStore) {
  
  plotData <- preTrialMeasures[
    STORE_NBR %in% c(trialStore, controlStore)
  ]
  
  plotData[
    ,
    STORE_TYPE := ifelse(
      STORE_NBR == trialStore,
      paste("Trial Store", trialStore),
      paste("Control Store", controlStore)
    )
  ]
  
  plotData[
    ,
    MONTH := as.Date(
      paste0(MONTH_ID, "01"),
      format = "%Y%m%d"
    )
  ]
  
  # Sales graph
  salesPlot <- ggplot(
    plotData,
    aes(
      x = MONTH,
      y = TOT_SALES,
      color = STORE_TYPE,
      group = STORE_TYPE
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = paste(
        "Pre-Trial Monthly Sales:",
        trialStore, "vs", controlStore
      ),
      x = "Month",
      y = "Total Sales",
      color = "Store"
    ) +
    theme_minimal()
  
  # Customer graph
  customerPlot <- ggplot(
    plotData,
    aes(
      x = MONTH,
      y = NUM_CUSTOMERS,
      color = STORE_TYPE,
      group = STORE_TYPE
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = paste(
        "Pre-Trial Monthly Customers:",
        trialStore, "vs", controlStore
      ),
      x = "Month",
      y = "Number of Customers",
      color = "Store"
    ) +
    theme_minimal()
  
  list(
    sales = salesPlot,
    customers = customerPlot
  )
}

# Store 77
plots_77 <- plotPreTrial(77, control_77)

plots_77$sales
plots_77$customers

# Store 86
plots_86 <- plotPreTrial(86, control_86)

plots_86$sales
plots_86$customers

# Store 88
plots_88 <- plotPreTrial(88, control_88)

plots_88$sales
plots_88$customers

plots_88$customers
# ==========================================================
# 11. FINAL SIGNIFICANCE SUMMARY
# ==========================================================

summariseTrial <- function(result, store) {
  
  sales <- result$sales_test
  customers <- result$customer_test
  critical <- result$tCritical
  
  sales_significant <-
    any(sales$tValue > critical)
  
  sales_positive <-
    any(
      sales$tValue > critical &
        sales$TRIAL_SALES > sales$CONTROL_SALES
    )
  
  customer_significant <-
    any(customers$tValue > critical)
  
  cat("\nStore:", store, "\n")
  cat("Sales significant:", sales_significant, "\n")
  cat("Positive sales impact:", sales_positive, "\n")
  cat("Customer impact significant:",
      customer_significant, "\n")
}

summariseTrial(results_77, 77)
summariseTrial(results_86, 86)
summariseTrial(results_88, 88)


results_77$sales_test
results_77$customer_test

results_86$sales_test
results_86$customer_test

results_88$sales_test
results_88$customer_test


# ==========================================================
# FINAL CONTROL STORE SUMMARY
# ==========================================================

finalControls <- data.table(
  TRIAL_STORE = c(77, 86, 88),
  CONTROL_STORE = c(
    control_77,
    control_86,
    control_88
  )
)

finalControls


control_88 <- 178

# ==========================================================
# 12. STORE 88 — TRIAL SALES VS SCALED CONTROL
# ==========================================================

# ----------------------------------------------------------
# Calculate sales scaling factor
# ----------------------------------------------------------

salesScale_88 <-
  preTrialMeasures[
    STORE_NBR == 88,
    sum(TOT_SALES)
  ] /
  preTrialMeasures[
    STORE_NBR == control_88,
    sum(TOT_SALES)
  ]


# ----------------------------------------------------------
# Create full sales comparison dataset
# ----------------------------------------------------------

salesComparison_88 <- merge(
  
  measureOverTime[
    STORE_NBR == control_88,
    .(
      MONTH_ID,
      CONTROL_SALES = TOT_SALES * salesScale_88
    )
  ],
  
  measureOverTime[
    STORE_NBR == 88,
    .(
      MONTH_ID,
      TRIAL_SALES = TOT_SALES
    )
  ],
  
  by = "MONTH_ID"
)


# ----------------------------------------------------------
# Calculate percentage difference
# ----------------------------------------------------------

salesComparison_88[
  ,
  percentageDiff :=
    abs(
      CONTROL_SALES - TRIAL_SALES
    ) /
    CONTROL_SALES
]


# ----------------------------------------------------------
# Calculate pre-trial standard deviation
# ----------------------------------------------------------

sdSales_88 <- sd(
  salesComparison_88[
    MONTH_ID < "201902",
    percentageDiff
  ]
)


# ----------------------------------------------------------
# Calculate confidence limits
# ----------------------------------------------------------

tCritical_88 <- results_88$tCritical

salesComparison_88[
  ,
  LOWER_LIMIT :=
    CONTROL_SALES *
    (1 - tCritical_88 * sdSales_88)
]

salesComparison_88[
  ,
  UPPER_LIMIT :=
    CONTROL_SALES *
    (1 + tCritical_88 * sdSales_88)
]


# ----------------------------------------------------------
# Convert MONTH_ID to Date
# ----------------------------------------------------------

salesComparison_88[
  ,
  MONTH := as.Date(
    paste0(MONTH_ID, "01"),
    format = "%Y%m%d"
  )
]


# ----------------------------------------------------------
# Create Store 88 sales impact graph
# ----------------------------------------------------------

salesImpact_88 <- ggplot(
  salesComparison_88,
  aes(x = MONTH)
) +
  
  # Confidence band
  geom_ribbon(
    aes(
      ymin = LOWER_LIMIT,
      ymax = UPPER_LIMIT
    ),
    fill = "grey70",
    alpha = 0.5
  ) +
  
  # Trial Store 88
  geom_line(
    aes(y = TRIAL_SALES),
    color = "black",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = TRIAL_SALES),
    color = "black",
    size = 3
  ) +
  
  # Scaled Control Store 178
  geom_line(
    aes(y = CONTROL_SALES),
    color = "black",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = CONTROL_SALES),
    color = "black",
    size = 3
  ) +
  
  labs(
    title = paste(
      "Store 88 Trial Sales vs Scaled Control Store",
      control_88
    ),
    x = "Month",
    y = "Total Sales"
  ) +
  
  theme_minimal()


# ----------------------------------------------------------
# Display graph
# ----------------------------------------------------------

salesImpact_88



# ==========================================================
# 13. STORE 88 — TRIAL CUSTOMER IMPACT
# ==========================================================

# Get Store 88 customer test results
customerImpact_88 <- copy(results_88$customer_test)


# ----------------------------------------------------------
# Convert MONTH_ID to Date
# ----------------------------------------------------------
# ==========================================================
# QUANTIUM VIRTUAL INTERNSHIP - TASK 2
# Retail Strategy and Analytics - Store Trial Analysis
# Author: Praveen Patil
# ==========================================================

# ==========================================================
# 1. LOAD LIBRARIES AND DATA
# ==========================================================

library(data.table)
library(ggplot2)

# Load QVI dataset
data <- fread("QVI_data.csv")


# ==========================================================
# 2. CREATE MONTHLY STORE METRICS
# ==========================================================

# Create month identifier
data[, MONTH_ID := format(DATE, "%Y%m")]

# Calculate monthly metrics for each store
measureOverTime <- data[
  ,
  .(
    TOT_SALES = sum(TOT_SALES),
    NUM_CUSTOMERS = uniqueN(LYLTY_CARD_NBR),
    NUM_TRANSACTIONS = uniqueN(TXN_ID),
    NUM_CHIPS = sum(PROD_QTY),
    AVG_PRICE_PER_UNIT =
      sum(TOT_SALES) / sum(PROD_QTY)
  ),
  by = .(STORE_NBR, MONTH_ID)
]

# Calculate customer behaviour metrics
measureOverTime[
  ,
  `:=`(
    AVG_TRANSACTIONS_PER_CUSTOMER =
      NUM_TRANSACTIONS / NUM_CUSTOMERS,
    
    AVG_CHIPS_PER_TRANSACTION =
      NUM_CHIPS / NUM_TRANSACTIONS
  )
]

# ==========================================================
# 3. IDENTIFY STORES WITH COMPLETE OBSERVATIONS
# ==========================================================

storesWithFullObs <- measureOverTime[
  ,
  .(MONTHS = uniqueN(MONTH_ID)),
  by = STORE_NBR
][
  MONTHS == 12,
  STORE_NBR
]

# Create pre-trial dataset
# Trial period begins in February 2019
preTrialMeasures <- measureOverTime[
  MONTH_ID < "201902" &
    STORE_NBR %in% storesWithFullObs
]

# ==========================================================
# 4. CONTROL STORE SELECTION FUNCTIONS
# ==========================================================

# ----------------------------------------------------------
# 4A. Pearson Correlation Function
# ----------------------------------------------------------

calculateCorrelation <- function(inputTable, metricCol, trialStore) {
  
  stores <- unique(inputTable$STORE_NBR)
  results <- data.table()
  
  trialData <- inputTable[
    STORE_NBR == trialStore,
    .(MONTH_ID, TRIAL = get(metricCol))
  ]
  
  for (controlStore in stores) {
    
    if (controlStore == trialStore)
      next
    
    controlData <- inputTable[
      STORE_NBR == controlStore,
      .(MONTH_ID, CONTROL = get(metricCol))
    ]
    
    mergedData <- merge(
      trialData,
      controlData,
      by = "MONTH_ID"
    )
    
    results <- rbind(
      results,
      data.table(
        Store1 = trialStore,
        Store2 = controlStore,
        correlation =
          cor(
            mergedData$TRIAL,
            mergedData$CONTROL,
            method = "pearson"
          )
      )
    )
  }
  
  results
}
# ----------------------------------------------------------
# 4B. Magnitude Similarity Function
# ----------------------------------------------------------

calculateMagnitude <- function(inputTable, metricCol, trialStore) {
  
  stores <- unique(inputTable$STORE_NBR)
  results <- data.table()
  
  trialData <- inputTable[
    STORE_NBR == trialStore,
    .(MONTH_ID, TRIAL = get(metricCol))
  ]
  
  for (controlStore in stores) {
    
    if (controlStore == trialStore)
      next
    
    controlData <- inputTable[
      STORE_NBR == controlStore,
      .(MONTH_ID, CONTROL = get(metricCol))
    ]
    
    mergedData <- merge(
      trialData,
      controlData,
      by = "MONTH_ID"
    )
    
    distance <- sum(
      abs(
        mergedData$TRIAL -
          mergedData$CONTROL
      )
    )
    
    results <- rbind(
      results,
      data.table(
        Store1 = trialStore,
        Store2 = controlStore,
        magnitude_distance = distance
      )
    )
  }
  
  results[
    ,
    magnitude_similarity :=
      1 -
      (
        magnitude_distance -
          min(magnitude_distance)
      ) /
      (
        max(magnitude_distance) -
          min(magnitude_distance)
      )
  ]
  
  results
}

# ----------------------------------------------------------
# 4C. Combined Control-Store Scoring Function
# ----------------------------------------------------------

selectControlStore <- function(trialStore) {
  
  # Sales similarity
  salesCorrelation <- calculateCorrelation(
    preTrialMeasures,
    "TOT_SALES",
    trialStore
  )
  
  salesMagnitude <- calculateMagnitude(
    preTrialMeasures,
    "TOT_SALES",
    trialStore
  )
  
  # Customer similarity
  customerCorrelation <- calculateCorrelation(
    preTrialMeasures,
    "NUM_CUSTOMERS",
    trialStore
  )
  
  customerMagnitude <- calculateMagnitude(
    preTrialMeasures,
    "NUM_CUSTOMERS",
    trialStore
  )
  
  # Combine sales scores
  salesScore <- merge(
    salesCorrelation,
    salesMagnitude,
    by = c("Store1", "Store2")
  )
  
  salesScore[
    ,
    sales_score :=
      (
        correlation +
          magnitude_similarity
      ) / 2
  ]
  
  # Combine customer scores
  customerScore <- merge(
    customerCorrelation,
    customerMagnitude,
    by = c("Store1", "Store2")
  )
  
  customerScore[
    ,
    customer_score :=
      (
        correlation +
          magnitude_similarity
      ) / 2
  ]
  
  # Combine sales and customer scores
  finalScores <- merge(
    salesScore[
      ,
      .(Store1, Store2, sales_score)
    ],
    customerScore[
      ,
      .(Store1, Store2, customer_score)
    ],
    by = c("Store1", "Store2")
  )
  
  finalScores[
    ,
    final_score :=
      (sales_score + customer_score) / 2
  ]
  
  # Rank control stores
  setorder(
    finalScores,
    -final_score
  )
  
  return(finalScores)
}

# ==========================================================
# 5. STORE 77 - CONTROL STORE SELECTION
# ==========================================================

scores_77 <- selectControlStore(77)

# Display top control candidates
head(scores_77, 10)

# Select highest scoring control store
control_77 <- scores_77[1, Store2]

control_77

# ==========================================================
# 6. STORE 86 - CONTROL STORE SELECTION
# ==========================================================

scores_86 <- selectControlStore(86)

head(scores_86, 10)

control_86 <- scores_86[1, Store2]

control_86


# ==========================================================
# 7. STORE 88 - CONTROL STORE SELECTION
# ==========================================================

scores_88 <- selectControlStore(88)

head(scores_88, 10)

control_88 <- scores_88[1, Store2]

control_88


# ==========================================================
# 8. TRIAL ANALYSIS FUNCTION
# ==========================================================

analyseTrial <- function(trialStore, controlStore) {
  
  # --------------------------------------------------------
  # Trial-period data
  # --------------------------------------------------------
  
  trialData <- measureOverTime[
    MONTH_ID >= "201902" &
      MONTH_ID <= "201904" &
      STORE_NBR %in% c(
        trialStore,
        controlStore
      )
  ]
  
  # --------------------------------------------------------
  # Basic trial-period metrics
  # --------------------------------------------------------
  
  summary <- trialData[
    ,
    .(
      TOTAL_SALES = sum(TOT_SALES),
      TOTAL_CUSTOMERS = sum(NUM_CUSTOMERS),
      AVG_TRANSACTIONS_PER_CUSTOMER =
        mean(AVG_TRANSACTIONS_PER_CUSTOMER)
    ),
    by = STORE_NBR
  ]
  
  print(summary)
  
  # --------------------------------------------------------
  # Scale control-store sales
  # --------------------------------------------------------
  
  salesScale <-
    preTrialMeasures[
      STORE_NBR == trialStore,
      sum(TOT_SALES)
    ] /
    preTrialMeasures[
      STORE_NBR == controlStore,
      sum(TOT_SALES)
    ]
  
  salesComparison <- merge(
    measureOverTime[
      STORE_NBR == controlStore,
      .(
        MONTH_ID,
        CONTROL_SALES =
          TOT_SALES * salesScale
      )
    ],
    measureOverTime[
      STORE_NBR == trialStore,
      .(
        MONTH_ID,
        TRIAL_SALES = TOT_SALES
      )
    ],
    by = "MONTH_ID"
  )
  
  # Calculate percentage difference
  salesComparison[
    ,
    percentageDiff :=
      abs(
        CONTROL_SALES -
          TRIAL_SALES
      ) /
      CONTROL_SALES
  ]
  
  # Pre-trial standard deviation
  sdSales <- sd(
    salesComparison[
      MONTH_ID < "201902",
      percentageDiff
    ]
  )
  
  # Calculate t-values
  salesComparison[
    ,
    tValue :=
      percentageDiff / sdSales
  ]
  
  # --------------------------------------------------------
  # Customer-driver analysis
  # --------------------------------------------------------
  
  customerScale <-
    preTrialMeasures[
      STORE_NBR == trialStore,
      sum(NUM_CUSTOMERS)
    ] /
    preTrialMeasures[
      STORE_NBR == controlStore,
      sum(NUM_CUSTOMERS)
    ]
  
  customerComparison <- merge(
    measureOverTime[
      STORE_NBR == controlStore,
      .(
        MONTH_ID,
        CONTROL_CUSTOMERS =
          NUM_CUSTOMERS * customerScale
      )
    ],
    measureOverTime[
      STORE_NBR == trialStore,
      .(
        MONTH_ID,
        TRIAL_CUSTOMERS =
          NUM_CUSTOMERS
      )
    ],
    by = "MONTH_ID"
  )
  
  customerComparison[
    ,
    percentageDiff :=
      abs(
        CONTROL_CUSTOMERS -
          TRIAL_CUSTOMERS
      ) /
      CONTROL_CUSTOMERS
  ]
  
  sdCustomers <- sd(
    customerComparison[
      MONTH_ID < "201902",
      percentageDiff
    ]
  )
  
  customerComparison[
    ,
    tValue :=
      percentageDiff / sdCustomers
  ]
  
  # --------------------------------------------------------
  # Return trial-period results
  # --------------------------------------------------------
  
  list(
    summary = summary,
    
    sales_test =
      salesComparison[
        MONTH_ID >= "201902" &
          MONTH_ID <= "201904",
        .(
          MONTH_ID,
          TRIAL_SALES,
          CONTROL_SALES,
          tValue
        )
      ],
    
    customer_test =
      customerComparison[
        MONTH_ID >= "201902" &
          MONTH_ID <= "201904",
        .(
          MONTH_ID,
          TRIAL_CUSTOMERS,
          CONTROL_CUSTOMERS,
          tValue
        )
      ],
    
    tCritical = qt(0.95, df = 7)
  )
}

# ==========================================================
# 9. STORE 77 TRIAL ANALYSIS
# ==========================================================

results_77 <- analyseTrial(
  77,
  control_77
)

results_77$summary
results_77$sales_test
results_77$customer_test
results_77$tCritical


# ==========================================================
# STORE 86 TRIAL ANALYSIS
# ==========================================================

results_86 <- analyseTrial(
  86,
  control_86
)

results_86$summary
results_86$sales_test
results_86$customer_test
results_86$tCritical


# ==========================================================
# STORE 88 TRIAL ANALYSIS
# ==========================================================

results_88 <- analyseTrial(
  88,
  control_88
)

results_88$summary
results_88$sales_test
results_88$customer_test
results_88$tCritical



# ==========================================================
# 10. PRE-TRIAL VISUALISATIONS
# ==========================================================

plotPreTrial <- function(trialStore, controlStore) {
  
  plotData <- preTrialMeasures[
    STORE_NBR %in% c(trialStore, controlStore)
  ]
  
  plotData[
    ,
    STORE_TYPE := ifelse(
      STORE_NBR == trialStore,
      paste("Trial Store", trialStore),
      paste("Control Store", controlStore)
    )
  ]
  
  plotData[
    ,
    MONTH := as.Date(
      paste0(MONTH_ID, "01"),
      format = "%Y%m%d"
    )
  ]
  
  # Sales graph
  salesPlot <- ggplot(
    plotData,
    aes(
      x = MONTH,
      y = TOT_SALES,
      color = STORE_TYPE,
      group = STORE_TYPE
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = paste(
        "Pre-Trial Monthly Sales:",
        trialStore, "vs", controlStore
      ),
      x = "Month",
      y = "Total Sales",
      color = "Store"
    ) +
    theme_minimal()
  
  # Customer graph
  customerPlot <- ggplot(
    plotData,
    aes(
      x = MONTH,
      y = NUM_CUSTOMERS,
      color = STORE_TYPE,
      group = STORE_TYPE
    )
  ) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(
      title = paste(
        "Pre-Trial Monthly Customers:",
        trialStore, "vs", controlStore
      ),
      x = "Month",
      y = "Number of Customers",
      color = "Store"
    ) +
    theme_minimal()
  
  list(
    sales = salesPlot,
    customers = customerPlot
  )
}

# Store 77
plots_77 <- plotPreTrial(77, control_77)

plots_77$sales
plots_77$customers

# Store 86
plots_86 <- plotPreTrial(86, control_86)

plots_86$sales
plots_86$customers

# Store 88
plots_88 <- plotPreTrial(88, control_88)

plots_88$sales
plots_88$customers

plots_88$customers
# ==========================================================
# 11. FINAL SIGNIFICANCE SUMMARY
# ==========================================================

summariseTrial <- function(result, store) {
  
  sales <- result$sales_test
  customers <- result$customer_test
  critical <- result$tCritical
  
  sales_significant <-
    any(sales$tValue > critical)
  
  sales_positive <-
    any(
      sales$tValue > critical &
        sales$TRIAL_SALES > sales$CONTROL_SALES
    )
  
  customer_significant <-
    any(customers$tValue > critical)
  
  cat("\nStore:", store, "\n")
  cat("Sales significant:", sales_significant, "\n")
  cat("Positive sales impact:", sales_positive, "\n")
  cat("Customer impact significant:",
      customer_significant, "\n")
}

summariseTrial(results_77, 77)
summariseTrial(results_86, 86)
summariseTrial(results_88, 88)


results_77$sales_test
results_77$customer_test

results_86$sales_test
results_86$customer_test

results_88$sales_test
results_88$customer_test


# ==========================================================
# FINAL CONTROL STORE SUMMARY
# ==========================================================

finalControls <- data.table(
  TRIAL_STORE = c(77, 86, 88),
  CONTROL_STORE = c(
    control_77,
    control_86,
    control_88
  )
)

finalControls


control_88 <- 178

# ==========================================================
# 12. STORE 88 — TRIAL SALES VS SCALED CONTROL
# ==========================================================

# ----------------------------------------------------------
# Calculate sales scaling factor
# ----------------------------------------------------------

salesScale_88 <-
  preTrialMeasures[
    STORE_NBR == 88,
    sum(TOT_SALES)
  ] /
  preTrialMeasures[
    STORE_NBR == control_88,
    sum(TOT_SALES)
  ]


# ----------------------------------------------------------
# Create full sales comparison dataset
# ----------------------------------------------------------

salesComparison_88 <- merge(
  
  measureOverTime[
    STORE_NBR == control_88,
    .(
      MONTH_ID,
      CONTROL_SALES = TOT_SALES * salesScale_88
    )
  ],
  
  measureOverTime[
    STORE_NBR == 88,
    .(
      MONTH_ID,
      TRIAL_SALES = TOT_SALES
    )
  ],
  
  by = "MONTH_ID"
)


# ----------------------------------------------------------
# Calculate percentage difference
# ----------------------------------------------------------

salesComparison_88[
  ,
  percentageDiff :=
    abs(
      CONTROL_SALES - TRIAL_SALES
    ) /
    CONTROL_SALES
]


# ----------------------------------------------------------
# Calculate pre-trial standard deviation
# ----------------------------------------------------------

sdSales_88 <- sd(
  salesComparison_88[
    MONTH_ID < "201902",
    percentageDiff
  ]
)


# ----------------------------------------------------------
# Calculate confidence limits
# ----------------------------------------------------------

tCritical_88 <- results_88$tCritical

salesComparison_88[
  ,
  LOWER_LIMIT :=
    CONTROL_SALES *
    (1 - tCritical_88 * sdSales_88)
]

salesComparison_88[
  ,
  UPPER_LIMIT :=
    CONTROL_SALES *
    (1 + tCritical_88 * sdSales_88)
]


# ----------------------------------------------------------
# Convert MONTH_ID to Date
# ----------------------------------------------------------

salesComparison_88[
  ,
  MONTH := as.Date(
    paste0(MONTH_ID, "01"),
    format = "%Y%m%d"
  )
]


# ----------------------------------------------------------
# Create Store 88 sales impact graph
# ----------------------------------------------------------

salesImpact_88 <- ggplot(
  salesComparison_88,
  aes(x = MONTH)
) +
  
  # Confidence band
  geom_ribbon(
    aes(
      ymin = LOWER_LIMIT,
      ymax = UPPER_LIMIT
    ),
    fill = "grey70",
    alpha = 0.5
  ) +
  
  # Trial Store 88
  geom_line(
    aes(y = TRIAL_SALES),
    color = "black",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = TRIAL_SALES),
    color = "black",
    size = 3
  ) +
  
  # Scaled Control Store 178
  geom_line(
    aes(y = CONTROL_SALES),
    color = "black",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = CONTROL_SALES),
    color = "black",
    size = 3
  ) +
  
  labs(
    title = paste(
      "Store 88 Trial Sales vs Scaled Control Store",
      control_88
    ),
    x = "Month",
    y = "Total Sales"
  ) +
  
  theme_minimal()


# ----------------------------------------------------------
# Display graph
# ----------------------------------------------------------

salesImpact_88



# ==========================================================
# 13. STORE 88 — TRIAL CUSTOMER IMPACT
# ==========================================================

# Get Store 88 customer test results
customerImpact_88 <- copy(results_88$customer_test)


# ----------------------------------------------------------
# Convert MONTH_ID to Date
# ----------------------------------------------------------

customerImpact_88[
  ,
  MONTH := as.Date(
    paste0(MONTH_ID, "01"),
    format = "%Y%m%d"
  )
]


# ----------------------------------------------------------
# Create customer impact graph
# ----------------------------------------------------------

customerImpactGraph_88 <- ggplot(
  customerImpact_88,
  aes(x = MONTH)
) +
  
  # Trial Store 88
  geom_line(
    aes(y = TRIAL_CUSTOMERS),
    color = "black",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = TRIAL_CUSTOMERS),
    color = "black",
    size = 3
  ) +
  
  # Scaled Control Store 178
  geom_line(
    aes(y = CONTROL_CUSTOMERS),
    color = "black",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = CONTROL_CUSTOMERS),
    color = "black",
    size = 3
  ) +
  
  labs(
    title = "Store 88 Trial Customers vs Scaled Control Store 178",
    x = "Month",
    y = "Number of Customers"
  ) +
  
  theme_minimal()


# ----------------------------------------------------------
# Display graph
# ----------------------------------------------------------

customerImpactGraph_88
customerImpact_88[
  ,
  MONTH := as.Date(
    paste0(MONTH_ID, "01"),
    format = "%Y%m%d"
  )
]


# ----------------------------------------------------------
# Create customer impact graph
# ----------------------------------------------------------

customerImpactGraph_88 <- ggplot(
  customerImpact_88,
  aes(x = MONTH)
) +
  
  # Trial Store 88
  geom_line(
    aes(y = TRIAL_CUSTOMERS),
    color = "black",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = TRIAL_CUSTOMERS),
    color = "black",
    size = 3
  ) +
  
  # Scaled Control Store 178
  geom_line(
    aes(y = CONTROL_CUSTOMERS),
    color = "black",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = CONTROL_CUSTOMERS),
    color = "black",
    size = 3
  ) +
  
  labs(
    title = "Store 88 Trial Customers vs Scaled Control Store 178",
    x = "Month",
    y = "Number of Customers"
  ) +
  
  theme_minimal()


# ----------------------------------------------------------
# Display graph
# ----------------------------------------------------------

customerImpactGraph_88



# ==========================================================
# 14. STORE 86 — TRIAL SALES VS SCALED CONTROL
# ==========================================================

# ----------------------------------------------------------
# Calculate sales scaling factor
# ----------------------------------------------------------

salesScale_86 <-
  preTrialMeasures[
    STORE_NBR == 86,
    sum(TOT_SALES)
  ] /
  preTrialMeasures[
    STORE_NBR == control_86,
    sum(TOT_SALES)
  ]


# ----------------------------------------------------------
# Create full sales comparison dataset
# ----------------------------------------------------------

salesComparison_86 <- merge(
  
  measureOverTime[
    STORE_NBR == control_86,
    .(
      MONTH_ID,
      CONTROL_SALES = TOT_SALES * salesScale_86
    )
  ],
  
  measureOverTime[
    STORE_NBR == 86,
    .(
      MONTH_ID,
      TRIAL_SALES = TOT_SALES
    )
  ],
  
  by = "MONTH_ID"
)


# ----------------------------------------------------------
# Calculate percentage difference
# ----------------------------------------------------------

salesComparison_86[
  ,
  percentageDiff :=
    abs(
      CONTROL_SALES - TRIAL_SALES
    ) /
    CONTROL_SALES
]


# ----------------------------------------------------------
# Calculate pre-trial standard deviation
# ----------------------------------------------------------

sdSales_86 <- sd(
  salesComparison_86[
    MONTH_ID < "201902",
    percentageDiff
  ]
)


# ----------------------------------------------------------
# Calculate confidence limits
# ----------------------------------------------------------

tCritical_86 <- results_86$tCritical

salesComparison_86[
  ,
  LOWER_LIMIT :=
    CONTROL_SALES *
    (1 - tCritical_86 * sdSales_86)
]

salesComparison_86[
  ,
  UPPER_LIMIT :=
    CONTROL_SALES *
    (1 + tCritical_86 * sdSales_86)
]


# ----------------------------------------------------------
# Convert MONTH_ID to Date
# ----------------------------------------------------------

salesComparison_86[
  ,
  MONTH := as.Date(
    paste0(MONTH_ID, "01"),
    format = "%Y%m%d"
  )
]


# ----------------------------------------------------------
# Create Store 86 sales impact graph
# ----------------------------------------------------------

salesImpact_86 <- ggplot(
  salesComparison_86,
  aes(x = MONTH)
) +
  
  # Confidence band
  geom_ribbon(
    aes(
      ymin = LOWER_LIMIT,
      ymax = UPPER_LIMIT
    ),
    fill = "grey70",
    alpha = 0.5
  ) +
  
  # Trial Store 86
  geom_line(
    aes(y = TRIAL_SALES),
    color = "black",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = TRIAL_SALES),
    color = "black",
    size = 3
  ) +
  
  # Scaled Control Store 155
  geom_line(
    aes(y = CONTROL_SALES),
    color = "black",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = CONTROL_SALES),
    color = "black",
    size = 3
  ) +
  
  labs(
    title = paste(
      "Trial Store 86 Sales vs Scaled Control Store",
      control_86
    ),
    x = "Month",
    y = "Total Sales"
  ) +
  
  theme_minimal()


# ----------------------------------------------------------
# Display graph
# ----------------------------------------------------------
salesImpact_86





# ==========================================================
# 14. STORE 86 — TRIAL SALES VS SCALED CONTROL
# ==========================================================

# ----------------------------------------------------------
# Calculate sales scaling factor
# ----------------------------------------------------------

salesScale_86 <-
  preTrialMeasures[
    STORE_NBR == 86,
    sum(TOT_SALES)
  ] /
  preTrialMeasures[
    STORE_NBR == control_86,
    sum(TOT_SALES)
  ]


# ----------------------------------------------------------
# Create full sales comparison dataset
# ----------------------------------------------------------

salesComparison_86 <- merge(
  
  measureOverTime[
    STORE_NBR == control_86,
    .(
      MONTH_ID,
      CONTROL_SALES = TOT_SALES * salesScale_86
    )
  ],
  
  measureOverTime[
    STORE_NBR == 86,
    .(
      MONTH_ID,
      TRIAL_SALES = TOT_SALES
    )
  ],
  
  by = "MONTH_ID"
)


# ----------------------------------------------------------
# Calculate percentage difference
# ----------------------------------------------------------

salesComparison_86[
  ,
  percentageDiff :=
    abs(
      CONTROL_SALES - TRIAL_SALES
    ) /
    CONTROL_SALES
]


# ----------------------------------------------------------
# Calculate pre-trial standard deviation
# ----------------------------------------------------------

sdSales_86 <- sd(
  salesComparison_86[
    MONTH_ID < "201902",
    percentageDiff
  ]
)


# ----------------------------------------------------------
# Calculate confidence limits
# ----------------------------------------------------------

tCritical_86 <- results_86$tCritical

salesComparison_86[
  ,
  LOWER_LIMIT :=
    CONTROL_SALES *
    (1 - tCritical_86 * sdSales_86)
]

salesComparison_86[
  ,
  UPPER_LIMIT :=
    CONTROL_SALES *
    (1 + tCritical_86 * sdSales_86)
]


# ----------------------------------------------------------
# Convert MONTH_ID to Date
# ----------------------------------------------------------

salesComparison_86[
  ,
  MONTH := as.Date(
    paste0(MONTH_ID, "01"),
    format = "%Y%m%d"
  )
]


# ----------------------------------------------------------
# Create Store 86 sales impact graph
# ----------------------------------------------------------

# ----------------------------------------------------------
# Create Store 86 sales impact graph
# ----------------------------------------------------------

salesImpact_86 <- ggplot(
  salesComparison_86,
  aes(x = MONTH)
) +
  
  # Confidence band
  geom_ribbon(
    aes(
      ymin = LOWER_LIMIT,
      ymax = UPPER_LIMIT
    ),
    fill = "grey70",
    alpha = 0.5
  ) +
  
  # Trial Store 86
  geom_line(
    aes(y = TRIAL_SALES),
    color = "black",
    linewidth = 1.2
  ) +
  
  geom_point(
    aes(y = TRIAL_SALES),
    color = "black",
    size = 3
  ) +
  
  # Scaled Control Store 155
  geom_line(
    aes(y = CONTROL_SALES),
    color = "black",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  geom_point(
    aes(y = CONTROL_SALES),
    color = "black",
    size = 3
  ) +
  
  # Trial period divider
  geom_vline(
    xintercept = as.Date("2019-02-01"),
    linetype = "dashed",
    color = "red",
    linewidth = 0.8
  ) +
  
  # Period labels
  annotate(
    "text",
    x = as.Date("2018-10-15"),
    y = max(salesComparison_86$UPPER_LIMIT),
    label = "Pre-Trial Period",
    fontface = "bold",
    size = 5
  ) +
  
  annotate(
    "text",
    x = as.Date("2019-04-15"),
    y = max(salesComparison_86$UPPER_LIMIT),
    label = "Trial Period",
    fontface = "bold",
    size = 5
  ) +
  
  labs(
    title = paste(
      "Trial Store 86 Sales vs Scaled Control Store",
      control_86
    ),
    subtitle = "Solid line: Trial Store 86 | Dashed line: Scaled Control Store",
    x = "Month",
    y = "Total Sales"
  ) +
  
  theme_minimal()


# Display graph
salesImpact_86