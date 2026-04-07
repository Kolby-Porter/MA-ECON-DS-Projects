# WA Marketing Campaign Analysis #
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(cobalt)
library(patchwork)

path <- "C:/Users/Kolby.Porter/MA-ECON-DS-Projects/projects/causal-inference/1_experimentation_ab_testing/WA_Marketing-Campaign.csv" # nolint: line_length_linter.

data <- read_csv(path)

summary(data)

#### Design Audit ####

# Research Question: Determine the effectiveness of three promotion strategies on sales. # nolint

# Before we begin, we need to outline the treatment, outcomes, and unit of analysis # # nolint

# Treatment: One of three promotional campaigns (1, 2, or 3) #
# Outcomes: SalesInThousands, the measure of sales in thousands of dollars #

# Unit of Analysis: store-week observation, identified by LocationID observed #
# over one of four campaign weeks #
# Because treatment is assigned at the store level but outcomes are measured #
# weekly, the effective treatment unit is the store, with repeated observations #
# over time. #

# Covariates:
## AgeOfStore, MarketSize, MarketID, week (one of four weeks when the promotions were run) # nolint

factor_data <- data %>%
    mutate(MarketSize = as.factor(MarketSize)) %>%
    mutate(Promotion = as.factor(Promotion))

# In investgating design validity, we will be assessing: #

    # 1.1 Group Means - SMD #

colnames(factor_data)
bal.tab(Promotion ~ AgeOfStore + MarketSize + MarketID + week, data = factor_data)

## All covariates exhibit small standardized differences (|SMD| < 0.1), ##
## indicating good balance consistent with random assignment. ##

    # 1.2 General Balance, Treatment Assignment and Independence #

table(factor_data$Promotion)
prop.table(table(factor_data$Promotion))


## No obvious allocation bias ##

table(factor_data$LocationID)

table(factor_data$LocationID, factor_data$Promotion)

## Each store is assigned to a single promotion and observed across four weeks,
## implying that treatment is assigned at the store level. This avoids treatment
## contamination within stores, but introduces repeated observations per unit.
## As a result, outcomes may be correlated within stores, and standard errors
## should account for clustering at the store level. ##

    # 1.3 Covariate Distributions #

ggplot(factor_data, aes(x= MarketSize, fill = Promotion)) + 
    geom_bar(position = ("dodge"))

ggplot(factor_data, aes(x = AgeOfStore, fill = Promotion)) +
    geom_density(alpha = 0.3)

## There is a secondary concentration of older stores within Promotion 3,
## suggesting slight distributional differences not captured by mean-based balance metrics.  # nolint
## However, substantial overlap remains across groups, and standardized differences remain small # nolint


    # 1.4 Sufficient Overlap - Positivity #

vars <- c("AgeOfStore", "week")

plots <- lapply(vars, function(var) {
    ggplot(factor_data, aes_string(x = var, fill = "Promotion")) +
        geom_density(alpha = 0.3) +
        labs(title = paste("Distribution of", var, "by Promotion Group"))
})

wrap_plots(plots)

ggplot(factor_data, aes(x = MarketSize, fill = Promotion)) +
  geom_bar(position = "dodge")

## There is substantial overlap in the support of each covariate across ##
## promotion groups, with no evidence of complete separation. ##

#### DESIGN AUDIT SUMMARY ####
# Overall, the design appears valid for causal inference, with good balance across covariates,
# no obvious allocation bias, and sufficient overlap in covariate distributions. 
# The main concern is the presence of repeated observations per store, 
# which may require adjustments for clustering in the analysis phase. 
# However, the random assignment of promotions to stores supports the assumption of 
# independence between treatment and potential outcomes, allowing us to proceed with 
# estimation while accounting for within-store correlation.

# Data Audit #

    # Structure #

head(factor_data)

str(factor_data)

factor_data %>%
  count(LocationID, week) %>%
  filter(n > 1)
dim(factor_data) 

## No duplicate store-week observations detected. Data structure is consistent
## with the intended unit of analysis. ##

    # Complteness #

colSums(is.na(factor_data))

## No missing values detected across key variables, reducing concern ##
## for attrition or measurement bias. ##

    # Validity #

neg_sales <- factor_data %>%
    filter(SalesInThousands < 0)
head(neg_sales)

sales_data <- factor_data %>%
    group_by(Promotion, week) %>%
    summarise(total_sales = sum(SalesInThousands))
head(sales_data)


ggplot(sales_data, aes(x = week, y = total_sales, group = Promotion, color = Promotion)) + 
    geom_line()

ggplot(factor_data, aes(x = SalesInThousands, group = Promotion, fill = Promotion)) +
    geom_density(alpha = 0.5)

promo_2_sales <- factor_data %>%
    filter(Promotion == 2)

summary(factor_data$SalesInThousands)

q1 <- quantile(factor_data$SalesInThousands, 0.25)
q3 <- quantile(factor_data$SalesInThousands, 0.75)
iqr <- q3 - q1

outliers <- promo_2_sales %>%
  filter(SalesInThousands < (q1 - 1.5*iqr) |
         SalesInThousands > (q3 + 1.5*iqr))

dim(outliers)

outliers

ggplot(promo_2_sales, aes(x = SalesInThousands)) +
    geom_histogram(binwidth = 5, fill = "blue", color = "black") +
    labs(title = "Distribution of SalesInThousands for Promotion 2")

## Promotion 2 exhibits a larger share of low sales observations relative to
## other groups. Given randomized assignment and strong covariate balance,
## this pattern likely reflects realized outcome variation rather than
## systematic differences in store quality. ##

    # Measurement #

## SalesInThousands is a continuous measure of store performance and is
## consistently defined across stores and weeks. As outcomes are measured
## after treatment assignment, there is no evidence of post-treatment bias
## in covariates or outcome construction.
##
## Covariates (AgeOfStore, MarketSize, MarketID, week) are pre-treatment
## characteristics and are not influenced by promotional assignment,
## supporting their validity for adjustment in estimation. ##

# Identification #

# Estimation #

# Validation #

# Decision #

