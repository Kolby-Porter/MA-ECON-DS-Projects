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



# Identification #

# Estimation #

anova_model <- aov(SalesInThousands ~ Promotion, data = factor_data)

summary(anova_model)
plot(anova_model)

## Thers is strong statistical evidence of differences in mean sales across ##
## the three promotion strategies (p < 0.001). ##

TukeyHSD(anova_model)

## Promotion 2 underperforms significantly relative to both alternatives, ##
## reducing average weekly sales by approximately 8–11 thousand units relative##
## to the best-performing strategies (which were 1 and 3) ##

# Validation #

# Decision #

