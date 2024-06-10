library(readxl)
df <- read_excel('/Users/jacopobinati/Desktop/thesis/Datasets/Data Finale_balanced1.xlsx')

library(igraph)
edges <- matrix(
  c('Trade Unions Density', 'Gini Index',
    'Confounders', 'Trade Unions Density',
    'Confounders', 'Gini Index'),
  ncol = 2, byrow = TRUE
)

P <- graph_from_edgelist(edges, directed = TRUE)

V(P)$label_shape <- "ellipse"

# Plot 
plot(
  P, layout = layout_with_sugiyama(P), vertex.size = 20, vertex.label = V(P)$name, 
  vertex.color = 'skyblue', edge.arrow.size = 1, main = "Causal Map", 
  font.main = 2, cex.main = 1.5, vertex.label.cex = 1, vertex.label.family = "Times", 
  vertex.label.color = "black", margin = -0
)

set.seed(123)
nodes_with_categories <- data.frame(
  name = c('Collective Bargain Coverage', 'Trade Unions Density', 'Gini Index', 
           'Real Wage Growth', 'Annual Inflation', 
           'Exports', 
           'Foreign direct investment, inflows', 
           'Foreign direct investment, outflows', 
           'Imports', 
           'Current health expenditure', 
           'Central government debt', 
           'GDP', 
           'Labour force participation rate', 
           'Unemployment rate', 'Labor force, total', 
           'Labor tax and contributions', 
           'Part time employment, total', 'Monthly Minimum Wage'),
  category = c(rep('Collective Bargaining', 2), 'Income Inequality', rep('Economic', 11), 
               rep('Labor Market', 4))
)
edges <- matrix(c('Trade Unions Density', 'Gini Index',
                  'Collective Bargain Coverage', 'Trade Unions Density',
                  'Real Wage Growth', 'Trade Unions Density',
                  'Annual Inflation', 'Real Wage Growth',
                  'Annual Inflation', 'Labor tax & contributions',
                  'Labor tax and contributions', 'GDP',
                  'Labor tax and contributions', 'Labor force',
                  'Part time employment', 'Labor force',
                  'Labour force participation rate', 'Labor force',
                  'Unemployment rate', 'Labor force',
                  'Labor force', 'Real Wage Growth',
                  'Labor force', 'Annual Inflation',
                  'Labor force', 'Collective Bargain Coverage',
                  'Labor force', 'GDP',
                  'GDP', 'Real Wage Growth',
                  'Exports', 'GDP',
                  'FDI, inflows', 'GDP',
                  'FDI, outflows', 'GDP',
                  'Imports', 'GDP'), 
                byrow = TRUE, ncol = 2)

G <- graph_from_edgelist(edges, directed = TRUE)
V(G)$category <- nodes_with_categories$category[match(V(G)$name, nodes_with_categories$name)]

c <- plot(G, 
          layout=layout_with_fr(G),
          vertex.size=40, 
          vertex.label=V(G)$name, 
          vertex.color="lightblue", 
          main="Causal Map: Labour Unions and Income Inequality",
          edge.arrow.size=.7, 
          vertex.label.cex=0.8, 
          vertex.label.family="Times")

library(ggplot2)
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/fullcausalmap.png", plot = c, width = 10, height = 6)

library(tidyr)
library(dplyr)
library(tibble)
df <- as_tibble(df)
df <- df %>%
  rename(
    Labor_force = `Labor force, total`,
    GDP = `GDP (current US$)`,
    Inflation = `Annual Inflation CPI`,
    WageGrowth = `Real Average Annual Wage Growth`,
    Female_Workers = `Labour force participation rate (Women age 15 to 64)`,
    TradeUnions_Density = `TradeUnions_Density`, # Ensure case sensitivity matches your original dataframe
    Gov_debt = `Central government debt, total (% of GDP)`,
    WomenUnemployment_rate = `Unemployment rate (Women age 15 to 64)`,
    Health_expenditure = `Current health expenditure (% of GDP)`,
    Exports_growth = `Exports of goods and services (annual % growth)`,
    Imports_growth = `Imports of goods and services (% of GDP)`,
    FDI_inflow = `Foreign direct investment, net inflows (% of GDP)`,
    FDI_outflow = `Foreign direct investment, net outflows (% of GDP)`,
    Tax_contribution = `Labor tax and contributions (% of commercial profits)`,
    Parttime_employment = `Part time employment, total (% of total employment)`,
    Monthly_Minimum_Wage = `Monthly Minimum Wage`,
    Gini_Index = `Gini Index`,
  )
rownames(df) <- NULL
df <- df %>%
  mutate(lnGDP = round(log(GDP), 2))
# Create a binary column 'minwage' where 1 indicates there's at least one positive value in 'Monthly Minimum Wage'
df <- df %>%
  mutate(minwage = ifelse(Monthly_Minimum_Wage > 0, 1, 0))
df <- df %>%
  mutate(lnminwage = ifelse(Monthly_Minimum_Wage > 0, round(log(Monthly_Minimum_Wage), 2), 0))
df <- df %>%
  mutate(ln_labor = ifelse(Labor_force > 0, round(log(Labor_force), 2), 0))

print(df %>%
        select(Monthly_Minimum_Wage, minwage, lnminwage),
      n = Inf)
first_year_positive_lnminwage <- df %>%
  filter(lnminwage > 0) %>%
  group_by(Country) %>%
  summarize(FirstYear = min(Year))

df$Year <- as.numeric(as.character(df$Year))

# Calculate  averages for each year
average_gini <- aggregate(Gini_Index ~ Year, data = df, FUN = mean)
average_trade_union <- aggregate(TradeUnions_Density ~ Year, data = df, FUN = mean)
average_collective_bargaining <- aggregate(CollectiveBargain_Coverage ~ Year, data = df, FUN = mean)
df$Gini_Index <- (df$Gini_Index /100)
df$TradeUnions_Density <- (df$TradeUnions_Density/100)
df$CollectiveBargain_Coverage <- (df$CollectiveBargain_Coverage/100)

summary(df$Gini_Index)
summary(df$TradeUnions_Density)
summary(df$CollectiveBargain_Coverage)

df <- df %>%
  mutate(unions = sqrt(TradeUnions_Density) * log(CollectiveBargain_Coverage))
summary(df$unions)

df <- df %>%
  mutate(
    densityOcoverage = TradeUnions_Density / CollectiveBargain_Coverage,
    density2 = TradeUnions_Density^2,
    coverage0density = CollectiveBargain_Coverage / TradeUnions_Density,
    density3 = TradeUnions_Density^3,
    sqdensity = sqrt(TradeUnions_Density),
    WageGrowth2 = WageGrowth^2
  )

# Summarize the new variables
summary(df$densityOcoverage)
summary(df$density2)
summary(df$coverage0density)
summary(df$density3)
summary(df$sqdensity)

sapply(df, function(x) sum(is.na(x)) / length(x))

library(readr)
rol <- read_csv("/Users/jacopobinati/Desktop/thesis/Datasets/rule-of-law-index.csv")
part <- read_csv("/Users/jacopobinati/Desktop/thesis/Datasets/civil-society-participation-index.csv")

countries_to_keep <- c('Austria', 'Belgium', 'Czechia', 'Denmark', 'Estonia', 'Finland', 
                       'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 
                       'Italy', 'Lithuania', 'Luxemburg', 'Netherlands', 'Norway', 'Poland', 
                       'Portugal', 'Slovak Republic', 'Spain', 'Sweden', 'Switzerland', 
                       'United Kingdom')

rol <- rol %>% 
  filter(Year >= 1990, Entity %in% countries_to_keep)

part <- part %>%
  filter(Year >= 1990, Entity %in% countries_to_keep)

columns_to_drop1 <- c('civsoc_particip_vdem_low_owid', 'civsoc_particip_vdem_high_owid')
columns_to_drop2 <- c('rule_of_law_vdem_low_owid', 'rule_of_law_vdem_high_owid')

rol <- select(rol, -one_of(columns_to_drop2))
part <- select(part, -one_of(columns_to_drop1))

rol <- rename(rol, rol = rule_of_law_vdem_owid, Country = Entity)
part <- rename(part, civsoc_particip = civsoc_particip_vdem_owid, Country = Entity)
# merging process
df <- merge(df, rol, by = c("Country", "Year"), all.x = TRUE)
df <- merge(df, part, by = c("Country", "Year"), all.x = TRUE)

# Drop unnecessary columns
dropdf <- c('Code.x', 'Code.y')
df <- select(df, -one_of(dropdf))

df$demo <- df$civsoc_particip * df$rol

dataset <- df

# Check for missing values in the dataset
missing_values <- is.na(dataset)
missing_values_summary <- colSums(missing_values)
print(missing_values_summary)
dataset <- dataset %>%
  filter(!Country %in% c("Slovak Republic", "Luxembourg"))

dataset$net_export <- dataset$Exports_growth - dataset$Imports_growth
dataset$net_FDI <- dataset$FDI_inflow - dataset$FDI_outflow
average_demo <- aggregate(demo ~ Year, data = dataset, FUN = mean)
average_civicsoc_particip <- aggregate(civsoc_particip ~ Year, data = dataset, FUN = mean)
average_rol <- aggregate(rol ~ Year, data = dataset, FUN = mean)

# ccalculate the normalized density and demo variables
dataset$norm_density <- (dataset$TradeUnions_Density - min(dataset$TradeUnions_Density)) / (max(dataset$TradeUnions_Density) - min(dataset$TradeUnions_Density))
dataset$norm_demo <- (dataset$demo - min(dataset$demo)) / (max(dataset$demo) - min(dataset$demo))

average_norm_density <- aggregate(norm_density ~ Year, data = dataset, FUN = mean)
average_norm_demo <- aggregate(norm_demo ~ Year, data = dataset, FUN = mean)

dataset$Year <- as.integer(dataset$Year)
dataset$Country <- as.factor(dataset$Country)
##########################################################
# Sum of NA values in each column
na_count <- sapply(dataset, function(x) sum(is.na(x)))
print(na_count)
# Sum of infinite values in each column
inf_count <- sapply(dataset, function(x) sum(is.infinite(x)))
print(inf_count)
# Combined check for any NA or infinite values across the entire dataset
total_issues <- sum(na_count + inf_count)
print(total_issues)

df_long <- tidyr::pivot_longer(dataset, 
                               cols = -c(Country, Year),  
                               names_to = "Variable",
                               values_to = "Value")

library(stargazer)
library(plm)
f1 <- c(
  "Gini_Index ~ density2 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate",
  
  "Gini_Index ~ density2 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export",
  
  "Gini_Index ~ density2 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + net_FDI + Tax_contribution"
)

create_models <- function(data, type, formulas) {
  lapply(formulas, function(formula) {
    formula <- as.formula(formula)
    if (type == "OLS") {
      return(lm(formula, data = dataset))
    } else if (type == "FE") {
      return(plm(formula, data = dataset, model = "within", effect = "twoways"))
    } else {
      stop("Unknown model type")
    }
  })
}

# Create OLS, FE models
ols_models <- create_models(df, "OLS", f1)
fe_models <- create_models(pdata, "FE", f1)

# Summarize the models
stargazer(ols_models[c(1, 2, 3)], type = "text", title = "OLS Regression Models")
stargazer(fe_models[c(1, 2, 3)], type = "text", title = "Fixed Effects Regression Models")

# plot TradeUnions_Density and Gini_Index to understand if the relationship is linear or not
ggplot(dataset, aes(x = TradeUnions_Density, y = Gini_Index)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  geom_smooth(method = "loess", color = "red", se = FALSE) + 
  labs(title = 'Trade Unions Density and Gini Index',
       x = 'Trade Unions Density',
       y = 'Gini Index') +
  theme_minimal()


## 4. Density and Coverage linear or non linear relationship?
# plot TradeUnions_Density and Gini_Index to understand if the relationship is linear or not
ggplot(dataset, aes(x = TradeUnions_Density, y = Gini_Index)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) + 
  geom_smooth(method = "loess", color = "red", se = FALSE) + 
  labs(title = 'Trade Unions Density and Gini Index',
       x = 'Trade Unions Density',
       y = 'Gini Index') +
  theme_minimal()

library(plm)
# Fit linear and non-linear models using plm
# Define the panel data structure
pdata <- pdata.frame(dataset, index = c("Country", "Year"))
# Fit a linear model
linear_model <- plm(Gini_Index ~ TradeUnions_Density, data = pdata, model = "within", effect = "twoways")
# Summarize the models
summary(linear_model)

non_linear_model <- plm(Gini_Index ~ TradeUnions_Density + I(density2), data = pdata, model = "within", effect = "twoways")
summary(non_linear_model)

stargazer(linear_model, non_linear_model, type = "text")

stargazer(linear_model, non_linear_model, type = "latex", out = "model_results.tex",
          title = "Regression Results",
          covariate.labels = c("Trade Unions Density", "Trade Unions Density Squared"),
          dep.var.labels = "Gini Index",
          model.names = FALSE,
          align = TRUE,
          no.space = TRUE,
          omit.stat = c("LL", "ser", "f"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# AIC and BIC calculations
n <- nrow(dataset) # number of observations

# Linear model
rss_linear <- sum(residuals(linear_model)^2)
k_linear <- length(coef(linear_model))
aic_linear <- n * log(rss_linear / n) + 2 * k_linear
bic_linear <- n * log(rss_linear / n) + log(n) * k_linear

# Non-linear model
rss_non_linear <- sum(residuals(non_linear_model)^2)
k_non_linear <- length(coef(non_linear_model))
aic_non_linear <- n * log(rss_non_linear / n) + 2 * k_non_linear
bic_non_linear <- n * log(rss_non_linear / n) + log(n) * k_non_linear

results <- data.frame(
  Model = c("Linear Model", "Non-linear Model"),
  AIC = c(aic_linear, aic_non_linear),
  BIC = c(bic_linear, bic_non_linear)
)

# Print the results
cat("Linear Model: AIC =", aic_linear, "BIC =", bic_linear, "\n")
cat("Non-linear Model: AIC =", aic_non_linear, "BIC =", bic_non_linear, "\n")

library(xtable)
results_table <- xtable(results, caption = "AIC and BIC for Linear and Non-linear Models", label = "tab:aic_bic")

# Save the table to a .tex file in the specified directory
output_file <- "/Users/jacopobinati/Desktop/thesis/results/aic_bic_results.tex"
print(results_table, file = output_file, type = "latex", include.rownames = FALSE)

library(patchwork)

# Plot residuals for the linear model
p1 <- ggplot(dataset, aes(x = TradeUnions_Density, y = residuals(linear_model))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = 'Residuals of Linear Model',
       x = 'Trade Unions Density',
       y = 'Residuals') +
  theme_minimal() + 
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 16))

# Plot residuals for the non-linear model
p2 <- ggplot(dataset, aes(x = TradeUnions_Density, y = residuals(non_linear_model))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = 'Residuals of Non-linear Model',
       x = 'Trade Unions Density',
       y = 'Residuals') +
  theme_minimal() +
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 16))

# Combine the two plots into a subplot
combined_plot <- p1 + p2 + plot_layout(ncol = 1)

# Display the combined plot
print(combined_plot)

# Save the combined plot
ggsave("/Users/jacopobinati/Desktop/thesis/Images/residuals_combined_plot.png", plot = combined_plot, width = 8, height = 12)

# Extract fitted values and residuals
dataset$fitted_linear <- fitted(linear_model)
dataset$residuals_linear <- residuals(linear_model)
dataset$fitted_non_linear <- fitted(non_linear_model)
dataset$residuals_non_linear <- residuals(non_linear_model)

library(gridExtra)
# Residuals vs Fitted for Linear Model
p3 <- ggplot(dataset, aes(x = fitted_linear, y = residuals_linear)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = 'Residuals vs Fitted Values for Linear Model',
       x = 'Fitted Values',
       y = 'Residuals') +
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 16))

# Q-Q Plot for Linear Model Residuals
p4 <- ggplot(dataset, aes(sample = residuals_linear)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = 'Q-Q Plot for Linear Model Residuals') +
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 16))

# Residuals vs Fitted for Non-linear Model
p5 <- ggplot(dataset, aes(x = fitted_non_linear, y = residuals_non_linear)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "red", se = FALSE) +
  labs(title = 'Residuals vs Fitted Values for Non-linear Model',
       x = 'Fitted Values',
       y = 'Residuals') +
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 16))

# Q-Q Plot for Non-linear Model Residuals
p6 <- ggplot(dataset, aes(sample = residuals_non_linear)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = 'Q-Q Plot for Non-linear Model Residuals') +
  theme_minimal()+
  theme(
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12),
    plot.title = element_text(family = "Times New Roman", size = 16))

# Combine plots into a single subplot
combined_plot1 <- grid.arrange(p3, p4, p5, p6, ncol = 2)

# Display the combined plot
print(combined_plot1)

# Save the combined plot
ggsave("/Users/jacopobinati/Desktop/thesis/Images/residuals_qq_combined_plot.png", plot = combined_plot1, width = 12, height = 8)

#### 4.1 Inference About "unions"


# Assuming 'unions' is the column containing unions data
unions_variable <- 'unions'

# Summary statistics
summary_stats <- summary(dataset[, unions_variable])

# Quantiles
quantiles <- quantile(dataset[, unions_variable])

# Counts
counts <- table(dataset[, unions_variable])

# Combine all statistics into a data frame
statistics <- data.frame(
  Mean = mean(dataset[, unions_variable]),
  Median = median(dataset[, unions_variable]),
  Standard_Deviation = sd(dataset[, unions_variable]),
  Minimum = min(dataset[, unions_variable]),
  Maximum = max(dataset[, unions_variable]),
  Q1 = quantiles[2],
  Q3 = quantiles[4],
  Counts = counts
)

statistics
ggplot(dataset, aes(x = dataset[, unions_variable])) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(x = "Unions", y = "Frequency", 
       title = "Histogram of Unions")

#### 4.2 Check the variance of TradeUnions_Density and skewness of CollectiveBargain_Coverage


# Check the variance of TradeUnions_Density
var_TradeUnions_Density <- var(dataset$TradeUnions_Density)

# Check the skewness of CollectiveBargain_Coverage
skewness_CollectiveBargain_Coverage <- sum((log(dataset$CollectiveBargain_Coverage) - mean(log(dataset$CollectiveBargain_Coverage)))^3) / (length(dataset$CollectiveBargain_Coverage) * sd(log(dataset$CollectiveBargain_Coverage))^3)

# Print results
cat("Variance of TradeUnions_Density:", var_TradeUnions_Density, "\n")
cat("Skewness of log(CollectiveBargain_Coverage):", skewness_CollectiveBargain_Coverage, "\n")

# Visualize distribution of CollectiveBargain_Coverage
ggplot(dataset, aes(x = log(CollectiveBargain_Coverage))) +
  geom_histogram(binwidth = 0.05, fill = "lightblue", color = "black") +
  labs(x = "log(CollectiveBargain_Coverage)", y = "Frequency", 
       title = "Histogram of log(CollectiveBargain_Coverage)") + 
  theme_minimal()

ggplot(dataset, aes(x = TradeUnions_Density)) +
  geom_histogram(binwidth = 0.025, fill = 'lightblue', color = "black") + 
  theme_minimal()
        
# Variance of TradeUnions_Density: 541.3063 
# Skewness of log(CollectiveBargain_Coverage): -1.008107 

#### 4.3 Which of the interactions better perform?

summary(dataset$density2)
summary(dataset$Gini_Index)
summary(dataset$densityOcoverage)
summary(dataset$coverage0density)

# Fit linear models
model_u1 <- lm(Gini_Index ~ densityOcoverage, data = dataset)
model_u2 <- lm(Gini_Index ~ density2, data = dataset)
model_u3 <- lm(Gini_Index ~ coverage0density, data = dataset)

# Calculate AIC and BIC for each model
aic_u1 <- AIC(model_u1)
aic_u2 <- AIC(model_u2)
aic_u3 <- AIC(model_u3)

bic_u1 <- BIC(model_u1)
bic_u2 <- BIC(model_u2)
bic_u3 <- BIC(model_u3)

# Compare AIC and BIC values
aic_values <- c(aic_u1, aic_u2, aic_u3)
bic_values <- c(bic_u1, bic_u2, bic_u3)
model_names <- c("model_u1", "model_u2", "model_u3")

# Create a summary dataframe
comparison_df <- data.frame(
  Model = model_names,
  AIC = aic_values,
  BIC = bic_values
)

# Print the comparison
print(comparison_df)

# Find the model with the lowest AIC and BIC
best_model_aic <- model_names[which.min(aic_values)]
best_model_bic <- model_names[which.min(bic_values)]

cat("The best model according to AIC is:", best_model_aic, "\n")
cat("The best model according to BIC is:", best_model_bic, "\n")


#### 4.4 Summary Statistics


library(dplyr)
library(stargazer)

variables <- c('Gini_Index', 'density2', 'CollectiveBargain_Coverage', 'Inflation', 
               'WageGrowth', 'lnminwage', 'lnGDP', 'demo', 'net_export', 'net_FDI',
               'Gov_debt', 'ln_labor', 'Female_Workers', 'Parttime_employment', 
               'WomenUnemployment_rate', 'Tax_contribution')

selected_data <- dataset %>% select(all_of(variables))
summary_stats <- summary(selected_data)
print(summary_stats)

stargazer(selected_data, type = "latex", out = "summary_statistics.tex")


## **5. Plots**


library(ggplot2)

YearIntroduced <- df_long %>%
  group_by(Country) %>%
  filter(Variable == "lnminwage" & Value > 0) %>%
  slice(1) %>%
  ungroup()

YearIntroduced <- dataset %>%
  group_by(Country) %>%
  mutate(YearIntroduced = ifelse(lnminwage > 0, 1, 0)) 

# Plot with YearIntroduced
b <- ggplot(dataset, aes(x = Year)) +
  geom_line(aes(y = Gini_Index, color = "Gini Index"), linetype = "solid") +
  geom_line(aes(y = TradeUnions_Density, color = "Trade Union Density"), linetype = "dashed") +
  facet_wrap(~ Country, scales = "free_y", nrow = 5) +
  labs(title = "Gini Index and Trade Union Density Over Time by Country",
       y = "Value",
       x = "Year",
       color = "Indicator") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))
b

# Plot with YearIntroduced
ggplot(dataset, aes(x = Year)) +
  geom_line(aes(y = WageGrowth, color = "Wage_Growth"), linetype = "solid") +
  geom_line(aes(y = Inflation, color = "Inflation"), linetype = "dashed") +
  facet_wrap(~ Country, scales = "free_y", nrow = 5) +
  labs(title = "Wage and INFLATION Over Time by Country",
       y = "Value",
       x = "Year",
       color = "Indicator") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))


data4 <- data.frame(average_trade_union = average_trade_union, average_gini = average_gini)

x <- ggplot(data4, aes(x = average_trade_union.TradeUnions_Density, y = average_gini.Gini_Index)) +
  geom_smooth(aes(color = "Linear Regression"), method = "lm", se = FALSE) +
  geom_smooth(aes(color = "Loess"), method = "loess", se = FALSE) + 
  labs(x = "Average Trade Union Density", y = "Average Gini Index",
       color = "Method") + 
  ggtitle("Relationship between Trade Union Density and Gini Index") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16)) +
  scale_color_manual(values = c("black", "blue"),
                     labels = c("Linear Regression", "Loess"))

ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/relationGini_Density.png", plot = x, width = 10, height = 6)
print(x)

# Save the plot
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/evolutionGini&Density.png", plot = b, width = 10, height = 6)

# Plotting the histogram for Gini_Index
ggplot(df, aes(x = Gini_Index)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "lightblue") +
  ggtitle("Histogram of Gini Index") +
  xlab("Gini Index") +
  ylab("Frequency") +
  theme_minimal() + 
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))

# Correlation Coefficients of Wealth Inequality vs. Union Density forEach Country IndividuallyUnion
correlation_df <- df %>%
  group_by(Country) %>%
  summarize(correlation = cor(Gini_Index, TradeUnions_Density, use = "pairwise.complete.obs"))
correlation_df$Country <- factor(correlation_df$Country, levels = correlation_df$Country[order(correlation_df$correlation)])

ggplot(correlation_df, aes(x = Country, y = correlation)) +
  geom_bar(stat = "identity", fill = "skyblue", position = "dodge") +
  geom_text(aes(label = round(correlation, 2)), vjust = -0.5, position = position_dodge(width = 0.7)) +
  ggtitle("Correlation Coefficients of Wealth Inequality vs. Union Density by Country") +
  xlab("Country") +
  ylab("Correlation Coefficient") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))

# Plotting the scatter of average Gini index, Trade union density, and collective brgaining over time
# First plot: Average Gini Index over time
ggplot(average_gini, aes(x = Year, y = Gini_Index)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Average Gini Index Over Time",
       x = "Year",
       y = "Average Gini Index") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))

# Second plot: Average Trade Union Density and Collective Bargaining Coverage over time
p <- ggplot() +
  geom_line(data = average_trade_union, aes(x = Year, y = TradeUnions_Density, color = "Trade Union Density"), size = 1) +
  geom_line(data = average_collective_bargaining, aes(x = Year, y = CollectiveBargain_Coverage, color = "Collective Bargaining Coverage"), size = 1) +
  labs(title = "Average Union Density and Bargaining Coverage Over Time",
       x = "Year",
       y = "Average",
       color = "Variable") +
  scale_color_manual(values = c("lightblue", "lightgreen"),
                     labels = c("Collective Bargaining Coverage", "Trade Union Density")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 16),  
        legend.text = element_text(family = "Times New Roman", size = 20),  
        legend.title = element_text(family = "Times New Roman", size = 20),
        plot.title = element_text(family = "Times New Roman", size = 24),
        axis.text.x = element_text(angle = 45, hjust = 1)) + # rotate x-axis labels
  ylim(20, 80)
print(p)

# Save the plot
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/averageovertime.png", plot = p, width = 10, height = 6)

j <- ggplot(dataset, aes(x = Year)) +
    geom_line(aes(y = WomenUnemployment_rate, color = "Women Unemployment Rate"), linetype = "solid") +
    geom_line(aes(y = TradeUnions_Density, color = "Trade Unions Density"), linetype = "dashed") +
    facet_wrap(~ Country, scales = "free_y", nrow = 5) +
    labs(title = "Women Unemployment Rate and Trade Unions Density Over Time",
         y = "Value",
         x = "Year",
         color = "Variable") +
    theme_minimal() +
    theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))
  
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/womenunemploy_density.png", plot = j, width = 10, height = 6)

#### 5.2 K-means clustering and plotting

# Perform k-means clustering
set.seed(123) # for reproducibility
kmeans_result <- kmeans(df[, c("TradeUnions_Density", "CollectiveBargain_Coverage")], 
                        centers = 3)

df$cluster <- as.factor(kmeans_result$cluster)

g <- ggplot(df, aes(x = TradeUnions_Density, y = CollectiveBargain_Coverage, color = cluster, label = paste(Country, Year, sep = " - "))) +
  geom_point(alpha = 0.7) +  
  geom_text(hjust = -0.2, vjust = -0.2, size = 1) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +  
  theme_minimal() +
  labs(
    title = "Scatter Plot of Trade Union Density vs. Collective Bargain Coverage with K-means Clustering",
    x = "Trade Union Density (%)",
    y = "Collective Bargain Coverage (%)",
    color = "Cluster"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 16),
    legend.position = "bottom",
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12)
  )
print(g)

ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/kmeanclustering_densityCoverage.png", plot = g, width = 10, height = 6)

df <- dataset[complete.cases(dataset[, c("TradeUnions_Density", "CollectiveBargain_Coverage")]), ]

# Ensure reproducibility
set.seed(12)

# Aggregating data by country to get average measures
country_avg_df <- df %>%
  group_by(Country) %>%
  summarize(
    avg_TradeUnions_Density = mean(TradeUnions_Density, na.rm = TRUE),
    avg_CollectiveBargain_Coverage = mean(CollectiveBargain_Coverage, na.rm = TRUE)
  )

# Perform k-means clustering on the aggregated data
kmeans_result <- kmeans(country_avg_df[, c("avg_TradeUnions_Density", "avg_CollectiveBargain_Coverage")], centers = 3)

# Add cluster labels to the aggregated dataframe
country_avg_df$cluster <- as.factor(kmeans_result$cluster)

cluster_labels <- c(
  "1" = "high density and high coverage",
  "2" = "low density and high coverage",
  "3" = "low density and low coverage"
)

# Scatter plot with k-means clustering
z <- ggplot(country_avg_df, aes(x = avg_TradeUnions_Density, y = avg_CollectiveBargain_Coverage, color = cluster, label = Country)) +
  geom_point(alpha = 0.9) +
  geom_text(hjust = -0.2, vjust = -0.2, size = 3) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", alpha = 0.2) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal() +
  labs(
    title = "Scatter Plot of Trade Union Density vs. Collective Bargain Coverage with K-means Clustering",
    x = "Average Trade Union Density",
    y = "Average Collective Bargain Coverage",
    color = "Cluster"
  ) +
  scale_color_manual(values = c("1" = "red", "2" = "blue", "3" = "green"), labels = cluster_labels) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Times New Roman", size = 16),
    legend.position = "bottom",
    text = element_text(family = "Times New Roman", size = 12),
    legend.text = element_text(family = "Times New Roman", size = 12),
    legend.title = element_text(family = "Times New Roman", size = 12)
  )

# Print the plot
print(z)
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/kmeanclustering_densityCoveragewithCountries.png", plot = z, width = 10, height = 6)

dataset$cluster <- NA
dataset$cluster[complete.cases(dataset[, c("TradeUnions_Density", "CollectiveBargain_Coverage")])] <- as.factor(kmeans_result$cluster)


dataset$cluster2 <- ifelse(dataset$cluster == 2, 1, 0)
dataset$cluster3 <- ifelse(dataset$cluster == 3, 1, 0)

# Check the new variable
table(df$cluster_binary)
table(dataset$cluster_binary)


#### 5.3 Plot the average values of demo, civsoc_particip, and rol over time

ggplot() +
  geom_line(data = average_civicsoc_particip, aes(x = Year, y = civsoc_particip, color = "Civic Participation"), size = 1) +
  geom_line(data = average_rol, aes(x = Year, y = rol, color = "Rule of Law"), size = 1) +
  labs(title = "Average Civic Participation and Rule of Law Over Time",
       x = "Year",
       y = "Average",
       color = "Variable") +
  scale_color_manual(values = c("blue", "green"),
                     labels = c("Civic Participation", "Rule of Law"),
                     name = "Variable") + 
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))


demo_density <- ggplot() +
  geom_line(data = average_norm_density, aes(x = Year, y = norm_density, color = "Density"), size = 1) +
  geom_line(data = average_norm_demo, aes(x = Year, y = norm_demo, color = "Democracy Index"), size = 1) +
  labs(title = "Average Normalized Density and Democracy Index Over Time",
       x = "Year",
       y = "Average",
       color = "Variable") +
  scale_color_manual(values = c("blue", "green"),
                     labels = c("Density", "Democracy Index")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),  
        legend.text = element_text(family = "Times New Roman", size = 12),  
        legend.title = element_text(family = "Times New Roman", size = 12),  
        plot.title = element_text(family = "Times New Roman", size = 16))
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/demo_density.png", plot = demo_density, width = 10, height = 6)
print(demo_density)

plot_fdi <- ggplot(dataset, aes(x = Year)) +
  geom_line(aes(y = FDI_inflow, color = "FDI Inflow")) +
  geom_line(aes(y = FDI_outflow, color = "FDI Outflow")) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "FDI Inflow and FDI Outflow Over Time by Country",
       y = "Value",
       x = "Year",
       color = "Indicator") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))

# Plot for GDP over time
plot_gdp <- ggplot(dataset, aes(x = Year)) +
  geom_line(aes(y = lnGDP, color = "lnGDP")) +
  facet_wrap(~ Country, scales = "free_y") +
  labs(title = "Logarithm of GDP Over Time by Country",
       y = "ln(GDP)",
       x = "Year",
       color = "Indicator") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))

# Save the plots
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/FDI.png", plot = plot_fdi, width = 10, height = 6)
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/GDP.png", plot = plot_gdp, width = 10, height = 6)

net <- ggplot(dataset, aes(x = Year)) +
  geom_line(aes(y = net_export, color = "Net Exports"), linetype = "solid") +
  geom_line(aes(y = net_FDI, color = "Net Foreign Direct Investments"), linetype = "dashed") +
  facet_wrap(~ Country, scales = "free_y", nrow = 5) +
  labs(title = "Net Exports and Net Foreign Direct Investments Over Time by Country",
       y = "Value",
       x = "Year",
       color = "Indicator") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "Times New Roman", size = 12),
        legend.text = element_text(family = "Times New Roman", size = 12),
        legend.title = element_text(family = "Times New Roman", size = 12),
        plot.title = element_text(family = "Times New Roman", size = 16))
ggsave(filename = "/Users/jacopobinati/Desktop/thesis/Images/netExportsFDI.png", plot = net, width = 10, height = 6)

## 5. Checking for Multicollinearity with Eigenvalue test and VIF

#### 5.1 Eigenvalue Test


# Check for multicollinearity using the eigenvalue test
correlation_matrix <- cor(dataset[, c("demo", "density2", "CollectiveBargain_Coverage","Inflation", "WageGrowth2", "Gov_debt", "ln_labor", "WomenUnemployment_rate", "net_export", "net_FDI", "Tax_contribution")])
eigenvalues <- eigen(correlation_matrix)$values
print(eigenvalues)

#### 5.2 VIF Test

# Check for multicollinearity using the VIF function
vif_values <- car::vif(lm(Gini_Index ~ demo + density2 + CollectiveBargain_Coverage + Inflation + WageGrowth2 + Gov_debt + ln_labor + WomenUnemployment_rate + net_export + net_FDI + Tax_contribution, data = dataset))

independent_vars <- c("demo", "unions", "Inflation", "WageGrowth2", "Gov_debt", "ln_labor", "WomenUnemployment_rate", "net_export", "net_FDI", "Tax_contribution")

vif_df <- data.frame(VIF = vif_values)
vif_df


independent_vars <- c("Democratic Governance", "Trade Unions Density", "Bargaining Coverage", "Inflation", "Wage Growth", "Government Debt (%)", "Log of Labor participation", "Women Unemployment Rate", "Net Export", "Net FDI", "Tax Contribution")

eigen_vif_df <- data.frame(Variables = independent_vars, Eigenvalue = eigenvalues, VIF = vif_values)

print(eigen_vif_df)

file_path <- "/Users/jacopobinati/Desktop/thesis/results/VIF_Eigenvalue.tex"

write.table(eigen_vif_df, file = file_path, sep = "\t", quote = FALSE, row.names = FALSE)

stargazer(eigen_vif_df, type = "latex", summary = FALSE, title = "Eigenvalues and VIF of Independent Variables",
          label = "tab:eigen_vif", out = "eigen_vif_table.tex")

variables <- dataset[, c("Gini_Index", "demo", "density2", "Inflation", "WageGrowth", 
                         "Gov_debt", "lnminwage", "lnGDP", 
                         "WomenUnemployment_rate", 
                         "Female_Workers","Parttime_employment", "net_export", "net_FDI")]

correlation_matrix <- cor(variables)
heatmap(correlation_matrix, 
        col = colorRampPalette(c("blue", "white", "red"))(100), 
        scale = "none", 
        main = "Correlation Heatmap") 

## 6. Model Building
library(AER)

library(readr)
df <- df %>%
  mutate(
    Country = as.factor(Country),
    Year = as.factor(Year)
  )
pdata <- pdata.frame(df, index = c("Country", "Year"))


# Creating first models for OLS, FE, and FD:
model_formulas <- c(
  "Gini_Index ~ density2 + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate",
  
  "Gini_Index ~ density2 + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export",
  
  "Gini_Index ~ density2 + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + net_FDI + Tax_contribution"
)

create_models <- function(data, type, formulas) {
  lapply(formulas, function(formula) {
    formula <- as.formula(formula)
    if (type == "OLS") {
      return(lm(formula, data = dataset))
    } else if (type == "FE") {
      return(plm(formula, data = dataset, model = "within", effect = "twoways"))
    } else if (type == "FD") {
      return(plm(formula, data = dataset, model = "fd"))
    } else {
      stop("Unknown model type")
    }
  })
}

ols_models <- create_models(df, "OLS", model_formulas)
fe_models <- create_models(pdata, "FE", model_formulas)
fd_models <- create_models(pdata, "FD", model_formulas)

stargazer(fe_models, type = "text", title = "Fixed Effects Regression Models")
stargazer(ols_models, type = "text", title = "OLS Regression Models")

FE_summary <- summary(fe_models)
FE_summary <- xtable(FE_summary)

file_path_FE <- "/Users/jacopobinati/Desktop/thesis/results/FE_table.tex"

print(FE_summary, file = file_path_FE)

## 6.1 Two-Ways Fixed Effect with Instrumental Variable

library(Matrix)
library(AER)
library(lfe)


#### 6.1.1. Endogeneity Test


library(lmtest)
library(zoo)
# First-stage regression: Regress the endogenous variable on the instruments and exogenous variables
first_stage <- lm(density2 ~ demo + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate, data = dataset)
first_stage_summary <- summary(first_stage)

# Extract fitted values from the first-stage regression
fitted_values_density2 <- fitted(first_stage)
# Check correlation between residuals and instruments for exogeneity
first_stage_residuals <- resid(first_stage)
correlation_exogeneity <- cor(first_stage_residuals, dataset$demo)
print(paste("Correlation between demo and residuals:", correlation_exogeneity))

# Second-stage regression: Regress the dependent variable on the fitted values from the first stage and other exogenous variables
second_stage <- lm(Gini_Index ~ fitted_values_density2 + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate, data = dataset)
summary(second_stage)

second_stage_summary <- summary(second_stage)

# Exclusion Restriction: Check if demo is not statistically significant in the second-stage regression
summary(second_stage)
stargazer(first_stage, type= "text", title = "First Stage 2SLS")
stargazer(second_stage, type = "text", title = "Second Stage 2SLS")


# Check the class of first_stage
print(class(first_stage))

# Print the summary of the model to inspect available attributes
print(summary(first_stage))

# If needed, calculate R-squared and Adjusted R-squared manually
r_squared <- summary(first_stage)$r.squared
adj_r_squared <- summary(first_stage)$adj.r.squared

print(r_squared)
print(adj_r_squared)

file_path_first_stage <- "/Users/jacopobinati/Desktop/thesis/results/first_stage_table.tex"

r_squared <- summary(first_stage)$r.squared
adj_r_squared <- summary(first_stage)$adj.r.squared

# Create the stargazer table
stargazer(
  first_stage, 
  type = "latex", 
  out = file_path_first_stage, 
  title = "First-Stage Regression Results",
  covariate.labels = c("Demo", "Collective Bargain Coverage", "Inflation", "Wage Growth", 
                       "Government Debt", "Log Labor Workforce", "Women Unemployment Rate"),
  omit = c("factor\\(Country\\)", "factor\\(Year\\)"), 
  omit.labels = c("Country", "Year"), 
  star.cutoffs = c(0.1, 0.05, 0.01),
  no.space = TRUE, 
  add.lines = list(
    c("Observations", nobs(first_stage)),
    c("R-squared", round(r_squared, 3)),
    c("Adjusted R-squared", round(adj_r_squared, 3)),
    c("Residual Std. Error", round(summary(first_stage)$sigma, 3)),
    c("Correlation between demo and residuals", round(correlation_exogeneity, 3))
  )
)



file_path_second_stage <- "/Users/jacopobinati/Desktop/thesis/results/second_stage_table.tex"

stargazer(
  second_stage, 
  type = "latex", 
  out = file_path_second_stage, 
  title = "Second-Stage Regression Results",
  covariate.labels = c("Fitted Density Squared", "Collective Bargain Coverage", "Inflation", "Wage Growth", "Government Debt", "Log Labor Workforce", "Women Unemployment Rate"),
  omit = c("factor\\(Country\\)", "factor\\(Year\\)"), 
  omit.labels = c("Country", "Year"), 
  star.cutoffs = c(0.1, 0.05, 0.01),
  no.space = TRUE, 
  add.lines = list(
    c("Observations", nobs(second_stage)),
    c("R-squared", round(second_stage_summary$r.squared, 3)),
    c("Adjusted R-squared", round(second_stage_summary$adj.r.squared, 3)),
    c("Residual Std. Error", round(second_stage_summary$sigma, 3))
  )
)


library(lmtest) # For the coeftest function

# Run the OLS regression
ols_model <- lm(Gini_Index ~ density2 + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate + net_export, data = dataset)

# Run the IV regression (2SLS)
iv_model <- ivreg(Gini_Index ~ density2 + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate + net_export | 
                  demo + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate + net_export, data = dataset)

# Extract the coefficients and variance-covariance matrices
coef_ols <- coef(ols_model)
vcov_ols <- vcov(ols_model)
coef_iv <- coef(iv_model)
vcov_iv <- vcov(iv_model)

# Perform the Hausman test
hausman_test <- function(coef_ols, coef_iv, vcov_ols, vcov_iv) {
  diff_coef <- coef_ols - coef_iv
  diff_vcov <- vcov_ols - vcov_iv
  test_statistic <- t(diff_coef) %*% solve(diff_vcov) %*% diff_coef
  p_value <- pchisq(test_statistic, df = length(diff_coef), lower.tail = FALSE)
  return(list(test_statistic = as.numeric(test_statistic), p_value = p_value))
}

# Run the test
dwh_result <- hausman_test(coef_ols, coef_iv, vcov_ols, vcov_iv)
print(paste("Hausman test statistic:", dwh_result$test_statistic))
print(paste("p-value:", dwh_result$p_value))

# Interpretation
if (dwh_result$p_value < 0.05) {
  print("Significant differences between OLS and IV coefficients suggest endogeneity.")
} else {
  print("No significant differences between OLS and IV coefficients suggest no endogeneity.")
}

library(stargazer)
library(AER)
library(sandwich)
library(survival)
model4_formula <- ivreg(Gini_Index ~ density2 + CollectiveBargain_Coverage + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + factor(Country) + factor(Year) | TradeUnions_Density + demo + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + factor(Country) + factor(Year), data = dataset)

model5_formula <- ivreg(Gini_Index ~ density2 + CollectiveBargain_Coverage + density2*cluster2 + density2*cluster3 +Inflation + WageGrowth + lnminwage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + factor(Country) + factor(Year) | TradeUnions_Density + demo + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + factor(Country) + factor(Year), data = dataset)

model6_formula <- ivreg(Gini_Index ~ density2 + CollectiveBargain_Coverage + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + net_FDI + Tax_contribution + factor(Country) + factor(Year) | TradeUnions_Density + demo + density2*cluster2 + density2*cluster3 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + net_FDI + Tax_contribution + factor(Country) + factor(Year), data = dataset)

# Estimate the models
model4 <- summary(model4_formula)
model5 <- summary(model5_formula)
model6 <- summary(model6_formula)
stargazer(model4_formula, model5_formula, model6_formula, type = "text")



stargazer(model4_formula, model5_formula, model6_formula, type = "text")

# Define the file path
file_path_models <- "/Users/jacopobinati/Desktop/thesis/results/models_table.tex"

# Generate LaTeX table using stargazer
stargazer(
  model4_formula, model5_formula, model6_formula, 
  type = "latex", 
  out = file_path_models, 
  title = "Regression Results",
  covariate.labels = c("Density Squared", "Collective Bargain Coverage", 
                       "Wage Growth", "Log Minimum Wage", "Female Workers", "Log GDP"),
  omit = c("factor\\(Country\\)", "factor\\(Year\\)", "Gov_debt", "ln_labor", 
           "Parttime_employment", "WomenUnemployment_rate", "net_export", "net_FDI", "Tax_contribution"), 
  omit.labels = c("Country", "Year", "Inflation", "Government Debt", "Log Labor", 
                  "Part-time Employment", "Women Unemployment Rate", 
                  "Net Export", "Net FDI", "Tax Contribution"), 
  star.cutoffs = c(0.1, 0.05, 0.01),
  no.space = TRUE, 
  add.lines = list(
    c("Observations", length(model4_formula$residuals), length(model5_formula$residuals), length(model6_formula$residuals)),
    c("R-squared", round(summary(model4_formula)$r.squared, 3), round(summary(model5_formula)$r.squared, 3), round(summary(model6_formula)$r.squared, 3)),
    c("Adjusted R-squared", round(summary(model4_formula)$adj.r.squared, 3), round(summary(model5_formula)$adj.r.squared, 3), round(summary(model6_formula)$adj.r.squared, 3)),
    c("Residual Std. Error", round(summary(model4_formula)$sigma, 3), round(summary(model5_formula)$sigma, 3), round(summary(model6_formula)$sigma, 3))
  )
)

model_ols1 <- lm(Gini_Index ~ TradeUnions_Density + CollectiveBargain_Coverage + TradeUnions_Density*cluster2 + TradeUnions_Density*cluster3 + Inflation + WageGrowth + lnminwage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate, data = dataset)

model_ols2 <- lm(Gini_Index ~ TradeUnions_Density + CollectiveBargain_Coverage + TradeUnions_Density*cluster2 + TradeUnions_Density*cluster3 +Inflation + WageGrowth + lnminwage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export, data = dataset)

model_ols3 <- lm(Gini_Index ~ TradeUnions_Density + CollectiveBargain_Coverage + TradeUnions_Density*cluster2 + TradeUnions_Density*cluster3 + Inflation + WageGrowth + lnminwage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + net_export + net_FDI + Tax_contribution, data = dataset)

# Estimate the models
modelols1 <- summary(model_ols1)
modelols2 <- summary(model_ols2)
modelols3 <- summary(model_ols3)
stargazer(model_ols1, model_ols2, model_ols3, type = "text")
file_path_ols <- "/Users/jacopobinati/Desktop/thesis/results/ols_models_table.tex"

# Create the LaTeX table with stargazer
stargazer(
  model_ols1, model_ols2, model_ols3, 
  type = "latex", 
  out = file_path_ols, 
  title = "OLS Regression Results",
   covariate.labels = c("Density Squared", "Collective Bargain Coverage", 
                       "Wage Growth", "Log Minimum Wage", "Female Workers", "Log GDP"),
  omit = c("factor\\(Country\\)", "factor\\(Year\\)", "Inflation", "Gov_debt", "ln_labor", 
           "Parttime_employment", "WomenUnemployment_rate", "net_export", "net_FDI", "Tax_contribution"), 
  omit.labels = c("Country", "Year", "Inflation", "Government Debt", "Log Labor", 
                  "Part-time Employment", "Women Unemployment Rate", 
                  "Net Export", "Net FDI", "Tax Contribution"), 
  omit.labels = c("Country", "Year"), 
  star.cutoffs = c(0.1, 0.05, 0.01),
  no.space = TRUE, 
  add.lines = list(
    c("Observations", nobs(model_ols1), nobs(model_ols2), nobs(model_ols3)),
    c("R-squared", round(summary(model_ols1)$r.squared, 3), round(summary(model_ols2)$r.squared, 3), round(summary(model_ols3)$r.squared, 3)),
    c("Adjusted R-squared", round(summary(model_ols1)$adj.r.squared, 3), round(summary(model_ols2)$adj.r.squared, 3), round(summary(model_ols3)$adj.r.squared, 3)),
    c("Residual Std. Error", round(summary(model_ols1)$sigma, 3), round(summary(model_ols2)$sigma, 3), round(summary(model_ols3)$sigma, 3))
  )
)
