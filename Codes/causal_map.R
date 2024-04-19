# Load the igraph library
library(igraph)
library(readxl)
df <- read_excel('/Users/jacopobinati/Desktop/thesis/Datasets/Data Finale_balanced1.xlsx')

# Define the nodes with categories
nodes_with_categories <- data.frame(
  name = c('Collective Bargain Coverage', 'Trade Unions Density', 'Gini Index', 
           'Real Wage Growth', 'Annual Inflation', 
           'Exports of goods and services', 
           'Foreign direct investment, net inflows', 
           'Foreign direct investment, net outflows', 
           'Imports of goods and services', 
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

# Define the edges
edges <- matrix(c('Trade Unions Density', 'Gini Index',
                  'Collective Bargain Coverage', 'Trade Unions Density',
                  'Real Wage Growth', 'Trade Unions Density',
                  'Annual Inflation', 'Real Wage Growth',
                  'Annual Inflation', 'Labor tax and contributions',
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
                  'Exports of goods and services', 'GDP',
                  'Foreign direct investment, net inflows', 'GDP',
                  'Foreign direct investment, net outflows', 'GDP',
                  'Imports of goods and services', 'GDP'), 
                byrow = TRUE, ncol = 2)

# Create the graph
G <- graph_from_edgelist(edges, directed = TRUE)

# Add node attributes
V(G)$category <- nodes_with_categories$category[match(V(G)$name, nodes_with_categories$name)]

# Plotting
plot(G, seed=42, vertex.size=25, vertex.label=V(G)$name, vertex.color="lightblue", 
     main="Causal Map: Labour Unions and Income Inequality",
     edge.arrow.size=.8, vertex.label.cex=0.5, vertex.label.family="Times",
     layout=layout_with_fr(G, area=vcount(G)^3, repulserad=vcount(G)^3))


########### NEW GRAPH EASY TO READ ###########
edges <- matrix(
  c('Trade Unions Density', 'Gini Index',
    'Confounders', 'Trade Unions Density',
    'Confounders', 'Gini Index'),
  ncol = 2, byrow = TRUE
)

# Create the graph
P <- graph_from_edgelist(edges, directed = TRUE)

# Define the layout
# Note: igraph does not have a shell layout, but you can use a circular layout as a simple alternative
layout <- layout_in_circle(P)

# Plot the graph
plot(P, layout = layout, vertex.size=20, vertex.label=V(P)$name, vertex.color='skyblue', 
     edge.arrow.size=1, main="Causal Map", font.main=2, cex.main=1.5,
     vertex.label.cex=1, vertex.label.family="Times", vertex.label.color="black",
     margin=-0)



###########
library(dplyr)
library(tibble)

# Assuming df is your data frame
df <- as_tibble(df) # Convert to tibble for nicer printing, if needed

# Rename columns
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

# Reset index (In R, this usually means making sure row names are sequential)
rownames(df) <- NULL

# let's change the name to some variables
print(names(df))
df <- df %>%
  mutate(lnGDP = round(log(GDP), 2))
# Create a binary column 'minwage' where 1 indicates there's at least one positive value in 'Monthly Minimum Wage'
# and 0 otherwise
df <- df %>%
  mutate(minwage = ifelse(Monthly_Minimum_Wage > 0, 1, 0))
df <- df %>%
  mutate(lnminwage = ifelse(Monthly_Minimum_Wage > 0, round(log(Monthly_Minimum_Wage), 2), 0))
df <- df %>%
  mutate(ln_labor = ifelse(Labor_force > 0, round(log(Labor_force), 2), 0))
# print Minimum_Monthly_Wage, minwage and lnminwage
print(df %>%
        select(Monthly_Minimum_Wage, minwage, lnminwage),
      n = Inf)

# create a new variable bargain
df <- df %>%
  mutate(bargain = TradeUnions_Density/CollectiveBargain_Coverage)
df <- df %>%
  mutate(bargain1 = sqrt(TradeUnions_Density) * log(CollectiveBargain_Coverage))
df$Year <- as.numeric(as.character(df$Year))

# First, pivot the data to a long format
df_long <- df %>%
  pivot_longer(cols = c(Gini_Index, TradeUnions_Density), names_to = "Indicator", values_to = "Value")

# Check for missing values
sapply(df, function(x) sum(is.na(x)) / length(x))

first_year_positive_lnminwage <- df %>%
  filter(lnminwage > 0) %>%
  group_by(Country) %>%
  summarize(FirstYear = min(Year))

library(tidyr)

########### IMAGES ############
library(ggplot2)

# Now, create the plot with ggplot2
ggplot(df_long, aes(x = Year, y = Value, color = Indicator, group = Indicator)) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y") + # Use 'scales = "free_y"' to allow each panel (country) to have its own y scale
  theme_minimal() +
  labs(title = "Evolution of Gini Index and Trade Unions Density by Country",
       y = "Value",
       x = "Year",
       color = "Indicator") +
  theme(legend.position = "bottom")

#######################################
# Create the plot with ggplot2
ggplot(df_long, aes(x = Year, y = Value, color = Indicator, group = interaction(Indicator, Country))) +
  geom_line() +
  facet_wrap(~ Country, scales = "free_y") + # Allows each country panel to have its own y scale
  geom_vline(data = df_long %>% filter(!is.na(YearIntroduced)), aes(xintercept = YearIntroduced), linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Evolution of Gini Index and Trade Unions Density by Country",
       y = "Value",
       x = "Year",
       color = "Indicator") +
  theme(legend.position = "bottom")

###########################

ggplot(df, aes(x = Year, y = WageGrowth, group = Country)) +  # Add 'group = Country' to ensure lines are drawn correctly
  geom_line() +  # Draws lines connecting the data points for each country
  facet_wrap(~ Country, scales = "free_y") +  # Creates a separate plot for each country with its own y scale
  theme_minimal() +  # Applies a minimal theme to the plot
  labs(title = "Evolution of Wage Growth by Country",  # Corrects the title to reflect wage growth evolution
       y = "Wage Growth",  # Corrects the y-axis label to reflect it's plotting wage growth
       x = "Year") +  # Labels the x-axis
  theme(legend.position = "none") # Since 'group = Country' is used, and countries are faceted, a legend is unnecessary

# Plotting the histogram for Gini_Index
ggplot(df, aes(x = Gini_Index)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "lightblue") + # You may adjust binwidth as necessary
  ggtitle("Histogram of Gini Index") +
  xlab("Gini Index") +
  ylab("Frequency") +
  theme_minimal()

# Plotting the histogram for TradeUnions_Density
ggplot(df, aes(x = TradeUnions_Density)) +
  geom_histogram(binwidth = 2, fill = "green", color = "lightblue") + # Adjust binwidth as necessary
  ggtitle("Histogram of Trade Unions Density") +
  xlab("Trade Unions Density") +
  ylab("Frequency") +
  theme_minimal()

# Scatter plot of Trade Union Density vs. Collective Bargain Coverage
ggplot(df, aes(x = TradeUnions_Density, y = CollectiveBargain_Coverage)) +
  geom_point(aes(color = Country), alpha = 1) +  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +  
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +  # Set axis limits to 0-100
  theme_minimal() +
  labs(
    title = "Scatter Plot of Trade Union Density vs. Collective Bargain Coverage",
    x = "Trade Union Density (%)",
    y = "Collective Bargain Coverage (%)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )
############ MODEL BUILDING with lm() ############
#############################################################
library(stargazer)
library(plm)
library(dplyr)

# Assuming 'df' is already loaded from an Excel file and ready to use
# Make sure 'Country' and 'Year' are factors for panel data analyses
df <- df %>%
  mutate(
    Country = as.factor(Country),
    Year = as.factor(Year)
  )

# Convert the data frame to a pdata.frame for plm (panel data operations)
pdata <- pdata.frame(df, index = c("Country", "Year"))

# Define model formulas as strings for reusability in different model types
model_formulas <- c(
  "Gini_Index ~ TradeUnions_Density + bargain1 + Inflation + WageGrowth + lnminwage",
  "Gini_Index ~ TradeUnions_Density + bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage",
  "Gini_Index ~ TradeUnions_Density + bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt",
  "Gini_Index ~ TradeUnions_Density + bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate",
  "Gini_Index ~ TradeUnions_Density + bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth",
  "Gini_Index ~ TradeUnions_Density + bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth + FDI_inflow + FDI_outflow + Tax_contribution"
)

# Function to create models of a specific type
create_models <- function(data, type, formulas) {
  lapply(formulas, function(formula) {
    formula <- as.formula(formula)
    if (type == "OLS") {
      return(lm(formula, data = data))
    } else if (type == "FE") {
      return(plm(formula, data = data, model = "within"))
    } else if (type == "FD") {
      return(plm(formula, data = data, model = "fd"))
    } else {
      stop("Unknown model type")
    }
  })
}

# Create OLS, FE, and FD models
ols_models <- create_models(df, "OLS", model_formulas)
fe_models <- create_models(pdata, "FE", model_formulas)
fd_models <- create_models(pdata, "FD", model_formulas)

# Summarize the models
stargazer(ols_models, type = "text", title = "OLS Regression Models")
stargazer(fe_models[c(4, 5, 6)], type = "text", title = "Fixed Effects Regression Models")
stargazer(fd_models, type = "text", title = "First Differences Regression Models")

##### BUILDING WITH IV. THE DATA ARE GATHERED FROM THE WEBSITE "https://ourworldindata.org/grapher/civil-society-participation-index" and "https://ourworldindata.org/grapher/rule-of-law-index?tab=chart&time=1990..latest"
# IN THIS CASE WE USE INTERACTION BETWEEN ROL AND CIVIC PARTICIPATION AS IV. 

library(plm)
library(AER)
library(dplyr)
library(readr)
# WE ASSUME THAT CIVIC PARTICIPATION IS EXOGENOUS TO GINI INDEX
rol <- read_csv("/Users/jacopobinati/Desktop/thesis/Datasets/rule-of-law-index.csv")
part <- read_csv("/Users/jacopobinati/Desktop/thesis/Datasets/civil-society-participation-index.csv")

# Specify countries to keep
countries_to_keep <- c('Austria', 'Belgium', 'Czechia', 'Denmark', 'Estonia', 'Finland', 
                       'France', 'Germany', 'Greece', 'Hungary', 'Iceland', 'Ireland', 
                       'Italy', 'Lithuania', 'Luxemburg', 'Netherlands', 'Norway', 'Poland', 
                       'Portugal', 'Slovak Republic', 'Spain', 'Sweden', 'Switzerland', 
                       'United Kingdom')

# Filter rol and part data
rol <- rol %>% 
  filter(Year >= 1990, Entity %in% countries_to_keep)

part <- part %>%
  filter(Year >= 1990, Entity %in% countries_to_keep)

# Drop columns
columns_to_drop1 <- c('civsoc_particip_vdem_low_owid', 'civsoc_particip_vdem_high_owid')
columns_to_drop2 <- c('rule_of_law_vdem_low_owid', 'rule_of_law_vdem_high_owid')

rol <- select(rol, -one_of(columns_to_drop2))
part <- select(part, -one_of(columns_to_drop1))

# Rename columns
rol <- rename(rol, rol = rule_of_law_vdem_owid, Country = Entity)
part <- rename(part, civsoc_particip = civsoc_particip_vdem_owid, Country = Entity)

# Assuming 'df' is your main dataframe and it has 'Country' and 'Year' columns
df <- merge(df, rol, by = c("Country", "Year"), all.x = TRUE)
df <- merge(df, part, by = c("Country", "Year"), all.x = TRUE)

# Drop unnecessary columns
dropdf <- c('Code.x', 'Code.y')
df <- select(df, -one_of(dropdf))

# Create a new column 'demo'
df$demo <- df$civsoc_particip * df$rol
# create a copy of df and call it dataset
dataset <- df

# Check for missing values in the dataset
missing_values <- is.na(dataset)
missing_values_summary <- colSums(missing_values)
print(missing_values_summary)
dataset <- dataset %>%
  filter(!Country %in% c("Slovak Republic", "Luxembourg"))
####### BUILDING THE MODELS
# Ensure 'Year' and 'Country' are correctly formatted
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
##########################################################

library(lfe)
library(lmtest)
first_stage <- lm(bargain1 ~ demo + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate, data = dataset)
summary(first_stage)

dataset$bargain1_fitted <- predict(first_stage, newdata = dataset)
# Perform the second stage of the IV regression
second_stage <- lm(Gini_Index ~ bargain1_fitted + CollectiveBargain_Coverage + Inflation + WageGrowth + Gov_debt + ln_labor + WomenUnemployment_rate, data = dataset)
# Display the summary of the second-stage regression
summary(second_stage)

## IV REGRESSION
library(AER)

# Define the model formulas
model4_formula <- ivreg(Gini_Index ~ bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate | TradeUnions_Density + demo + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate, data = dataset)

model5_formula <- ivreg(Gini_Index ~ bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth | TradeUnions_Density + demo + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth, data = dataset)

model6_formula <- ivreg(Gini_Index ~ bargain1 + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth + FDI_inflow + FDI_outflow + Tax_contribution | TradeUnions_Density + demo + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth + FDI_inflow + FDI_outflow + Tax_contribution, data = dataset)

# Estimate the models
model4 <- summary(model4_formula)
model5 <- summary(model5_formula)
model6 <- summary(model6_formula)

# Display the summaries
print(model4)
print(model5)
print(model6)
stargazer(model4_formula, model5_formula, model6_formula, type="text")



