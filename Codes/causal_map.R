# Load the igraph library
library(igraph)
library(readxl)
df <- read_excel('/Users/jacopobinati/Desktop/thesis/Datasets/Data Finale_balanced1.xlsx')
# Define the nodes and their categories (not used directly in igraph, but useful for context)
nodes_with_categories <- list(
  'Collective Bargain Coverage' = 'Collective Bargaining',
  'Trade Unions Density' = 'Collective Bargaining',
  'Gini Index' = 'Income Inequality',
  'Real Average Annual Wage Growth' = 'Economic',
  'Annual Inflation CPI' = 'Economic',
  'Exports of goods and services (annual % growth)' = 'Economic',
  'Foreign direct investment, net inflows (% of GDP)' = 'Economic',
  'Foreign direct investment, net outflows (% of GDP)' = 'Economic',
  'Imports of goods and services (% of GDP)' = 'Economic',
  'Current health expenditure (% of GDP)' = 'Economic',
  'Central government debt, total (% of GDP)' = 'Economic',
  'GDP (current US$)' = 'Economic',
  'Labour force participation rate (Women age 15 to 64)' = 'Labor Market',
  'Unemployment rate (Women age 15 to 64)' = 'Labor Market',
  'Labor force, total' = 'Labor Market',
  'Labor tax and contributions (% of commercial profits)' = 'Labor Market',
  'Part time employment, total (% of total employment)' = 'Labor Market',
  'Monthly Minimum Wage' = 'Labor Market'
)

# Define the edges
edges <- matrix(c(
  'Trade Unions Density', 'Gini Index',
  'Collective Bargain Coverage', 'Trade Unions Density',
  
  'Real Average Annual Wage Growth', 'Trade Unions Density',
  
  'Annual Inflation CPI', 'Real Average Annual Wage Growth',
  'Annual Inflation CPI', 'Labor tax and contributions (% of commercial profits)',
  
  'Labor tax and contributions (% of commercial profits)', 'GDP (current US$)',
  'Labor tax and contributions (% of commercial profits)', 'Labor force, total',
  'Part time employment, total (% of total employment)', 'Labor force, total',
  'Labour force participation rate (Women age 15 to 64)', 'Labor force, total',
  'Unemployment rate (Women age 15 to 64)', 'Labor force, total',
  
  'Labor force, total', 'Real Average Annual Wage Growth',
  'Labor force, total', 'Annual Inflation CPI',
  'Labor force, total', 'Collective Bargain Coverage',
  'Labor force, total', 'GDP (current US$)',
  'GDP (current US$)', 'Real Average Annual Wage Growth',
  'Exports of goods and services (annual % growth)', 'GDP (current US$)',
  'Foreign direct investment, net inflows (% of GDP)', 'GDP (current US$)',
  'Foreign direct investment, net outflows (% of GDP)', 'GDP (current US$)',
  'Imports of goods and services (% of GDP)', 'GDP (current US$)'
), byrow=TRUE, ncol=2)

# Create the graph
G <- graph_from_edgelist(edges, directed=TRUE)
layout <- layout_with_fr(G, area=vcount(G)^3, repulserad=vcount(G)^3)

png("causal_map_collective_bargaining_and_income_inequality.png", width=2000, height=1500, res=150)

plot(G,
     layout=layout,
     layout=layout_with_fr(G), # This layout algorithm tends to spread nodes more evenly
     vertex.size=20, # Larger vertex size
     vertex.label.cex=.4, # Larger font size for labels
     edge.arrow.size=0.5, # Slightly larger arrows
     main="Causal Map: Collective Bargaining and Income Inequality")

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

# Check for missing values
sapply(df, function(x) sum(is.na(x)) / length(x))

########### IMAGES ############
library(ggplot2)
library(tidyr)

# Assuming df is your original dataframe with 'Country', 'Year', 'Gini_Index', and 'TradeUnions_Density'
# First, pivot the data to a long format
df_long <- df %>%
  pivot_longer(cols = c(Gini_Index, TradeUnions_Density), names_to = "Indicator", values_to = "Value")

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

df$Year <- as.numeric(as.character(df$Year))

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
  "Gini_Index ~ TradeUnions_Density + Inflation + WageGrowth + lnminwage",
  "Gini_Index ~ TradeUnions_Density + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage",
  "Gini_Index ~ TradeUnions_Density + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + Health_expenditure",
  "Gini_Index ~ TradeUnions_Density + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + Health_expenditure + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate",
  "Gini_Index ~ TradeUnions_Density + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + Health_expenditure + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth",
  "Gini_Index ~ TradeUnions_Density + Inflation + WageGrowth + lnminwage + CollectiveBargain_Coverage + lnGDP + Gov_debt + Health_expenditure + ln_labor + Female_Workers + Parttime_employment + WomenUnemployment_rate + Exports_growth + Imports_growth + FDI_inflow + FDI_outflow + Tax_contribution"
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
stargazer(fe_models, type = "text", title = "Fixed Effects Regression Models")
stargazer(fd_models, type = "text", title = "First Differences Regression Models")


