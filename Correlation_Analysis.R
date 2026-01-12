#I am analysing how multipe outcome indicators of weatherization risk relate to 
#economic, housing and rental conditions. 

#Heating risk/ weatherisation indicators: 

#Total_Heating_Violations - Historical Burden
#Open Violations - current risk
#violaitons_Per_address - Intensity 
#historical_risk_score - past composite risk
#current_risk_score - current composite risk 

#Explanatory Variables: 

#Economic Vulnerability: 
#Median_household_income, poverty_rate_pct, snap_households_pct

#Structural Info: 
#pct_housing_pre_1980, pct_housing_pre_1960

#Rental_Conditions: 
#rr_invalid_rate, rental_risk_score.


library(dplyr)

corr_df <- weatherization_full %>%
  filter(
    Neighborhood != "Unknown",
    !is.na(median_household_income)  # removes Franklin Square + Hawley Green (no census coverage)
  ) %>%
  select(
    # Heating / risk outcomes
    total_heating_violations,
    open_violations,
    open_violation_pct,
    violations_per_address,
    historical_risk_score,
    current_risk_score,
    
    # Rental factors
    rr_invalid_rate,
    rental_properties,
    rental_risk_score,
    
    # Census factors
    neighborhood_population,
    median_household_income,
    poverty_rate_pct,
    snap_households_pct,
    pct_housing_pre_1980,
    pct_housing_pre_1960
  )


dim(corr_df)

#sapply is equivalent to lambda function in Python: 
#so here we are taking the corr_df data frame and we are applying the function
#to find null values to each and every column. 


sapply(corr_df, function(x) sum(is.na(x)))

#STEP 2: Computing the correlation Matrix

cor_matrix <- cor(
  corr_df,
  use = "pairwise.complete.obs",
  method = "spearman"
)

#sanity check: 
round(cor_matrix[1:6, 10:15], 2)

corr_table <- cor_matrix %>%
round(2) %>%
  as.data.frame()

view(corr_table)


corr_focus <- cor_matrix[
  c("total_heating_violations",
    "open_violations",
    "open_violation_pct",
    "violations_per_address",
    "historical_risk_score",
    "current_risk_score"),
  c("median_household_income",
    "poverty_rate_pct",
    "snap_households_pct",
    "pct_housing_pre_1960",
    "pct_housing_pre_1980",
    "rr_invalid_rate",
    "rental_properties",
    "rental_risk_score",
    "neighborhood_population")
] %>%
  round(2)

View(corr_focus)



#visualising the correlation_matrix

library(tidyverse)

#it turns matrix into rows.
cor_long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(
    cols = -var1,
    names_to = "var2",
    values_to = "correlation"
  )

#Plot the heatmap: 

ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  ) +
  labs(
    title = "Correlation Matrix: Heating Risk, Rental, and Census Factors",
    fill = "Spearman\nCorrelation"
  )



#How did we calculate rental_risk_score: 

#rental_concentration + invalid_rentals - so we scaled it and then took an avergae 
#of both to create a rental_risk_score

#Whats the one key insights from this project? One insight that you found interesting? 

#One key insights from the project is that the feature "rental_risk_score" (rental compliance)
#explains the heating violations more strongly than individual socio-economic features
#indicating how housing governance and maintainence conditions are more immediate drivers of weatherization risks
#rather than demographics alone

#One more key insights I found is that SNAP participation shows a stronger 
#association with heating violations than poverty_pct that makes sense 
#because SNAP_pct - households which are actually dependent on support are more 
#vulnerable - it captures economic and lived vulnerability better than 
#poverty_pct because poverty_pct is just binary ( it is above or below)
#does not capture other factors like unstable_incomes, cost of living..etc

#Energy insecurity actually overlaps with SNAP reliance. 
------------------------
#Now moving on to creating a composite weatherisation score: 
  
#First, I am creating a score using census features:
#We don’t weight variables using correlation coefficients because 
#correlation does not represent causal importance — it is sample-dependent, 
#sensitive to outliers, and would introduce circular reasoning if 
#used to construct a score that’s later evaluated against the same outcome. 
#Instead, we select variables based on theory and robustness, normalize them 
#to a common scale, and combine them with equal weights 
#for interpretability and stability.
  

#Scaling:
weatherization_full <- weatherization_full %>%
  mutate(
    snap_scaled        = scale_minmax(snap_households_pct),
    poverty_scaled     = scale_minmax(poverty_rate_pct),
    housing_age_scaled = scale_minmax(pct_housing_pre_1960)
 )


summary(weatherization_full %>%
          select(snap_scaled, poverty_scaled, housing_age_scaled))


#Creating a vulnerability score: 

weatherization_full <- weatherization_full %>%
  mutate(
    census_vulnerability_score = rowMeans(
      select(., snap_scaled, poverty_scaled, housing_age_scaled),
      na.rm = TRUE
    )
  )

summary(weatherization_full$census_vulnerability_score)

#So I intentionally did not combine all the three layers of scores into one
#That is because I feel they are three different dimensions 
#Heating_Violations, Rental_compliance and census_features represent three 
#different mechanisms of weatherization.

view(weatherization_full)


