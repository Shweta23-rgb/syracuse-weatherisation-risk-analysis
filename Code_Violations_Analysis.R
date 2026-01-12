library(tidyverse)
my_data <- read_csv("/Users/saiswethalakkoju/Downloads/Open_Data_Project/Code_Violations_V2.csv")

# View full dataset in viewer
View(my_data)

# Quick look at the data
glimpse(my_data)

# Or
head(my_data)

#Just look at the violation column
table(my_data$violation)

#Lookin at the complaint types across the dataset:
library(dplyr)
my_data %>% 
  count(complaint_type_name, sort = TRUE)

# Searching for heating-related keywords in the violation column
heating_keywords <- c("heat", "heating", "furnace", "boiler", 
                      "frozen", "cold", "temperature", "hvac")


#creating a filter for heating related vioaltions
heating_violations <- my_data %>%
  filter(grepl(paste(heating_keywords, collapse = "|"), 
               violation, 
               ignore.case = TRUE))

heating_violations %>%
  count(violation, sort = TRUE) %>%
  head(20)

#Counting the heating violations
nrow(heating_violations)

#What I have done so far is like a broad filter - 
#This may contain some false positives - 
#Like refrigerator temperature ( Not at all related to the heating issues, hot water(Maybe or maybe not a heating violation here, correct?))

#So to avoid false positives - Lets build a more precise filter here:

heating_violations %>%
  count(violation, sort = TRUE) %>%
  head(20)

#From above, I found there are mostly two types of violations: 
#Space Violations - Heating Supply, Unsafe Heating appliances etc
#Water Heating Facilities - Does lack of water heating facilities indicate winter hardship : Yes and No - and to not loose important data I am including the water heating problems as well
#but, as a secondary category.  I am going to seperate two issues. 


space_heating_keywords <- c(
  "heat supply",
  "heating appliance",
  "furnace",
  "room temperature",
  "unvented heater"
)

water_heating_keywords <- c(
  "water heating",
  "hot water"
)

#I am classifying different types of violations here:
heating_violations <- my_data %>%
  mutate(
    heating_category = case_when(
      grepl(paste(space_heating_keywords, collapse = "|"),
            violation, ignore.case = TRUE) ~ "Space Heating",
      
      grepl(paste(water_heating_keywords, collapse = "|"),
            violation, ignore.case = TRUE) ~ "Water Heating",
      
      TRUE ~ "Other"
    )
  ) %>%
  filter(heating_category != "Other")

glimpse(heating_violations)

#Labelling NA neighborhoods as unknown:
heating_violations <- heating_violations %>%
  mutate(
    Neighborhood = if_else(
      is.na(Neighborhood) | Neighborhood == "",
      "Unknown",
      Neighborhood
    )
  )


#Now, moving on we are counting number of neighbourhoods that have these violations reported.
#basically we asking the question - where is the risk concentrated geograhically? 

#For each neighborhood, compute:
  
#1. Total Space Heating Violations

#2. Total Water Heating Violations

#3. Total Heating Violations (combined)

#4. Number of unique properties affected

#5. Open vs Closed counts

heating_by_neighborhood <- heating_violations %>%
  group_by(Neighborhood) %>%
  summarize(
    total_space_heating = sum(heating_category == "Space Heating"),
    total_water_heating = sum(heating_category == "Water Heating"),
    total_heating_violations = n(),
    unique_addresses = n_distinct(complaint_address),
    open_violations = sum(status_type_name == "Open"),
    closed_violations = sum(status_type_name == "Closed")
  ) %>%
  arrange(desc(total_heating_violations))

glimpse(heating_by_neighborhood)

View(heating_by_neighborhood)


#sanity Checks:
sum(is.na(heating_violations$Neighborhood))

head(heating_by_neighborhood, 10)


#To create a verison 1 of violation based score, I am including: 
# This gives us - Volume, Breadth and Urgency

# 1. Total Heating Violations
# 2. Unique Addresses
# 3. Open violations - which indicate current risk.

heating_by_neighborhood <- heating_by_neighborhood %>%
  mutate(
    violations_per_address = total_heating_violations / unique_addresses,
    open_violation_pct = open_violations / total_heating_violations
  )

view(heating_by_neighborhood)

#Creating the score and selecting features that goes into the score:
#The Weatherization Risk Score is based on four core signals derived from code violations data: 
#1. total heating-related violations (space and water heating combined), 
#2. number of unique properties affected, 
#3. historical prevalence of heating issues, 
#4. indicators of current unresolved risk.

#Scaling 

#Selecting Features to Scale:

features_to_scale <- heating_by_neighborhood %>%
  select(
    total_heating_violations,
    unique_addresses,
    open_violations
  )

scale_minmax <- function(x) {
  if (max(x, na.rm = TRUE) == min(x, na.rm = TRUE)) {
    return(rep(0, length(x)))
  }
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

scaled_features <- features_to_scale %>%
  mutate(across(everything(), scale_minmax))


#Sanity Check::
summary(scaled_features)

#Calculating two different weatherisation scores:
heating_scaled <- heating_by_neighborhood %>%
  bind_cols(
    scaled_features %>%
      rename_with(~ paste0(.x, "_scaled"))
  )

#Historical Risk Score:
#has this neighborhood historically experienced heating-related problems ever and how widespread are they? 

heating_scaled <- heating_scaled %>%
  mutate(
    historical_risk_score =
      (total_heating_violations_scaled +
         unique_addresses_scaled) / 2
  )

#Current Risk Score:
heating_scaled <- heating_scaled %>%
  mutate(
    current_risk_score = open_violations_scaled
  )

#Using code violations data,
#I identified neighborhoods with the highest concentration of heating-related issues, distinguishing between space heating and water heating problems 
#and accounting for repeat issues across properties.

view(heating_scaled)

#The objective behind building a scoring system is basically to understand which neighborhoods have had a persistent, widespread heating problems ovee time?

#Current Risk Score: Which neighborhoods have ongoing heating related problems.


#Ranking Neighborhoods by historical risk score:

heating_scaled %>%
  arrange(desc(historical_risk_score)) %>%
  select(Neighborhood, historical_risk_score) %>%
  head(10)

#Ranking Neighborhoods by current risk score: 

heating_scaled %>%
  arrange(desc(current_risk_score)) %>%
  select(Neighborhood, current_risk_score) %>%
  head(10)


#Visualising: 

hist_cutoff <- median(heating_scaled$historical_risk_score, na.rm = TRUE)
curr_cutoff <- median(heating_scaled$current_risk_score, na.rm = TRUE)



heating_scaled <- heating_scaled %>%
  mutate(
    risk_quadrant = case_when(
      historical_risk_score >= hist_cutoff & current_risk_score >= curr_cutoff ~ "High Historical / High Current",
      historical_risk_score <  hist_cutoff & current_risk_score >= curr_cutoff ~ "Low Historical / High Current",
      historical_risk_score >= hist_cutoff & current_risk_score <  curr_cutoff ~ "High Historical / Low Current",
      TRUE ~ "Low Historical / Low Current"
    ),
    # simple “priority” to decide what gets labeled
  priority = 0.6 * historical_risk_score + 0.4 * current_risk_score
  )



label_df <- plot_df %>%
  group_by(risk_quadrant) %>%
  slice_max(priority, n = 3, with_ties = FALSE) %>%
  ungroup()

ggplot(plot_df, aes(x = historical_risk_score, y = current_risk_score, color = risk_quadrant)) +
  geom_point(size = 3.2, alpha = 0.85) +
  geom_vline(xintercept = hist_cutoff, linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = curr_cutoff, linetype = "dashed", linewidth = 0.6) +
  
  # label ONLY selected points
  geom_label_repel(
    data = label_df,
    aes(label = Neighborhood),
    size = 3.2,
    label.size = 0.15,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    max.overlaps = Inf,
    show.legend = FALSE
  ) +
  
  labs(
    title = "Syracuse Weatherization Risk Quadrant",
    subtitle = "Historical vs Current Heating Risk (labels = top 3 per quadrant)",
    x = "Historical Heating Risk Score",
    y = "Current Heating Risk Score",
    color = "Risk Category"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

while (!is.null(dev.list())) dev.off()

dev.new(width = 12, height = 7)

nrow(label_df)








