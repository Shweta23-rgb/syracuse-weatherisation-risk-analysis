#The Rental Registry dataset basically answers one key question: 
#Is a rental property currently compliant with Syracuse's rental registry requirements? 

library(tidyverse)
rental_df <- read_csv("/Users/saiswethalakkoju/Downloads/Open_Data_Project/Syracuse_Rental_Registry.csv")
violation_df <- read_csv("/Users/saiswethalakkoju/Downloads/Open_Data_Project/Code_Violations_V2.csv")

view(rental_registry)
glimpse(rental_registry)

#What exactly are we doing here: 

#1. rental_registry does not have neighborhood information
#2. So we are creating a bridge from the violations data, which does have neighborhood information. 
#Basically SBL -> Neighborhood
#3. Then we attach that neighborhood to each Rental Record via SBL.

#Step1: Takes the violations dataset which has man rows per property because each property can have multiple violations
#keeps only two columns (SBL and neighborhood) and then removes exact duplicate pairs
#It makes each SBL, Neighborhood pair appear only once. 

property_neighborhood <- violation_df %>%
  filter(!is.na(SBL), SBL != "") %>%
  select(SBL, Neighborhood) %>%
  distinct()   # one row per (SBL, Neighborhood)

view(property_neighborhood)

#Step 2: We are counting how many different neighborhoods each SBL appears in:
property_neighborhood %>%
  count(SBL) %>%
  filter(n > 1) %>%
  arrange(n) %>%
  head(10)


#For each SBL, We look at all neighborhoods it appears in violations_data
#Counts how often each SBL appears per neighborhood. 
#Picks the most frequent neighborhood per SBL 
#Keeps only that one neighborhood - sbl unique pair
#End result here is ONE ROW PER SBL AMD ONE SBL -> ONE neighborhood. 


property_neighborhood_clean <- my_data %>%
  filter(!is.na(SBL), !is.na(Neighborhood)) %>%
  count(SBL, Neighborhood, sort = TRUE) %>%
  group_by(SBL) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(SBL, Neighborhood)


#Sanity Check: Returns O, that means mapping is clean. Each SBL appears in only one neighborhood.
property_neighborhood_clean %>%
  count(SBL) %>%
  filter(n > 1)


#STEP - MERGING BOTH rental_df and violations_df here:
rental_with_neighborhood <- rental_df %>%
  filter(!is.na(SBL), SBL != "") %>%
  left_join(property_neighborhood_clean, by = "SBL")


view(rental_with_neighborhood)

#Out of 11k Rental Properties, 27.8% of the data never appeared in the code_violations dataset. 
rental_with_neighborhood %>%
  summarize(
    total = n(),
    missing_neighborhood = sum(is.na(Neighborhood)),
    pct_missing = missing_neighborhood / total
  )

#Coverage Summary: 
coverage_summary <- rental_with_neighborhood %>%
  summarize(
    total_rentals = n(),
    used_rentals = sum(!is.na(Neighborhood)),
    pct_used = used_rentals / total_rentals
  )

#Dropping Rentals which do not have any neighborhoods associated with them here: 
rental_final <- rental_with_neighborhood %>%
  filter(!is.na(Neighborhood))


#Aggregating by Neigborhood: 
#For each neighborhood, we want:
#1. Total rental properties
#2. Valid rentals
#3. Invalid (non-compliant) rentals
#4. Rental compliance rate
#5. Rental non-compliance rate



rental_by_neighborhood <- rental_final %>%
  filter(NeedsRR == "Yes") %>%   # only true rentals
  mutate(
    rr_valid = case_when(
      RRisValid == "Yes" ~ 1L,
      RRisValid == "No"  ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  group_by(Neighborhood) %>%
  summarize(
    rental_properties   = n_distinct(SBL),
    valid_rentals       = n_distinct(SBL[rr_valid == 1], na.rm = TRUE),
    invalid_rentals     = n_distinct(SBL[rr_valid == 0], na.rm = TRUE),
    rr_valid_rate       = valid_rentals / rental_properties,
    rr_invalid_rate     = invalid_rentals / rental_properties
  ) %>%
  ungroup()

view(rental_by_neighborhood)

summary(rental_by_neighborhood$rental_properties)

summary(rental_by_neighborhood$rr_invalid_rate)


rental_by_neighborhood %>%
  arrange(desc(rr_invalid_rate)) %>%
  head(10)

heating_plus_rentals <- heating_scaled %>%
  left_join(rental_by_neighborhood, by = "Neighborhood")

view(heating_plus_rentals)

unique_rentals <- unique(rental_by_neighborhood$Neighborhood)

view(unique_rentals)


### Calculating Rental Score: 

#Step 1: In which neighborhoods are renters most exposed to unsafe or non complaint housing? 

#The answer lies in two things: 
#1. Rental Concentration - Number of Rental Properties ()
#2. Rental Non Compliance - % of rentals without valid RR. 

#Step 2: Scaling: 

heating_plus_rentals <- heating_plus_rentals %>%
  mutate(
    rental_properties_scaled = scale_minmax(rental_properties),
    rr_invalid_rate_scaled   = scale_minmax(rr_invalid_rate)
  )

#Step 3: Creating Rental Risk Score: 

heating_plus_rentals <- heating_plus_rentals %>%
  mutate(
    rental_risk_score =
      (rental_properties_scaled + rr_invalid_rate_scaled) / 2
  )

#Step 4: Scanity Checks: 
summary(heating_plus_rentals$rental_risk_score)


heating_plus_rentals %>%
  arrange(desc(rental_risk_score)) %>%
  select(Neighborhood, rental_risk_score) %>%
  head(10)

view(heating_plus_rentals)


#------------------------------------------------------------------------------------------------------------------------------

#What exactly is a census tarct: A small, stable geographic area defined by the U.S. Census Bureau to represent a neighborhood-sized population with relatively 
#similar social and economic characteristics.


## Census Level Tract Data -> Mapping that into Syracuse Neighborhood Data: 

#Lets get the Syracuse Neighborhood Geojson file in here: 
#To work with spatial data we need to install sf (Simple Features Package)


install.packages("sf", dependencies = TRUE, type = "source")
library(sf)

#This is basically neighborhoods and their geographical coordinates/ Polygons

#Step 1: Loading Neighborhood Level Geometry
neighborhoods <- st_read("/Users/saiswethalakkoju/Downloads/Open_Data_Project/Syracuse_Neighborhoods_2327829995554664018.geojson")

glimpse(neighborhoods)

view(neighborhoods)


#Step 2: Loading Census Tract Geometry: 

#Reasoning:
#Census Tracts shape file which has coordinates - because the original census_tracts csv file just has tract level attributes like GEOID, Median Income, Housing Age. 
#The CENSUS csv file do not have tract geometry - we cannot assign it a neighborhood.
#To combine the tract level data with Syracuse Neighborhood Level data we need polygons/coordinates related to each tract.
#So that is why we have downloaded census tract level geometry - this gives us geoid and geometry. 
#So we can combine this with the census csv files. 


tracts_ny <- st_read("/Users/saiswethalakkoju/Downloads/tl_2024_36_tract/tl_2024_36_tract.shp")

glimpse(tracts_ny)

tracts_onondaga <- tracts_ny %>%
  filter(COUNTYFP == "067")

view(tracts_onondaga)


### Each row in dp03 represents data for each census tract. 

### Each row = one tract in onondaga county.


install.packages("tidycensus")
library(tidycensus)

census_api_key("6bf38d8333eba48948f192a4eab14751fb00d6e6", install = TRUE)


Sys.getenv("CENSUS_API_KEY")


library(tidycensus)
library(dplyr)

dp03 <- get_acs(
  geography = "tract",
  table = "DP03",
  state = "NY",
  county = "Onondaga",
  year = 2023,
  survey = "acs5",
  output = "wide"
)

dp04 <- get_acs(
  geography = "tract",
  table = "DP04",
  state = "NY",
  county = "Onondaga",
  year = 2023,
  survey = "acs5",
  output = "wide"
)


glimpse(dp03)
glimpse(dp04)

## We are selecting specific socio economic indicators from dp03 and dp04 for eevry census tract
## Creating a tract_level economic profile that can later be spatially mapped to neighborhoods. 

##Research: What exactly is spatial mapping and how does that work? 

#Selecting the necessary columns here: Mapping the variable names correctly is the first step here: 

library(tidycensus)
library(dplyr)
library(stringr)

dp03_dict <- load_variables(2023, "acs5/profile", cache = TRUE) %>%
  filter(str_detect(name, "^DP03")) %>%
  transmute(
    variable = name,
    label = str_squish(str_replace_all(label, "!!", " > ")),
    concept
  )

nrow(dp03_dict)
dp03_dict %>% slice(1:10)

dp03_dict %>%
  filter(str_detect(str_to_lower(label), "median household income")) %>%
  select(variable, label) %>%
  arrange(variable)

view(dp03_dict)


#To reiterate, DP03 is an added layer to the rental_risk_score we have calculated before: 

#We are adding this to understand, do residents in this neighborhood have the economic capacity to absorb or fix heating-related housing issues?

dp03_vars <- c(
  median_household_income = "DP03_0062",
  poverty_rate_pct        = "DP03_0128P",
  snap_households_pct     = "DP03_0074P"
)

dp03_tract <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Onondaga",
  year = 2023,
  survey = "acs5",
  variables = dp03_vars,
  output = "wide"
) %>%
  transmute(
    GEOID,
    NAME,
    median_household_income = median_household_incomeE,
    poverty_rate_pct        = poverty_rate_pctE,
    snap_households_pct     = snap_households_pctE
  )


summary(dp03_tract$median_household_income)
summary(dp03_tract$poverty_rate_pct)
summary(dp03_tract$snap_households_pct)


#Dealing with DP04: 

dp04_dict <- load_variables(2023, "acs5/profile", cache = TRUE) %>%
  filter(str_detect(name, "^DP04")) %>%
  transmute(
    variable = name,
    label = str_squish(str_replace_all(label, "!!", " > ")),
    concept
  )

dp04_yearbuilt <- dp04_dict %>%
  filter(str_detect(str_to_lower(label), "year structure built")) %>%
  select(variable, label) %>%
  arrange(variable)

dp04_yearbuilt

dp04_yearbuilt %>%
  filter(str_ends(variable, "P")) %>%
  select(variable, label)


pre1980_vars <- dp04_yearbuilt %>%
  filter(
    str_ends(variable, "P"),
    str_detect(label, "Built 1970 to 1979|Built 1960 to 1969|Built 1950 to 1959|Built 1940 to 1949|Built 1939 or earlier")
  ) %>%
  pull(variable)

pre1960_vars <- dp04_yearbuilt %>%
  filter(
    str_ends(variable, "P"),
    str_detect(label, "Built 1950 to 1959|Built 1940 to 1949|Built 1939 or earlier")
  ) %>%
  pull(variable)

pre1980_vars
pre1960_vars


dp04_tract_raw <- get_acs(
  geography = "tract",
  state = "NY",
  county = "Onondaga",
  year = 2023,
  survey = "acs5",
  variables = unique(c(pre1980_vars, pre1960_vars)),
  output = "wide"
)

dp04_housing_age_tract <- dp04_tract_raw %>%
  transmute(
    GEOID,
    NAME,
    pct_housing_pre_1980 = rowSums(across(all_of(paste0(pre1980_vars, "E"))), na.rm = TRUE),
    pct_housing_pre_1960 = rowSums(across(all_of(paste0(pre1960_vars, "E"))), na.rm = TRUE)
  )

summary(dp04_housing_age_tract$pct_housing_pre_1980)
summary(dp04_housing_age_tract$pct_housing_pre_1960)

# Optional check: pre-1960 should never exceed pre-1980
sum(dp04_housing_age_tract$pct_housing_pre_1960 > dp04_housing_age_tract$pct_housing_pre_1980, na.rm = TRUE)


#Merging dp03 and dp04: 

tract_features <- dp03_tract %>%
  left_join(
    dp04_housing_age_tract %>% 
      select(GEOID, pct_housing_pre_1980, pct_housing_pre_1960),
    by = "GEOID"
  )


#Sanity Check: 

view(tract_features)


#Sanity Check: 

sum(is.na(tract_features$pct_housing_pre_1980))

names(tracts_onondaga)


library(sf)
tract_sf <- tracts_onondaga %>%
  select(GEOID, geometry) %>%
  left_join(tract_features, by = "GEOID")


# should show POLYGON/MULTIPOLYGON
st_geometry_type(tract_sf) %>% head()

# should be 0 if everything matched
sum(is.na(tract_sf$median_household_income))

# confirm columns exist
names(tract_sf)

view(tract_sf)

#So what is tract_sf = censu tract level geometry (Polygons) + Tract Level Features we need. 

#Keeping only tracts that intersect with Syracuse Neighborhoods - Spatial Mapping
library(sf)

tract_sf_syracuse <- tract_sf[
  lengths(st_intersects(tract_sf, neighborhoods)) > 0,
]

#When we ran the above code, we encountered an error : tract_sf and neighborhoods are in different Coordinate Reference Systems (CRS)
#What is CRS? -- How coordinates are interpreted and What “units” and “projection” the map uses

#To check if both of them are using the same CRS (Coordinate Reference System)
st_crs(tract_sf)
st_crs(neighborhoods)

#From the output we can see that
#tract_sf is in NAD83 (EPSG:4269)
#neighborhoods is in WGS 84 (EPSG:4326)


#Make both layers EPSG:4326 (WGS84), then intersect.
#Both datasets are now in the same Coordinate Reference System
#Specifically: WGS 84 (EPSG:4326)

tract_sf_4326 <- st_transform(tract_sf, 4326)
neighborhoods_4326 <- st_transform(neighborhoods, 4326)

tract_sf_syracuse <- tract_sf_4326[
  lengths(st_intersects(tract_sf_4326, neighborhoods_4326)) > 0,
]


#Sanity Check: 

st_crs(tract_sf_4326)
st_crs(neighborhoods_4326)

nrow(tract_sf_4326)
nrow(tract_sf_syracuse)

view(tract_sf_syracuse)


view(neighborhoods)
view(neighborhoods_4326)

#Attach the neighborhood name to a tract only if the tract centroid point lies inside the neighborhood polygon.
#Why are we using centroids here, specifically? 
#tracts are polygons, neighbourhoods are polygons. 
#A tract might touch multiple neighbourhoods, centroid gives us one point to each neighborhod.


tract_centroids <- st_centroid(tract_sf_syracuse)
tract_to_neighborhood <- st_join(
  tract_centroids,
  neighborhoods_4326 %>% select(Name),
  join = st_within
)

#What is within here?  That means assign the neighborhood only if the centroid point is inside the neighborhood polygon.

view(tract_to_neighborhood)
#tract_to_neigboorhood contains only Syracuse tracts. 

# 1. Any tracts not assigned to a neighborhood?
sum(is.na(tract_to_neighborhood$Name))

# 2. Peek at assignments
head(
  tract_to_neighborhood %>%
    st_drop_geometry() %>%
    select(GEOID, Name)
)

#After assigning these we can see that there are 14 tracts that are unassigned? 

#tract_to_neighborhood has 69 Syracuse tracts.

#After st_within (centroid inside polygon), 14 tracts had Name = NA.

#If you drop those 14, you will lose their economic + housing-age features.

#Then when you aggregate to neighborhoods later, 
#your neighborhood metrics will be incomplete / biased (especially near city boundaries).

#So, we need to deal with the unassigned tracts here. 

#But first why is this happening? 
#They happen because: We used centroids and some tract centroids fall right on boundaries
#or just outside the neighborhood polygons


# 1) We first pull the GEOIDS of the tracts that whose neighborhood name 
# is missing (Name == NA) in tract_to_neighborhood dataset
unassigned_geoids <- tract_to_neighborhood %>%
  st_drop_geometry() %>%
  filter(is.na(Name)) %>%
  pull(GEOID)

length(unassigned_geoids)   # should be 14
unassigned_geoids           # optional: prints the GEOIDs

# 2) Get the TRACT POLYGONS for only those 14 GEOIDs
unassigned_tract_polygons <- tract_sf_syracuse %>%
  filter(GEOID %in% unassigned_geoids) %>%
  select(GEOID, geometry)

nrow(unassigned_tract_polygons)  # should be 14

#Here in the step2, we dont take the centroid, instead the full tract polygon
#that is because centroid is a single point, if the centroid falls right on the
#boundary or outside, then the name is not assigned. 
#so to calculate the overlap region, we are using full polygons again. 


# 3) Put both layers into a CRS that works well with area calculations 
#(UTM Zone 18N fits Syracuse well)

#Overlap area must be measured in meaningful units (meters²), not degrees.

#EPSG:4326 is latitude/longitude (degrees) → not ideal for area.

#So we temporarily transform both datasets to a projected CRS (UTM Zone 18N works well for Syracuse):

unassigned_tract_polygons_utm <- st_transform(unassigned_tract_polygons, 32618)
neighborhoods_utm <- st_transform(neighborhoods_4326 %>% select(Name), 32618)

# 4) Compute the overlap polygons and overlap area
overlaps <- st_intersection(
  unassigned_tract_polygons_utm,
  neighborhoods_utm
) %>%
  mutate(overlap_area = as.numeric(st_area(geometry)))


#Sanity Check 
overlaps %>%
  st_drop_geometry() %>%
  select(GEOID, Name, overlap_area) %>%
  arrange(desc(overlap_area)) %>%
  head(10)

#What is happening in STEP - 4: 

#Takes one tract polygon, checks it against every neighborhood polygon

#Wherever they overlap: Creates a new polygon = the overlapping shape

#Keeps both IDs (GEOID and Name) Computes the area of that overlapping shape

# So if one tract overlaps 3 neighborhoods, you get 3 rows for that tract.

# 5) For each tract, pick the neighborhood with the largest overlap

unassigned_fixed_area <- overlaps %>%
  st_drop_geometry() %>%              # we only need the IDs + overlap_area
  group_by(GEOID) %>%
  slice_max(overlap_area, n = 1, with_ties = FALSE) %>%   # pick biggest overlap
  ungroup() %>%
  select(GEOID, Name)

nrow(unassigned_fixed_area)                 # should be 14
sum(is.na(unassigned_fixed_area$Name))      # should be 0
unassigned_fixed_area %>% count(GEOID) %>% filter(n > 1)  # should be 0 rows


#STEP - 6: Combining the “already assigned” tracts + the “fixed” unassigned tracts

assigned_map <- tract_to_neighborhood %>%
  st_drop_geometry() %>%
  filter(!is.na(Name)) %>%
  select(GEOID, Name)

tract_neighborhood_map <- bind_rows(assigned_map, unassigned_fixed_area)


#Sanity Checks: 
nrow(tract_neighborhood_map)                     # should be 69
sum(is.na(tract_neighborhood_map$Name))          # should be 0
tract_neighborhood_map %>% count(GEOID) %>% filter(n > 1)  # should be 0 rows


view(tract_neighborhood_map)

tract_features_syr <- tract_features %>%
  filter(GEOID %in% tract_neighborhood_map$GEOID) %>%
  left_join(tract_neighborhood_map, by = "GEOID")


#Sanity Checks: 

nrow(tract_features_syr)

sum(is.na(tract_features_named_syr$Name))


tract_features_named_syr %>% count(GEOID) %>% filter(n > 1)


view(tract_features_named_syr)

names(tract_features_named_syr)

#Now we are moving from tract_level to neighborhood_level. We need to aggregate features by neighbourhood in the tract_features_names_syr. 

#To do that, we use different methods for different features. 

#For poverty_rate_pct and snap_household_pct, I am planning to use population_based_weights

#For median_household_income we use median because Income distributions are skewed,  
#A very few rich households can distort the mean

#For housing age - We take mean here


#So STEP -1 , to calculate population weighted mean, we need population total per tract: 

#pop_tract contains population for all onondaga tracts.
pop_tract <- get_acs(
  geography = "tract",
  table = "B01003",
  state = "NY",
  county = "Onondaga",
  year = 2023,
  survey = "acs5"
) %>%
  select(
    GEOID,
    total_population = estimate
  )

# quick sanity checks
nrow(pop_tract)                 # should be ~142 (Onondaga tracts)
summary(pop_tract$total_population)

#STEP2: Join tract population into Syarcuse tract features: 

tract_features_named_syr <- tract_features_named_syr %>%
  left_join(pop_tract, by = "GEOID")

# sanity checks
nrow(tract_features_named_syr)                      # should still be 69
sum(is.na(tract_features_named_syr$total_population))  # should be 0 (or very close to 0)
summary(tract_features_named_syr$total_population)

view(tract_features_named_syr)


#STEP 3: AGGREGATING TRACTS TO NEIGHBORHOOD LEVEL: 
#How aare we aggregating here: 

#For poverty_rate_pct and snap_households_pct, we used a population-weighted mean
#For household ages we took simple mean
#For median_income - just a median 


neighborhood_features <- tract_features_named_syr %>%
  st_drop_geometry() %>%
  group_by(Name) %>%
  summarise(
    n_tracts = n(),
    neighborhood_population = sum(total_population, na.rm = TRUE),
    
    median_household_income = median(median_household_income, na.rm = TRUE),
    
    poverty_rate_pct = weighted.mean(poverty_rate_pct, w = total_population, na.rm = TRUE),
    snap_households_pct = weighted.mean(snap_households_pct, w = total_population, na.rm = TRUE),
    
    pct_housing_pre_1980 = mean(pct_housing_pre_1980, na.rm = TRUE),
    pct_housing_pre_1960 = mean(pct_housing_pre_1960, na.rm = TRUE)
  ) %>%
  ungroup()


tract_features_named_syr <- tract_features_named_syr %>%
  rename(
    census_tract_name = NAME,
    neighborhood = Name
  )

names(tract_features_named_syr)
names(neighborhood_features)

view(neighborhoods)

neighborhood_features <- neighborhood_features %>%
  rename(
    neighborhood = Name
  )

#Fixing Naming issues

neighborhood_features <- neighborhood_features %>%
  mutate(neighborhood = str_replace(neighborhood, "\\.$", ""))

heating_plus_rentals <- heating_plus_rentals %>%
  mutate(
    Neighborhood = case_when(
      Neighborhood %in% c("Hawley Green", "Hawley-Green") ~ "Hawley Green",
      TRUE ~ Neighborhood
    )
  )



#MERGING
weatherization_full <- heating_plus_rentals %>%
  left_join(neighborhood_features, by = c("Neighborhood" = "neighborhood"))

sum(is.na(weatherization_full$median_household_income))

weatherization_full %>%
  filter(is.na(median_household_income)) %>%
  distinct(Neighborhood)


nrow(weatherization_full)              # should equal nrow(heating_plus_rentals)
sum(is.na(weatherization_full$median_household_income))

view(weatherization_full)

# See exact strings on BOTH sides
sort(unique(weatherization_full$Neighborhood))
sort(unique(neighborhood_features$neighborhood))

names(weatherization_full)

