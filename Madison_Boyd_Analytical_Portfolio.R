# =========================================================
# CSC 308: Organization of Programming Languages
# Project: Healthcare Access Trends (2021-2023) Extra Credit
# Student: Madison Boyd
# =========================================================

# Load the required library for data cleaning
library(dplyr)

library(tidyverse)

# =========================================================
# SECTION: 2021 DATA PREPARATION AND ANALYSIS
# =========================================================

# ---------------------------------------------------------
# STEP 1: PROCESSING AND CLEANING 2021 DATA
# ---------------------------------------------------------





# --- FILE 1: Additional Measure Data ---
# skipped the first placeholder row told R to treat "N/A" as a missing value
data21_measures <- read.csv("data/raw/2021/2021CountyHealthRankingsData/AdditionalMeasureData.csv", 
                            skip = 1, 
                            na.strings = c("N/A", "", " "))

# Narrowing down to variables for Research Questions
clean21_measures <- data21_measures %>%
  select(
    FIPS, State, County,
    Median.Household.Income,
    X..Uninsured,
    X..Rural,
    Life.Expectancy
  ) %>%
  filter(County != "") %>%
  # PREPROCESSING: Removing the rows that were originally N/A
  na.omit()

# --- VERIFICATION STEP: Data Quality Check ---
# This proves the N/A rows are gone
print("Checking for missing values in 2021 Measures:")
print(sum(is.na(clean21_measures))) # Should output 0

print("Row count after cleaning (Verify it decreased):")
print(nrow(clean21_measures))

# view data
View(clean21_measures)

head(clean21_measures)

# Add Year label for future comparison
clean21_measures$Year <- 2021



# --- FILE 2. Additional Measures Sources and Years File ---

# 1. Read the file
data21_metadata <- read.csv("data/raw/2021/2021CountyHealthRankingsData/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

# 2. clean data 
clean21_metadata <- data21_metadata %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Description = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  filter(!is.na(Measure) & Measure != "")

# Verification
print("2021 Data Dictionary (Metadata) Successfully Cleaned:")
head(clean21_metadata)

# view data

View(clean21_metadata)

# --- FILE 3. Outcomes and Factors Rankings File ---

library(dplyr)

# read file data
data21_outcomesandfactorsrankings <- read.csv("data/raw/2021/2021CountyHealthRankingsData/Outcomes and Factors Rankings.csv", 
                                              skip = 1, 
                                              na.strings = c("N/A", "", " "))

# view data
View(data21_outcomesandfactorsrankings)

# clean data

clean21_ranked <- data21_outcomesandfactorsrankings %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # In this file: 
    # 'Rank' is Health Outcomes (Current Health)
    # 'Rank.1' is Health Factors (Future Health Predictors)
    Health_Outcomes_Rank = Rank,
    Health_Factors_Rank = Rank.1
  ) %>%
  filter(County != "") %>%
  # Data Quality: Remove rows where the county wasn't ranked (N/A)
  na.omit()

nrow(clean21_ranked)
# Check to ensure the names are changed
head(clean21_ranked)

View(clean21_ranked)

# --- FILE 4. Outcomes and Factors sub Rankings File ---

library(dplyr)

# Load the sub-rankings file, skipping the top header

data21_subranked <- read.csv("data/raw/2021/2021CountyHealthRankingsData/Outcomes and Factors SubRankings.csv", 
                             skip = 1, 
                             na.strings = c("N/A", "", " "))

# Narrowing down to the sub-ranks that answer the 6 research questions

# map the R names to the actual categories 
clean21_subranked <- data21_subranked %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # Match numbered ranks to their categories
    Length_of_Life_Rank   = Rank,
    # (Rank.1 is usually Quality of Life)
    # (Rank.2 is usually Health Behaviors)
    Clinical_Care_Rank    = Rank.3, 
    Socioeconomic_Rank    = Rank.4
  ) %>%
  filter(County != "") %>%
  # PREPROCESSING: Ensure complete dataset for EDA
  na.omit()

# Verification for  Report
print("Sample of Renamed Sub-Rankings:")
head(clean21_subranked)

# view data

View(clean21_subranked)

# --- FILE 5. Ranked Measure Data ---
# read file
data21_rankedmeasures <- read.csv("data/raw/2021/2021CountyHealthRankingsData/Ranked Measure Data.csv", 
                                  skip = 1, 
                                  na.strings = c("N/A", "", " "))

# view file
 View(data21_rankedmeasures)

# 2. Narrow down to the "In-Depth" variables for report
clean21_rankedmeasures <- data21_rankedmeasures %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # Primary health outcome metric
    YPLL_Rate = Years.of.Potential.Life.Lost.Rate,
    # Demographic comparison metrics (for Research Question 3)
    YPLL_Black = YPLL.Rate..Black.,
    YPLL_White = YPLL.Rate..White.,
    # Quality of life indicators
    Percent_Poor_Health = X..Fair.or.Poor.Health,
    Percent_Low_Birthweight = X..Low.birthweight
  ) %>%
  filter(County != "") %>%
  # PREPROCESSING: Ensuring Data Quality by removing incomplete records
  na.omit()

# 3. Verification: Check the first 5 rows
print("Verification: 2021 Ranked Measures Sample")
head(clean21_rankedmeasures, 5)

# view data

View(clean21_rankedmeasures)


# --- FILE 6. Ranked Measures Sources and Year ---
# 1. Read the file
data21_metadata <- read.csv("data/raw/2021/2021CountyHealthRankingsData/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

# 2. Clean the file
metadata21 <- data21_metadata %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Definition = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  # PREPROCESSING: Remove the rows where Measure is empty (Data Quality)
  filter(!is.na(Measure) & Measure != "")

# 3. Verification
print("2021 Metadata Table Successfully Cleaned:")
head(metadata21)

#View data

View(metadata21)

# ---------------------------------------------------------
# STEP 2: INTEGRATING 2021 DATA
# ---------------------------------------------------------

# 1. Join all files strictly by FIPS 
master_2021 <- clean21_measures %>%
  inner_join(clean21_ranked, by = "FIPS") %>%
  inner_join(clean21_subranked, by = "FIPS") %>%
  inner_join(clean21_rankedmeasures, by = "FIPS")

# 2. Standardize names and remove the .x/.y duplicates
master_2021 <- master_2021 %>%
  rename(State = State.x, County = County.x) %>%
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"))

# 3. Final Check 
print("--- MASTER 2021 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final Row Count:", nrow(master_2021)))

# Check the first few columns to ensure names are clean
head(master_2021[, 1:5])

# install the package

install.packages("ggplot2")

# verify and load

library(ggplot2)

# install hexbin package 

install.packages("hexbin")

# install tidyr

install.packages("tidyr")





# --- B. DATA TYPE AUDIT (FIXES OVERLAPPING AXES & 'NR' DISCRETE ERRORS) ---
# Ensure R treats these as Numeric Doubles (Continuous) rather than text (Discrete).
# This prevents the "Discrete value supplied to continuous scale" error in ggplot.

# --- B. DATA TYPE AUDIT ---
# Fixing the 'replacement has 0 rows' error by using verified column names.

# 1. Force Rankings and Outcomes to Numeric
master_2021$Health_Factors_Rank  <- as.numeric(as.character(master_2021$Health_Factors_Rank))
master_2021$Health_Outcomes_Rank <- as.numeric(as.character(master_2021$Health_Outcomes_Rank))
master_2021$Clinical_Care_Rank   <- as.numeric(as.character(master_2021$Clinical_Care_Rank))
master_2021$Socioeconomic_Rank   <- as.numeric(as.character(master_2021$Socioeconomic_Rank))

# 2. Force Economic and Quality of Life Measures
master_2021$Median.Household.Income <- as.numeric(as.character(master_2021$Median.Household.Income))
master_2021$Percent_Poor_Health     <- as.numeric(as.character(master_2021$Percent_Poor_Health))

# 3. Force Racial Disparity Metrics (YPLL - Years of Potential Life Lost)
# These replace Life Expectancy as primary demographic disparity proof
master_2021$YPLL_Rate  <- as.numeric(as.character(master_2021$YPLL_Rate))
master_2021$YPLL_Black <- as.numeric(as.character(master_2021$YPLL_Black))
master_2021$YPLL_White <- as.numeric(as.character(master_2021$YPLL_White))

# 4. PREPROCESSING: Data Quality Check
# This removes rows where conversion failed (like "NR" entries)
master_2021 <- na.omit(master_2021)

# 5. VERIFICATION
print("Audit Successful. Verified Numeric Columns:")
str(master_2021[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])


# --- C.  EDA PLOT: INCOME VS. CLINICAL CARE ---
# Using the Full State Names found in the unique() check
target_states <- c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida") 

plot_data_21 <- master_2021 %>% 
  filter(State %in% target_states)

ggplot(plot_data_21, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  # Alpha = 0.2 handles density; size = 1 keeps it clean
  geom_point(alpha = 0.2, color = "midnightblue", size = 1) +
  # Linear model trend line to answer Research Questions
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  # Facet by State to solve overplotting and show regional differences
  facet_wrap(~State, scales = "free_x") + 
  # Formatting X-axis to "k" for professional clarity
  scale_x_continuous(labels = function(x) paste0(x/1000, "k")) +
  # Formatting Y-axis with specific breaks to stop side-number overlap
  scale_y_continuous(breaks = seq(0, 200, by = 50)) + 
  labs(title = "EDA: Healthcare Access Trends by State (2021)",
       subtitle = "Relationship between Median Income and Clinical Care Ranking",
       x = "Median Household Income ($k)",
       y = "Clinical Care Rank (1 = Best)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold") # Bold the State names for clarity
  )

# --- D. SAVE THE FINAL PLOT ---

ggsave("2021_Socioeconomic_vs_Health_EDA.png", width = 10, height = 7, dpi = 300)

# --- E. VERIFICATION ---
print("2021 Master Processing Complete.")
summary(master_2021$Clinical_Care_Rank)



# --- F. EDA PLOT: RACIAL DISPARITY (PREMATURE MORTALITY) ---

# 1. Prepare the data using the verified YPLL columns from master_2021

racial_data_21 <- master_2021 %>%
  filter(State %in% c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida")) %>%
  select(State, YPLL_Black, YPLL_White) %>%
  # Pivoting the YPLL columns for side-by-side comparison
  pivot_longer(cols = c(YPLL_Black, YPLL_White), 
               names_to = "Race", 
               values_to = "YPLL_Rate") %>%
  na.omit()

# 2. Generate the Boxplot
# Note: In YPLL, a HIGHER bar means higher premature mortality
ggplot(racial_data_21, aes(x = State, y = YPLL_Rate, fill = Race)) +
  geom_boxplot(alpha = 0.7) +
  # Use colors midnight blue and orange for contrast
  scale_fill_manual(values = c("midnightblue", "darkorange"), 
                    labels = c("Black population", "White population")) +
  labs(title = "Regional Premature Mortality Disparities (2021)",
       subtitle = "Comparison of Black and White Years of Potential Life Lost (YPLL)",
       caption = "Data Source: County Health Rankings 2021",
       x = "State",
       y = "YPLL Rate (Years Lost per 100k)",
       fill = "Demographic Group") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 14)
  )

# 3. Save image for report
ggsave("2021_Racial_Disparity_YPLL_Plot.png", width = 10, height = 6)


# --- G.  EDA PLOT: HEALTH OUTCOME VS HEALTH FACTOR  ---

# 2. CREATE THE PLOT DATA: Filter for South Atlantic and remove NAs
# creates the 'plot2_data' object that R is looking for to prevent error
plot2_data <- master_2021 %>%
  filter(State %in% c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida")) %>%
  filter(!is.na(Health_Factors_Rank) & !is.na(Health_Outcomes_Rank))

# 3. GENERATE THE PLOT
ggplot(plot2_data, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  #  geom_jitter to spread out dots so they don't overlap (width/height = 2)
  geom_jitter(alpha = 0.4, color = "darkgreen", width = 2, height = 2) +
  
  # trend line to show the relationship between factors and outcomes
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  
  # CLEAN THE AXES:breaks so the numbers aren't crowded
  scale_y_continuous(breaks = seq(0, 150, by = 50)) + 
  scale_x_continuous(breaks = seq(0, 150, by = 50)) +
  
  # Separate by state for regional comparison
  facet_wrap(~State) + 
  
  labs(title = "Predictors vs. Current Health Outcomes (2021)",
       subtitle = "Relationship between Health Factors and Current County Rankings",
       x = "Health Factors Rank (1 = Best Environment)",
       y = "Health Outcomes Rank (1 = Best Health)") +
  theme_light() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# 4. Save the image for report
ggsave("2021_Outcomes_vs_Factors_.png", width = 10, height = 7)


# --- H. OUTCOMES VS. FACTORS (ALL STATES) ---




# 2. DATA QUALITY: 
# Removing NAs ensures the red trend line can be calculated correctly
all_states_plot_data <- master_2021 %>%
  filter(!is.na(Health_Factors_Rank) & !is.na(Health_Outcomes_Rank))

# 3. GENERATE THE COMPREHENSIVE PLOT
ggplot(all_states_plot_data, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  # smaller 'size' and 'alpha' because there are thousands of counties
  # geom_jitter  so you data can be seen through the clusters
  geom_jitter(alpha = 0.2, color = "darkgreen", size = 0.5, width = 1.5, height = 1.5) +
  
  # red trend line to show the overall national/state relationship
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, size = 0.5) +
  
  # CLEAN THE AXES: Use 100-unit breaks to keep the labels from overlapping
  scale_y_continuous(breaks = seq(0, 300, by = 100)) + 
  scale_x_continuous(breaks = seq(0, 300, by = 100)) +
  
  # Facet by State
  facet_wrap(~State) + 
  
  labs(title = "National Health Predictors vs. Outcomes (2021)",
       subtitle = "Relationship between Health Factors and Current County Rankings across All States",
       x = "Health Factors Rank (1 = Best)",
       y = "Health Outcomes Rank (1 = Best)") +
  
  # THEME: Shrink the text sizes so the 50 labels fit on one screen
  theme_light() +
  theme(
    strip.text = element_text(size = 6, face = "bold"), # Smaller state labels
    axis.text = element_text(size = 5),                # Smaller axis numbers
    axis.title = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

# 4. Save the high-resolution version 
ggsave("2021_National_Outcomes_vs_Factors.png", width = 20, height = 15, dpi = 300)

# --- I. EDA Plot Clinical Care Rank and Premature Mortality (YPLL) ---

master_five_states <- master_2021 %>%
  filter(State %in% c("Maryland", "Virginia", "North Carolina", "Georgia", "Florida"))

# Plot: Clinical Care Access vs. Mortality
ggplot(master_five_states, aes(x = Clinical_Care_Rank, y = YPLL_Rate)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  facet_wrap(~State) + 
  labs(title = "Healthcare Access vs. Mortality: South Atlantic Focus (2021)",
       subtitle = "Relationship between Clinical Care Rank and YPLL Rate",
       x = "Clinical Care Rank (Lower = Better Access)",
       y = "YPLL Rate (Years Lost per 100k)") +
  theme_minimal()

# Save the focused plot
ggsave("Clinical_Care_vs_YPLL_South_Atlantic.png", width = 10, height = 7, dpi = 300)

# --- J. Rural vs Urban YPLL Comparision ---



# Plot: Rural vs. Urban YPLL Comparison


regional_comparison_2021 <- master_2021 %>%
  filter(State %in% c("Maryland", "Virginia", "North Carolina", "Georgia", "Florida")) %>%
  group_by(State) %>%
  mutate(Median_Rural = median(X..Rural, na.rm = TRUE)) %>%
  # If a county is above its state's median rurality, it's 'High Rural'
  mutate(County_Type = ifelse(X..Rural > Median_Rural, "Higher Rurality", "Lower Rurality")) %>%
  ungroup() %>%
  mutate(County_Type = factor(County_Type, levels = c("Lower Rurality", "Higher Rurality")))


regional_comparison_2021 %>% group_by(State, County_Type) %>% tally()


ggplot(regional_comparison_2021, aes(x = State, y = YPLL_Rate, fill = County_Type)) +
  geom_violin(position = position_dodge(width = 0.8, preserve = "single"), alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.8, preserve = "single"), outlier.shape = 1) +
  scale_fill_manual(values = c("Higher Rurality" = "#5e3c99", "Lower Rurality" = "#fdb863")) +
  labs(title = "Regional Mortality by Relative Rurality (2021)",
       subtitle = "Comparing Counties Above/Below State Median Rurality",
       x = "State",
       y = "YPLL Rate",
       fill = "Classification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggsave("South_Atlantic_Relative_Rurality_2021.png", width = 12, height = 8, dpi = 300)

# ---------------------------------------------------------
# STEP 3: 2021 ANALYTICAL BASELINE & SUMMARY
# ---------------------------------------------------------

library(dplyr)

# --- A. Mathematical Correlation Proofs ---

# 1. Income vs. Health Rank
cor_income_health <- cor.test(master_2021$Median.Household.Income, 
                              master_2021$Clinical_Care_Rank, 
                              use = "complete.obs")

# 2. Health Factors vs. Health Outcomes 
# Proves if environmental factors (income/smoking) actually predict current health
cor_factors_outcomes <- cor.test(master_2021$Health_Factors_Rank, 
                                 master_2021$Health_Outcomes_Rank, 
                                 use = "complete.obs")

# 3. Racial Disparity Correlation (Black vs. White YPLL)
# Measures how closely the premature mortality rates of both groups move together
cor_racial_ypll <- cor.test(master_2021$YPLL_Black, 
                            master_2021$YPLL_White, 
                            use = "complete.obs")

# --- B. Create a Combined Statistical Coefficient Table ---

cor_results_2021 <- data.frame(
  Analysis_Type = c("Income vs Clinical Care", "Factors vs Outcomes", "Black vs White YPLL"),
  Correlation_Coefficient = c(cor_income_health$estimate, 
                              cor_factors_outcomes$estimate, 
                              cor_racial_ypll$estimate),
  P_Value = c(cor_income_health$p.value, 
              cor_factors_outcomes$p.value, 
              cor_racial_ypll$p.value),
  Year = 2021
)

write.csv(cor_results_2021, "2021_Statistical_Proofs.csv", row.names = FALSE)


# --- C. Regional Summary Table (South Atlantic) ---

summary_2021 <- master_2021 %>%
  # Filter for your specific South Atlantic focus
  filter(State %in% c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida")) %>%
  group_by(State) %>%
  summarise(
    Avg_Income_21 = round(mean(Median.Household.Income, na.rm = TRUE), 0),
    Avg_Health_Rank_21 = round(mean(Clinical_Care_Rank, na.rm = TRUE), 1),
    
    # Premature Mortality Metrics
    Avg_YPLL_Black_21 = round(mean(YPLL_Black, na.rm = TRUE), 0),
    Avg_YPLL_White_21 = round(mean(YPLL_White, na.rm = TRUE), 0),
    
    # The Disparity Gap Calculation
    YPLL_Disparity_Gap_21 = round(mean(YPLL_Black, na.rm = TRUE) - mean(YPLL_White, na.rm = TRUE), 0),
    
    Total_Counties = n()
  ) %>%
  # Sort by State name for a clean, organized table
  arrange(State)

# Save the file with your preferred name
write.csv(summary_2021, "Summary_Baseline_2021.csv", row.names = FALSE)

# --- D. Final Verification ---
print("--- 2021 STATISTICAL PROOFS ---")
print(cor_results_2021)
print("--- 2021 REGIONAL SUMMARY ---")
print(summary_2021)

# --- C. Final 2021 Audit ---
# Proving 'Data Quality' by checking for any remaining NAs in master file
print("Final NA check for 2021 Master:")
print(sum(is.na(master_2021)))

# Exporting a CSV version of your Master File to  folder 
write.csv(master_2021, "Master_2021_Cleaned.csv", row.names = FALSE)


# =========================================================
# SECTION: 2022 DATA PREPARATION AND ANALYSIS
# =========================================================

library(dplyr)

# ---------------------------------------------------------
# STEP 1: PROCESSING AND CLEANING 2022 DATA
# ---------------------------------------------------------

# --- FILE 1: Additional Measure Data ---

data22_measures <- read.csv("data/raw/2022/2022 County Health Rankings Data/Additional Measure Data.csv", 
                            skip = 1, 
                            na.strings = c("N/A", "", " "))

head(data22_measures)

names(data22_measures)  # Lists every column name in the console
View(data22_measures)   # Opens the data in a spreadsheet tab at the top

# Creating the clean 2022 dataset
clean22_measures <- data22_measures %>%
  # 1. Select and Rename based on the headers you found
  # Format: New_Name = Old_Heading_From_CSV
  select(
    FIPS, 
    State,
    County,
    Median.Household.Income,
    X..Rural = X..rural,
    Covid_Deaths_2020 = X..deaths.due.to.COVID.19.during.2020,
    Covid_Death_Rate_22 = COVID.19.death.rate,
    Life_Exp_22 = Life.Expectancy
  ) %>%
  
  # 2. Basic Cleaning: Remove rows where County is empty 
  # This keeps the dataset whole but removes the non-county summary rows
  filter(County != "")

# View the result to ensure it's exactly what you need
View(clean22_measures)

write.csv(clean22_measures, "clean22_measures.csv", row.names = FALSE)

# --- FILE 2: Additional Measure Sources and Years ---

# 1. Read the 2022 raw metadata
data22_metadata <- read.csv("data/raw/2022/2022 County Health Rankings Data/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

# 2. Clean and Rename
clean22_metadata <- data22_metadata %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Description = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  # Filter out empty rows or section headers
  filter(!is.na(Measure) & Measure != "")

# 3. Verification
print("2022 Metadata Successfully Cleaned!")
head(clean22_metadata)

View(clean22_metadata)

write.csv(clean22_metadata, "clean22_metadata.csv", row.names = FALSE)

# --- FILE 3: Outcomes and Factors Rankings ---

# 1. Read the 2022 file

data22_ranked <- read.csv("data/raw/2022/2022 County Health Rankings Data/Outcomes and Factors Rankings.csv", 
                          skip = 1, 
                          na.strings = c("N/A", "", " "))

# 2. Clean and Rename
clean22_ranked <- data22_ranked %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    # Standardizing names for side-by-side comparison with 2021
    Health_Outcomes_Rank = Rank,
    Health_Factors_Rank = Rank.1
  ) %>%
  # Keep only actual counties (removes State/National summary rows)
  filter(County != "") %>% 
  # Drop N/As for counties that weren't ranked
  na.omit()

# 3. Verification
print("--- 2022 RANKINGS CLEANED ---")
print(paste("Number of Ranked Counties:", nrow(clean22_ranked)))
head(clean22_ranked)

View(clean22_ranked)
# 4. Save to Console (Quick Save)
write.csv(clean22_ranked, "clean22_ranked.csv", row.names = FALSE)

# --- FILE 4: Outcomes and Factors SubRankings ---

# 1. Read the 2022 SubRankings file
data22_subranked <- read.csv("data/raw/2022/2022 County Health Rankings Data/Outcomes and Factors SubRankings.csv", 
                             skip = 1, 
                             na.strings = c("N/A", "", " "))

# 2. Clean and Rename
clean22_subranked <- data22_subranked %>%
  dplyr::select(
    FIPS, 
    State, 
    County,
    Length_of_Life_Rank   = Rank,
    Clinical_Care_Rank    = Rank.3, 
    Socioeconomic_Rank    = Rank.4
  ) %>%
  # Remove state-level summary rows
  filter(County != "") %>%
  # Remove unranked counties to keep data tight
  na.omit()

# 3. Verification

head(clean22_subranked)

# 4. SAVE FILE
write.csv(clean22_subranked, "clean22_subranked.csv", row.names = FALSE)

# --- FILE 5: Ranked Measure Data ---

# 1. Read the 2022 Ranked Measure file
data22_rankedmeasures <- read.csv("data/raw/2022/2022 County Health Rankings Data/Ranked Measure Data.csv", 
                                  skip = 1, 
                                  na.strings = c("N/A", "", " "))

# 2. Clean and Rename
clean22_rankedmeasures <- data22_rankedmeasures %>%
  dplyr::select(
    FIPS, State, County,
    YPLL_Rate = Years.of.Potential.Life.Lost.Rate,
    # Note the period at the end of Black. and the lowercase white.
    YPLL_Black = YPLL.Rate..Black.,
    YPLL_White = YPLL.Rate..white., 
    Percent_Poor_Health = X..Fair.or.Poor.Health,
    Percent_Low_Birthweight = X..Low.birthweight
  ) %>%
  filter(County != "") %>%
  # Keep counties with a primary YPLL rate
  filter(!is.na(YPLL_Rate))

# 3. Verification
print("--- 2022 RANKED MEASURES CLEANED ---")
print(paste("Rows available for YPLL analysis:", nrow(clean22_rankedmeasures)))

# 4. SAVE FILE
write.csv(clean22_rankedmeasures, "clean22_rankedmeasures.csv", row.names = FALSE)

# --- FILE 6: Ranked Measure Sources and Years ---

# 1. Read the 2022 metadata file
metadata22 <- read.csv("data/raw/2022/2022 County Health Rankings Data/Ranked Measure Sources and Years.csv", 
                       skip = 0, 
                       na.strings = c("N/A", "", " "))

# 2. Clean and Rename

clean22_metadata_ranked <- metadata22 %>%
  dplyr::select(
    Category    = Focus.Area,
    Measure     = Measure,
    Definition  = Description,
    Source      = Source,
    Years       = Year.s.
  ) %>%
  # Filter out empty rows to keep the dictionary clean
  filter(!is.na(Measure) & Measure != "")

# 3. Verification
print("--- 2022 RANKED METADATA CLEANED ---")
head(clean22_metadata_ranked)

# 4. SAVE FILE 
write.csv(clean22_metadata_ranked, "metadata22.csv", row.names = FALSE)

# ---------------------------------------------------------
# STEP 2 (2022): INTEGRATING 2022 DATA
# ---------------------------------------------------------

# 1. Join all 2022 files strictly by FIPS 
master_2022 <- clean22_measures %>%
  inner_join(clean22_ranked, by = "FIPS") %>%
  inner_join(clean22_subranked, by = "FIPS") %>%
  inner_join(clean22_rankedmeasures, by = "FIPS")

# 2. Cleanup: Standardize names and remove the .x/.y duplicates
master_2022 <- master_2022 %>%
  rename(State = State.x, County = County.x) %>%
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"))

# 3. Final Check for Join Success
print("--- MASTER 2022 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final 2022 Row Count:", nrow(master_2022)))

# ---------------------------------------------------------
# B. DATA TYPE AUDIT (2022)
# ---------------------------------------------------------


# 1. Force Rankings and Outcomes to Numeric
master_2022$Health_Factors_Rank  <- as.numeric(as.character(master_2022$Health_Factors_Rank))
master_2022$Health_Outcomes_Rank <- as.numeric(as.character(master_2022$Health_Outcomes_Rank))
master_2022$Clinical_Care_Rank   <- as.numeric(as.character(master_2022$Clinical_Care_Rank))
master_2022$Socioeconomic_Rank   <- as.numeric(as.character(master_2022$Socioeconomic_Rank))

# 2. Force Economic and Quality of Life Measures
master_2022$Median.Household.Income <- as.numeric(as.character(master_2022$Median.Household.Income))
master_2022$Percent_Poor_Health     <- as.numeric(as.character(master_2022$Percent_Poor_Health))

# 3. Force Racial Disparity Metrics (YPLL)
master_2022$YPLL_Rate  <- as.numeric(as.character(master_2022$YPLL_Rate))
master_2022$YPLL_Black <- as.numeric(as.character(master_2022$YPLL_Black))
master_2022$YPLL_White <- as.numeric(as.character(master_2022$YPLL_White))

master_2022$X..Rural <- as.numeric(as.character(master_2022$X..Rural))

# 4. PREPROCESSING
master_2022 <- na.omit(master_2022)

# 5. VERIFICATION
print("2022 Audit Successful. Verified Numeric Columns:")
str(master_2022[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])

# 6. SAVE THE MASTER
write.csv(master_2022, "Master_2022_Final.csv", row.names = FALSE)



# --- C. EDA PLOT (2022): INCOME VS. CLINICAL CARE ---
target_states <- c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida") 

plot_data_22 <- master_2022 %>% 
  filter(State %in% target_states)

ggplot(plot_data_22, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(alpha = 0.2, color = "midnightblue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~State, scales = "free_x") + 
  scale_x_continuous(labels = function(x) paste0(x/1000, "k")) +
  scale_y_continuous(breaks = seq(0, 200, by = 50)) + 
  labs(title = "EDA: Healthcare Access Trends by State (2022)",
       subtitle = "Relationship between Median Income and Clinical Care Ranking",
       x = "Median Household Income ($k)",
       y = "Clinical Care Rank (1 = Best)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )

ggsave("2022_Socioeconomic_vs_Health_EDA.png", width = 10, height = 7, dpi = 300)

# --- F. EDA PLOT (2022): RACIAL DISPARITY ---
racial_data_22 <- master_2022 %>%
  filter(State %in% target_states) %>%
  select(State, YPLL_Black, YPLL_White) %>%
  pivot_longer(cols = c(YPLL_Black, YPLL_White), 
               names_to = "Race", 
               values_to = "YPLL_Rate") %>%
  na.omit()

ggplot(racial_data_22, aes(x = State, y = YPLL_Rate, fill = Race)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("midnightblue", "darkorange"), 
                    labels = c("Black population", "White population")) +
  labs(title = "Regional Premature Mortality Disparities (2022)",
       subtitle = "Comparison of Black and White Years of Potential Life Lost (YPLL)",
       caption = "Data Source: County Health Rankings 2022",
       x = "State",
       y = "YPLL Rate (Years Lost per 100k)",
       fill = "Demographic Group") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(face = "bold"))

ggsave("2022_Racial_Disparity_YPLL_Plot.png", width = 10, height = 6)

# --- G.  EDA PLOT: HEALTH OUTCOME VS HEALTH FACTOR  ---

# 1. CREATE THE PLOT DATA: Filter for South Atlantic and remove NAs
plot2_data_22 <- master_2022 %>%
  filter(State %in% c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida")) %>%
  filter(!is.na(Health_Factors_Rank) & !is.na(Health_Outcomes_Rank))

# 2. GENERATE THE PLOT
ggplot(plot2_data_22, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  # Using jitter to prevent point-stacking
  geom_jitter(alpha = 0.4, color = "darkgreen", width = 2, height = 2) +
  
  # Trend line to show the 2022 relationship
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  
  # Clean axes
  scale_y_continuous(breaks = seq(0, 150, by = 50)) + 
  scale_x_continuous(breaks = seq(0, 150, by = 50)) +
  
  # Separate by state
  facet_wrap(~State) + 
  
  labs(title = "Predictors vs. Current Health Outcomes (2022)",
       subtitle = "Relationship between Health Factors and Current County Rankings",
       x = "Health Factors Rank (1 = Best Environment)",
       y = "Health Outcomes Rank (1 = Best Health)") +
  theme_light() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# 3. Save the image
ggsave("2022_Outcomes_vs_Factors_Regional.png", width = 10, height = 7)


# --- H.  EDA PLOT: HEALTH OUTCOME VS HEALTH FACTOR (ALL STATES)  ---

# 1. DATA QUALITY: Remove NAs for the national trend
all_states_22_plot_data <- master_2022 %>%
  filter(!is.na(Health_Factors_Rank) & !is.na(Health_Outcomes_Rank))

# 2. GENERATE THE COMPREHENSIVE PLOT
ggplot(all_states_22_plot_data, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  # Micro-jitter for thousands of data points
  geom_jitter(alpha = 0.2, color = "darkgreen", size = 0.5, width = 1.5, height = 1.5) +
  
  # National trend line
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, size = 0.5) +
  
  # 100-unit breaks for the larger national scale
  scale_y_continuous(breaks = seq(0, 300, by = 100)) + 
  scale_x_continuous(breaks = seq(0, 300, by = 100)) +
  
  # Facet by State (all 50)
  facet_wrap(~State) + 
  
  labs(title = "National Health Predictors vs. Outcomes (2022)",
       subtitle = "Relationship between Health Factors and Current County Rankings across All States",
       x = "Health Factors Rank (1 = Best)",
       y = "Health Outcomes Rank (1 = Best)") +
  
  theme_light() +
  theme(
    strip.text = element_text(size = 6, face = "bold"), 
    axis.text = element_text(size = 5),                
    axis.title = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

# 3. Save high-res version (wider for 50 states)
ggsave("2022_National_Outcomes_vs_Factors.png", width = 20, height = 15, dpi = 300)



# --- I. 2022 EDA: Clinical Care Rank vs. Mortality ---


master_five_states_2022 <- master_2022 %>%
  filter(State %in% c("Maryland", "Virginia", "North Carolina", "Georgia", "Florida"))

# Plot: Clinical Care Access vs. Mortality (2022)
ggplot(master_five_states_2022, aes(x = Clinical_Care_Rank, y = YPLL_Rate)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  facet_wrap(~State) + 
  labs(title = "Healthcare Access vs. Mortality: South Atlantic Focus (2022)",
       subtitle = "Relationship between Clinical Care Rank and YPLL Rate",
       x = "Clinical Care Rank (Lower = Better Access)",
       y = "YPLL Rate (Years Lost per 100k)") +
  theme_minimal()

# Save 
ggsave("Clinical_Care_vs_YPLL_South_Atlantic_2022.png", width = 10, height = 7, dpi = 300)


# --- J. 2022 Rural vs Urban YPLL Comparison ---

#  state-relative median threshold for the 2022 data
regional_comparison_2022 <- master_2022 %>%
  filter(State %in% c("Maryland", "Virginia", "North Carolina", "Georgia", "Florida")) %>%
  group_by(State) %>%
  mutate(Median_Rural = median(X..Rural, na.rm = TRUE)) %>%
  # relative split based on 2022 rurality distributions
  mutate(County_Type = ifelse(X..Rural > Median_Rural, "Higher Rurality", "Lower Rurality")) %>%
  ungroup() %>%
  mutate(County_Type = factor(County_Type, levels = c("Lower Rurality", "Higher Rurality")))


regional_comparison_2022 %>% group_by(State, County_Type) %>% tally()

# Plot: Relative Rurality Comparison (2022)
ggplot(regional_comparison_2022, aes(x = State, y = YPLL_Rate, fill = County_Type)) +
  geom_violin(position = position_dodge(width = 0.8, preserve = "single"), alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.8, preserve = "single"), outlier.shape = 1) +
  scale_fill_manual(values = c("Higher Rurality" = "#5e3c99", "Lower Rurality" = "#fdb863")) +
  labs(title = "Regional Mortality by Relative Rurality (2022)",
       subtitle = "Comparing Counties Above/Below State Median Rurality",
       x = "State",
       y = "YPLL Rate",
       fill = "Classification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save 
ggsave("South_Atlantic_Relative_Rurality_2022.png", width = 12, height = 8, dpi = 300)


# ---------------------------------------------------------
# STEP 3: 2022 ANALYTICAL BASELINE & SUMMARY
# ---------------------------------------------------------



# 1. Correlation Proofs
cor_income_health_22 <- cor.test(master_2022$Median.Household.Income, master_2022$Clinical_Care_Rank, use = "complete.obs")
cor_factors_outcomes_22 <- cor.test(master_2022$Health_Factors_Rank, master_2022$Health_Outcomes_Rank, use = "complete.obs")
cor_racial_ypll_22 <- cor.test(master_2022$YPLL_Black, master_2022$YPLL_White, use = "complete.obs")

# 2. Combined Coefficient Table
cor_results_2022 <- data.frame(
  Analysis_Type = c("Income vs Clinical Care", "Factors vs Outcomes", "Black vs White YPLL"),
  Correlation_Coefficient = c(cor_income_health_22$estimate, cor_factors_outcomes_22$estimate, cor_racial_ypll_22$estimate),
  P_Value = c(cor_income_health_22$p.value, cor_factors_outcomes_22$p.value, cor_racial_ypll_22$p.value),
  Year = 2022
)

write.csv(cor_results_2022, "2022_Statistical_Proofs.csv", row.names = FALSE)

# 3. Regional Summary Table
summary_2022 <- master_2022 %>%
  filter(State %in% target_states) %>%
  group_by(State) %>%
  summarise(
    Avg_Income_22 = round(mean(Median.Household.Income, na.rm = TRUE), 0),
    Avg_Health_Rank_22 = round(mean(Clinical_Care_Rank, na.rm = TRUE), 1),
    Avg_YPLL_Black_22 = round(mean(YPLL_Black, na.rm = TRUE), 0),
    Avg_YPLL_White_22 = round(mean(YPLL_White, na.rm = TRUE), 0),
    YPLL_Disparity_Gap_22 = round(mean(YPLL_Black, na.rm = TRUE) - mean(YPLL_White, na.rm = TRUE), 0),
    Total_Counties = n()
  ) %>%
  arrange(State)

write.csv(summary_2022, "Summary_Baseline_2022.csv", row.names = FALSE)



# =========================================================
# SECTION: 2023 DATA PREPARATION AND ANALYSIS
# =========================================================

library(dplyr)

# --- FILE 1: Additional Measure Data ---
data23_measures_raw <- read.csv("data/raw/2023/2023 County Health Rankings Data/Additional Measure Data.csv", 
                                skip = 1, 
                                na.strings = c("N/A", "", " "))

clean23_measures <- data23_measures_raw %>%
  dplyr::select(
    FIPS, State, County,
    Median.Household.Income,
    # Renaming 'Uninsured Adults' to the standard name so graphs don't break
    X..Uninsured = X..Uninsured.Adults,
  
    X..Rural = X..Rural
  ) %>%
  filter(County != "")

clean23_measures$Year <- 2023
write.csv(clean23_measures, "clean23_measures.csv", row.names = FALSE)


# --- FILE 2: Metadata - Additional Measures ---
data23_metadata <- read.csv("data/raw/2023/2023 County Health Rankings Data/Addtl Measure Sources and Years.csv", 
                            skip = 0, 
                            na.strings = c("N/A", "", " "))

clean23_metadata <- data23_metadata %>%
  dplyr::select(Category = Focus.Area, Measure, Description, Source, Years = Year.s.) %>%
  filter(!is.na(Measure) & Measure != "")

write.csv(clean23_metadata, "clean23_metadata.csv", row.names = FALSE)


# --- FILE 3: Outcomes and Factors Rankings ---
data23_ranked <- read.csv("data/raw/2023/2023 County Health Rankings Data/Outcomes and Factors Rankings.csv", 
                          skip = 1, 
                          na.strings = c("N/A", "", " "))

clean23_ranked <- data23_ranked %>%
  dplyr::select(FIPS, State, County, 
                Health_Outcomes_Rank = Rank, 
                Health_Factors_Rank = Rank.1) %>%
  filter(County != "") %>%
  na.omit()

write.csv(clean23_ranked, "clean23_ranked.csv", row.names = FALSE)

# --- FILE 4: Outcomes and Factors SubRankings ---
data23_subranked <- read.csv("data/raw/2023/2023 County Health Rankings Data/Outcomes and Factors SubRankings.csv", 
                             skip = 1, 
                             na.strings = c("N/A", "", " "))

clean23_subranked <- data23_subranked %>%
  dplyr::select(FIPS, State, County,
                Length_of_Life_Rank = Rank,
                Clinical_Care_Rank = Rank.3, 
                Socioeconomic_Rank = Rank.4) %>%
  filter(County != "") %>%
  na.omit()

write.csv(clean23_subranked, "clean23_subranked.csv", row.names = FALSE)

# --- FILE 5: Ranked Measure Data ---
data23_rankedmeasures <- read.csv("data/raw/2023/2023 County Health Rankings Data/Ranked Measure Data.csv", 
                                  skip = 1, 
                                  na.strings = c("N/A", "", " "))

clean23_rankedmeasures <- data23_rankedmeasures %>%
  dplyr::select(
    FIPS, State, County,
    YPLL_Rate = contains("Potential.Life.Lost.Rate"),
    YPLL_Black = contains("YPLL.Rate..Black"),
    YPLL_White = contains("YPLL.Rate..white"), # Checks lowercase first
    Percent_Poor_Health = contains("Fair.or.Poor.Health"),
    Percent_Low_Birthweight = contains("Low.birthweight")
  ) %>%
  filter(County != "") %>%
  filter(!is.na(YPLL_Rate))

write.csv(clean23_rankedmeasures, "clean23_rankedmeasures.csv", row.names = FALSE)


# --- FILE 6: Metadata - Ranked Measures ---
metadata23 <- read.csv("data/raw/2023/2023 County Health Rankings Data/Ranked Measure Sources and Years.csv", 
                       skip = 0, 
                       na.strings = c("N/A", "", " "))

clean23_metadata_ranked <- metadata23 %>%
  dplyr::select(Category = Focus.Area, Measure, Definition = Description, Source, Years = Year.s.) %>%
  filter(!is.na(Measure) & Measure != "")

write.csv(clean23_metadata_ranked, "metadata23.csv", row.names = FALSE)

# ---------------------------------------------------------
# STEP 2 (2023): INTEGRATING 2023 DATA
# ---------------------------------------------------------

master_2023 <- clean23_measures %>%
  inner_join(clean23_ranked, by = "FIPS") %>%
  inner_join(clean23_subranked, by = "FIPS") %>%
  inner_join(clean23_rankedmeasures, by = "FIPS")

# 2. Standardize names and remove the .x/.y duplicates
master_2023 <- master_2023 %>%
  # Rename the multiple matches from the Join to our standard names
  rename(
    State = State.x, 
    County = County.x,
    YPLL_Black = YPLL_Black1, # Selecting the primary rate
    YPLL_White = YPLL_White1  # Selecting the primary rate
  ) %>%
  # Remove duplicate suffixes and extra YPLL matches (Black2, White2, etc.)
  select(-ends_with(".y"), -ends_with(".x.x"), -ends_with(".y.y"), 
         -contains("Black2"), -contains("Black3"), -contains("Black4"),
         -contains("White2"), -contains("White3"), -contains("White4"))

# 3. Final Check for Join success
print("--- MASTER 2023 SUCCESSFULLY ESTABLISHED ---")
print(paste("Final 2023 Row Count:", nrow(master_2023)))


# ---------------------------------------------------------
# B. DATA TYPE AUDIT (2023)
# ---------------------------------------------------------

# 1. Force Rankings and Outcomes to Numeric
master_2023$Health_Factors_Rank  <- as.numeric(as.character(master_2023$Health_Factors_Rank))
master_2023$Health_Outcomes_Rank <- as.numeric(as.character(master_2023$Health_Outcomes_Rank))
master_2023$Clinical_Care_Rank   <- as.numeric(as.character(master_2023$Clinical_Care_Rank))
master_2023$Socioeconomic_Rank   <- as.numeric(as.character(master_2023$Socioeconomic_Rank))

# 2. Force Economic and Quality of Life Measures
master_2023$Median.Household.Income <- as.numeric(as.character(master_2023$Median.Household.Income))
master_2023$Percent_Poor_Health     <- as.numeric(as.character(master_2023$Percent_Poor_Health))

# 3. Force Racial Disparity Metrics (YPLL)
master_2023$YPLL_Rate  <- as.numeric(as.character(master_2023$YPLL_Rate))
master_2023$YPLL_Black <- as.numeric(as.character(master_2023$YPLL_Black))
master_2023$YPLL_White <- as.numeric(as.character(master_2023$YPLL_White))

# 4. PREPROCESSING: Remove any "NR" or missing rows created by the numeric force
master_2023 <- na.omit(master_2023)

# 5. VERIFICATION
print("2023 Audit Successful. Verified Numeric Columns:")
str(master_2023[c("YPLL_Black", "Health_Factors_Rank", "Median.Household.Income")])

# 6. SAVE THE MASTER TO CONSOLE/PROJECT FOLDER
write.csv(master_2023, "Master_2023_Final.csv", row.names = FALSE)


# --- C. EDA PLOT (2023): INCOME VS. CLINICAL CARE ---
target_states <- c("Virginia", "North Carolina", "Maryland", "Georgia", "Florida") 

plot_data_23 <- master_2023 %>% 
  filter(State %in% target_states)

ggplot(plot_data_23, aes(x = Median.Household.Income, y = Clinical_Care_Rank)) +
  geom_point(alpha = 0.2, color = "midnightblue", size = 1) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~State, scales = "free_x") + 
  scale_x_continuous(labels = function(x) paste0(x/1000, "k")) +
  scale_y_continuous(breaks = seq(0, 200, by = 50)) + 
  labs(title = "EDA: Healthcare Access Trends by State (2023)",
       subtitle = "Relationship between Median Income and Clinical Care Ranking",
       x = "Median Household Income ($k)",
       y = "Clinical Care Rank (1 = Best)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    strip.text = element_text(face = "bold")
  )

ggsave("2023_Socioeconomic_vs_Health_EDA.png", width = 10, height = 7, dpi = 300)

# --- F. EDA PLOT (2023): RACIAL DISPARITY ---
racial_data_23 <- master_2023 %>%
  filter(State %in% target_states) %>%
  select(State, YPLL_Black, YPLL_White) %>%
  pivot_longer(cols = c(YPLL_Black, YPLL_White), 
               names_to = "Race", 
               values_to = "YPLL_Rate") %>%
  na.omit()

ggplot(racial_data_23, aes(x = State, y = YPLL_Rate, fill = Race)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("midnightblue", "darkorange"), 
                    labels = c("Black population", "White population")) +
  labs(title = "Regional Premature Mortality Disparities (2023)",
       subtitle = "Comparison of Black and White Years of Potential Life Lost (YPLL)",
       caption = "Data Source: County Health Rankings 2023",
       x = "State",
       y = "YPLL Rate (Years Lost per 100k)",
       fill = "Demographic Group") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(face = "bold"))

ggsave("2023_Racial_Disparity_YPLL_Plot.png", width = 10, height = 6)

# --- G.  EDA PLOT: HEALTH OUTCOME VS HEALTH FACTOR (2023) ---

# 1. CREATE THE PLOT DATA
plot2_data_23 <- master_2023 %>%
  filter(State %in% target_states) %>%
  filter(!is.na(Health_Factors_Rank) & !is.na(Health_Outcomes_Rank))

# 2. GENERATE THE PLOT
ggplot(plot2_data_23, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  geom_jitter(alpha = 0.4, color = "darkgreen", width = 2, height = 2) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE) +
  scale_y_continuous(breaks = seq(0, 150, by = 50)) + 
  scale_x_continuous(breaks = seq(0, 150, by = 50)) +
  facet_wrap(~State) + 
  labs(title = "Predictors vs. Current Health Outcomes (2023)",
       subtitle = "Relationship between Health Factors and Current County Rankings",
       x = "Health Factors Rank (1 = Best Environment)",
       y = "Health Outcomes Rank (1 = Best Health)") +
  theme_light() +
  theme(
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("2023_Outcomes_vs_Factors_Regional.png", width = 10, height = 7)


# --- H.  EDA PLOT: HEALTH OUTCOME VS HEALTH FACTOR (ALL STATES 2023) ---

all_states_23_plot_data <- master_2023 %>%
  filter(!is.na(Health_Factors_Rank) & !is.na(Health_Outcomes_Rank))

ggplot(all_states_23_plot_data, aes(x = Health_Factors_Rank, y = Health_Outcomes_Rank)) +
  geom_jitter(alpha = 0.2, color = "darkgreen", size = 0.5, width = 1.5, height = 1.5) +
  geom_smooth(method = "lm", color = "red", linetype = "dashed", se = FALSE, size = 0.5) +
  scale_y_continuous(breaks = seq(0, 300, by = 100)) + 
  scale_x_continuous(breaks = seq(0, 300, by = 100)) +
  facet_wrap(~State) + 
  labs(title = "National Health Predictors vs. Outcomes (2023)",
       subtitle = "Relationship between Health Factors and Current County Rankings across All States",
       x = "Health Factors Rank (1 = Best)",
       y = "Health Outcomes Rank (1 = Best)") +
  theme_light() +
  theme(
    strip.text = element_text(size = 6, face = "bold"), 
    axis.text = element_text(size = 5),                
    axis.title = element_text(size = 8),
    panel.grid.minor = element_blank()
  )

ggsave("2023_National_Outcomes_vs_Factors.png", width = 20, height = 15, dpi = 300)

# --- I. 2023 EDA: Clinical Care Rank vs. Mortality ---

master_five_states_2023 <- master_2023 %>%
  filter(State %in% target_states)

ggplot(master_five_states_2023, aes(x = Clinical_Care_Rank, y = YPLL_Rate)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  facet_wrap(~State) + 
  labs(title = "Healthcare Access vs. Mortality: South Atlantic Focus (2023)",
       subtitle = "Relationship between Clinical Care Rank and YPLL Rate",
       x = "Clinical Care Rank (Lower = Better Access)",
       y = "YPLL Rate (Years Lost per 100k)") +
  theme_minimal()

ggsave("Clinical_Care_vs_YPLL_South_Atlantic_2023.png", width = 10, height = 7, dpi = 300)

# --- J. 2023 Rural vs Urban YPLL Comparison ---

regional_comparison_2023 <- master_2023 %>%
  filter(State %in% target_states) %>%
  group_by(State) %>%
  mutate(Median_Rural = median(X..Rural, na.rm = TRUE)) %>%
  mutate(County_Type = ifelse(X..Rural > Median_Rural, "Higher Rurality", "Lower Rurality")) %>%
  ungroup() %>%
  mutate(County_Type = factor(County_Type, levels = c("Lower Rurality", "Higher Rurality")))

regional_comparison_2023 %>% group_by(State, County_Type) %>% tally()

ggplot(regional_comparison_2023, aes(x = State, y = YPLL_Rate, fill = County_Type)) +
  geom_violin(position = position_dodge(width = 0.8, preserve = "single"), alpha = 0.5) +
  geom_boxplot(width = 0.2, position = position_dodge(width = 0.8, preserve = "single"), outlier.shape = 1) +
  scale_fill_manual(values = c("Higher Rurality" = "#5e3c99", "Lower Rurality" = "#fdb863")) +
  labs(title = "Regional Mortality by Relative Rurality (2023)",
       subtitle = "Comparing Counties Above/Below State Median Rurality",
       x = "State",
       y = "YPLL Rate",
       fill = "Classification") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("South_Atlantic_Relative_Rurality_2023.png", width = 12, height = 8, dpi = 300)







# ---------------------------------------------------------
# STEP 3: 2023 ANALYTICAL BASELINE & SUMMARY
# ---------------------------------------------------------



# 1. Correlation Proofs (Mathematical evidence for RQs)
cor_income_health_23 <- cor.test(master_2023$Median.Household.Income, master_2023$Clinical_Care_Rank, use = "complete.obs")
cor_factors_outcomes_23 <- cor.test(master_2023$Health_Factors_Rank, master_2023$Health_Outcomes_Rank, use = "complete.obs")
cor_racial_ypll_23 <- cor.test(master_2023$YPLL_Black, master_2023$YPLL_White, use = "complete.obs")

# 2. Combined Coefficient Table for 2023
cor_results_2023 <- data.frame(
  Analysis_Type = c("Income vs Clinical Care", "Factors vs Outcomes", "Black vs White YPLL"),
  Correlation_Coefficient = c(cor_income_health_23$estimate, cor_factors_outcomes_23$estimate, cor_racial_ypll_23$estimate),
  P_Value = c(cor_income_health_23$p.value, cor_factors_outcomes_23$p.value, cor_racial_ypll_23$p.value),
  Year = 2023
)

# Export results for your "Statistical Analysis" chapter
write.csv(cor_results_2023, "2023_Statistical_Proofs.csv", row.names = FALSE)

# 3. Regional Summary Table: South Atlantic Focus
summary_2023 <- master_2023 %>%
  filter(State %in% target_states) %>%
  group_by(State) %>%
  summarise(
    Avg_Income_23 = round(mean(Median.Household.Income, na.rm = TRUE), 0),
    Avg_Health_Rank_23 = round(mean(Clinical_Care_Rank, na.rm = TRUE), 1),
    Avg_YPLL_Black_23 = round(mean(YPLL_Black, na.rm = TRUE), 0),
    Avg_YPLL_White_23 = round(mean(YPLL_White, na.rm = TRUE), 0),
    YPLL_Disparity_Gap_23 = round(mean(YPLL_Black, na.rm = TRUE) - mean(YPLL_White, na.rm = TRUE), 0),
    Total_Counties = n()
  ) %>%
  arrange(State)

# Export the final summary for your "Results" tables
write.csv(summary_2023, "Summary_Baseline_2023.csv", row.names = FALSE)

