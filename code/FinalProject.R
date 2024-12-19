# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(readr)
library(kableExtra)


#--------------------------------------[Data loading and pre-processing]--------------------------------------#
#Please download DecDataset2020, DecDataset2023 dataset from https://github.com/Stat184-Fall2024/Sec1_FP_FeliciaVijayarangam_VarshaGiridharan/tree/main/data
original2020 <- "/Users/varshagiridharan/Downloads/STAT184_FinalProject_Datasets/DecDataset2020.csv"
flights2020 <- read.csv(original2020)

# Loading the 2020, 2023 flight data from CSV file
original2023 <- "/Users/varshagiridharan/Downloads/STAT184_FinalProject_Datasets/DecDataset2023.csv"
flights2023 <- read.csv(original2023)

christmas_week2020 <- flights2020 %>%
  filter(DAY_OF_MONTH >= 20 & DAY_OF_MONTH <= 27) 

#Filter December 2023:
christmas_week2023 <- flights2023 %>%
  filter(DAY_OF_MONTH >= 20 & DAY_OF_MONTH <= 27) 

#EDA
# Flight Summary of Christmas week in 2020
summary2020 <- summary(christmas_week2020)
summary2020_transposed <- t(summary2020)

# 'L_UNIQUE_CARRIERS.csv' has the columns 'Code' (airline code) and 'Description' (full airline name)
code <- "/Users/varshagiridharan/Downloads/STAT184_FinalProject_Datasets/L_UNIQUE_CARRIERS.csv" 
airlineCode <- read.csv(code)

#--------------------------------------[Data visualizations]--------------------------------------#


# PDF output Table
kable(summary2020_transposed, caption = "Flight Summary Table of Christmas Week 2020") %>%
  #Style the format such as table heading font and position
  kable_styling(
    latex_options = c("striped", "scale_down"), 
    font_size = 8,                              
    position = "center"                          
  ) %>%
  #Looks at the columns and rows and adjust the lengths accordingly
  column_spec(1, bold = TRUE, width = "4cm") %>%  
  column_spec(2:ncol(summary2020_transposed), width = "1.5cm") %>% 
  row_spec(0, bold = TRUE, background = "#D3D3D3") 



# Flight Summary Table for Christmas week in 2023
summary2023 <- summary(christmas_week2023)
summary2023_transposed <- t(summary2023)

# PDF output Table
kable(summary2023_transposed, caption = "Flight Summary Table of Christmas Week 2020") %>%
  kable_styling(
    latex_options = c("striped", "scale_down"),  # Use LaTeX-specific options
    font_size = 8,                               # Adjust font size
    position = "center"                          # Align to center by default
  ) %>%
  column_spec(1, bold = TRUE, width = "4cm") %>%  # Adjust the first column width
  column_spec(2:ncol(summary2023_transposed), width = "1.5cm") %>% # Adjust other columns
  row_spec(0, bold = TRUE, background = "#D3D3D3") # Style header row



# Merge the datasets based on the airline code
airlineInfo2020 <- christmas_week2020 %>%
  left_join(airlineCode, by = c("OP_UNIQUE_CARRIER" = "Code"))


avgDelay_airlineDay2020 <- airlineInfo2020 %>%
  group_by(Description, DAY_OF_MONTH) %>%
  summarize(AverageDelay = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")

specific_airlines <- c("Delta Air Lines Inc.", "Alaska Airlines Inc.", "Hawaiian Airlines Inc.", 
                       "United Air Lines Inc.", "American Airlines Inc.", "JetBlue Airways", 
                       "Southwest Airlines Co.", "Spirit Airlines", "Allegiant Air",
                       "Frontier Airlines Inc.")


ggplot(avgDelay_airlineDay2020 %>% filter(Description %in% specific_airlines), 
       aes(x = factor(DAY_OF_MONTH), y = AverageDelay, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Departure Delay by Airline for Christmas Week 2020",
       x = "Days of Christmas Week",
       y = "Average Delay (Minutes)",
       fill = "Airline") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10), 
    legend.position = "bottom",
    legend.text = element_text(size = 5))

airlineInfo2023 <- christmas_week2023 %>%
  left_join(airlineCode, by = c("OP_UNIQUE_CARRIER" = "Code"))


# Calculate the average delay time by airline
avgDelay_airlineDay2023 <- airlineInfo2023 %>%
  group_by(Description, DAY_OF_MONTH) %>%
  summarize(AverageDelay2023 = mean(DEP_DELAY_NEW, na.rm = TRUE), .groups = "drop")

specific_airlines2023 <- c("Delta Air Lines Inc.", "Alaska Airlines Inc.", "Hawaiian Airlines Inc.", 
                           "United Air Lines Inc.", "American Airlines Inc.", "JetBlue Airways", 
                           "Southwest Airlines Co.", "Spirit Airlines", "Allegiant Air",
                           "Frontier Airlines Inc.")


#Plotting Average 
ggplot(avgDelay_airlineDay2023 %>% filter(Description %in% specific_airlines2023), 
       aes(x = factor(DAY_OF_MONTH), y = AverageDelay2023, fill = Description)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Departure Delay by Airline for Christmas Week 2023",
       x = "Days of Christmas Week",
       y = "Average Delay (Minutes)",
       fill = "Airline") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size =10 ), 
    legend.position = "bottom",
    legend.text = element_text(size = 5)
  )


# Calculate average delays for Allegiant Air
avgDelayAlleg2023 <- airlineInfo2023 %>%
  filter(Description %in% specific_airlines2023) %>%
  group_by(Description) %>%
  summarize(
    Departure = mean(DEP_DELAY_NEW, na.rm = TRUE),
    Arrival = mean(ARR_DELAY_NEW, na.rm = TRUE),
    Carrier = mean(CARRIER_DELAY, na.rm = TRUE),
    Weather = mean(WEATHER_DELAY, na.rm = TRUE),
    NAS = mean(NAS_DELAY, na.rm = TRUE),
    Security = mean(SECURITY_DELAY, na.rm = TRUE),
    LateAircraft = mean(LATE_AIRCRAFT_DELAY, na.rm = TRUE),
    Diverted = mean(DIV_ARR_DELAY, na.rm = TRUE),
    .groups = "drop"
  )

kable(avgDelayAlleg2023, caption = "Average Delay (in minutes) Based on Types of Delays for the Top 10 Airlines Christmas Weeks 2023") %>%
  kable_styling(
    latex_options = c("striped", "scale_down"),
    font_size = 8,
    position = "center"
  ) 

# 1. Most popular airports in the week of Christmas for 2023 

# Create a dataframe that counts the number of flights and calculates average delays 
# by ORIGIN (airport)
flights_by_airport_2023 <- christmas_week2023 %>%
  group_by(ORIGIN) %>%
  summarise(
    Number_of_Flights = n(),
    Avg_Weather_Delay = median(WEATHER_DELAY, na.rm = TRUE),
    Avg_Carrier_Delay = median(CARRIER_DELAY, na.rm = TRUE)
  ) %>%
  arrange(desc(Number_of_Flights))

# Filter for the top 10 busiest airports
top10_flights_by_airport_2023 <- flights_by_airport_2023 %>%
  slice_max(order_by = Number_of_Flights, n = 10) %>%
  left_join(airlineInfo2023 %>% select(ORIGIN), 
            by = "ORIGIN") %>%
  distinct()

# 2. Most popular airlines in the week of Christmas for 2023 

# Create a dataframe that counts the number of flights and calculates 
# average delays by Airline Name
flights_by_airline_2023 <- christmas_week2023 %>%
  group_by(OP_UNIQUE_CARRIER) %>%
  summarise(
    Number_of_Flights = n(),
    Avg_Weather_Delay = median(WEATHER_DELAY, na.rm = TRUE),
    Avg_Carrier_Delay = median(CARRIER_DELAY, na.rm = TRUE)
  ) %>%
  arrange(desc(Number_of_Flights))

# Filter for the top 10 airlines by number of flights
top10_flights_by_airline_2023 <- flights_by_airline_2023 %>%
  slice_max(order_by = Number_of_Flights, n = 10) %>%
  left_join(airlineInfo2023 %>% select(OP_UNIQUE_CARRIER), 
            by = "OP_UNIQUE_CARRIER") %>%
  distinct()

# 3. Compare mean weather delays at top 10 airports for 2023 vs. 2020

# Research Question: How did mean weather delays at the top 10 busiest airports during the 
# Christmas travel week compare between 2020 and 2023?

bind_rows(
  christmas_week2023 %>% mutate(Year = 2023),
  christmas_week2020 %>% mutate(Year = 2020)
) %>%
  filter(ORIGIN %in% top10_flights_by_airport_2023$ORIGIN) %>%
  group_by(ORIGIN, Year) %>%
  summarise(Mean_Weather_Delay = mean(WEATHER_DELAY, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Mean_Weather_Delay, y = reorder(ORIGIN, Mean_Weather_Delay), fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  labs(
    title = "Mean Weather Delays at Top 10 Airports (2023 vs. 2020)",
    x = "Mean Weather Delay (minutes)",
    y = "Airport (IATA Code)",
    fill = "Year"
  ) +
  theme_minimal()


# Research Question: How did the average delays (weather and carrier delays) differ between 
#Pittsburgh (PIT) and Philadelphia (PHL) during the week of Christmas in 2023 compared to 2020?

bind_rows(
  christmas_week2023 %>% mutate(Year = 2023),
  christmas_week2020 %>% mutate(Year = 2020)
) %>%
  filter(ORIGIN %in% c("PIT", "PHL"), DAY_OF_MONTH >= 20 & DAY_OF_MONTH <= 27) %>%
  pivot_longer(cols = c(CARRIER_DELAY, WEATHER_DELAY), names_to = "Delay_Type", values_to = "Delay_Value") %>%
  group_by(DAY_OF_MONTH, ORIGIN, Delay_Type, Year) %>%
  summarise(Mean_Delay = mean(Delay_Value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = DAY_OF_MONTH, y = Mean_Delay, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ ORIGIN, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Mean Delay by Type, Day, and Airport (Pittsburgh and Philadelphia, 2023 vs. 2020)",
    x = "Day of the Month",
    y = "Mean Delay (minutes)",
    fill = "Year"
  )
