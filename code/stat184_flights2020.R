#Load necessary libraries
library(dplyr)
library(readr)
library(esquisse)
library(tidyr)
library(ggcorrplot)
library(reshape2)


#--------------------------------------------[Data loading and pre-processing]--------------------------------------------#

# Read data into environment
flights2020 <- read_csv("~/Downloads/STAT184_FinalProject_Datasets/bts_dataset_2020.csv")

# Filter to view data between 20th and 27th December
christmas_week_2020_data <- flights2020 %>%
  filter(DAY_OF_MONTH >= 20 & DAY_OF_MONTH <= 27)

#EDA
head(flights2020)
summary(flights2020)

# clean data frame
cleandf_2020_data <- na.omit(christmas_week_2020_data)

# 'L_UNIQUE_CARRIERS.csv' has the columns 'Code' (airline code) and 'Description' (full airline name)
airlineCode <- read.csv("~/Downloads/STAT184_FinalProject_Datasets/L_UNIQUE_CARRIERS.csv" )

# 'airports.csv' has the columns 'IATA' and 'Airport name'
airportCode <- read.csv("~/Downloads/STAT184_FinalProject_Datasets/airports.csv" )

# Merge the datasets based on the airline / airport codes
ChristmasWeekFlights2020 <- cleandf_2020_data %>%
  # Merge with airline codes
  left_join(airlineCode, by = c("OP_UNIQUE_CARRIER" = "Code")) %>%
  rename(Airline_Name = Description) %>%
  # Select only relevant columns from airportCode and merge
  left_join(airportCode %>% select(IATA, Airport.name), by = c("ORIGIN" = "IATA")) %>%
  rename(Airport_Name = Airport.name)

#Save filtered data on local machine
output_path <- "~/Downloads/STAT184_FinalProject_Datasets/ChristmasWeekFlights2020.csv"
write_csv(ChristmasWeekFlights2020, output_path)

# EDA
head(ChristmasWeekFlights2020)
summary(ChristmasWeekFlights2020)

# We find that there were no cancellations (CANCELLED) or flight diversions (DIVERTED) for the week of Christmas in 2020


#######                                                    [EDA]                                                     #######

# Use esquisser to find explore and find meaningful trends in data
#esquisser(data = ChristmasWeekFlights2020, viewer = "browser")


# 1. Most popular airports in the week of Christmas for 2020 

# Create a dataframe that counts the number of flights and calculates average delays by ORIGIN (airport)
flights_by_airport <- ChristmasWeekFlights2020 %>%
  group_by(ORIGIN) %>%
  summarise(
    Number_of_Flights = sum(FLIGHTS, na.rm = TRUE),
    Avg_Weather_Delay = median(WEATHER_DELAY, na.rm = TRUE),
    Avg_Carrier_Delay = median(CARRIER_DELAY, na.rm = TRUE)
  ) %>%
  arrange(desc(Number_of_Flights))

# Filter for the top 10 busiest airports
top10_flights_by_airport <- flights_by_airport %>%
  slice_max(order_by = Number_of_Flights, n = 10) %>%
  left_join(ChristmasWeekFlights2020 %>% select(ORIGIN, Airport_Name), 
            by = "ORIGIN") %>%
  distinct()

View(top10_flights_by_airport) # View result

# 2. Most popular airlines in the week of Christmas for 2020 

# Create a dataframe that counts the number of flights and calculates average delays by Airline Name
flights_by_airline <- ChristmasWeekFlights2020 %>%
  group_by(OP_UNIQUE_CARRIER) %>%
  summarise(
    Number_of_Flights = sum(FLIGHTS, na.rm = TRUE),
    Avg_Weather_Delay = median(WEATHER_DELAY, na.rm = TRUE),
    Avg_Carrier_Delay = median(CARRIER_DELAY, na.rm = TRUE)
  ) %>%
  arrange(desc(Number_of_Flights))

# Filter for the top 10 airlines by number of flights
top10_flights_by_airline <- flights_by_airline %>%
  slice_max(order_by = Number_of_Flights, n = 10) %>%
  left_join(ChristmasWeekFlights2020 %>% select(OP_UNIQUE_CARRIER, Airline_Name), 
            by = "OP_UNIQUE_CARRIER") %>%
  distinct()

View(top10_flights_by_airline) # View result


#-----------------------------------------------------[DATA VISUALIZATIONS]-----------------------------------------------------#
# Additional graphs
'''
# Weather delay by top 10 airlines -- Produces no meaningful results when using median (instead of mean) as the statistic calculated
ggplot(top10_flights_by_airline, aes(x = reorder(Airline_Name, -Avg_Weather_Delay), y = Avg_Weather_Delay)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Weather Delay by Top 10 Airlines (Christmas Week 2020)",
    x = "Airlines",
    y = "Average Weather Delay (mins)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Carrier delay by top 10 airlines
ggplot(top10_flights_by_airline, aes(x = reorder(Airline_Name, -Avg_Carrier_Delay), y = Avg_Carrier_Delay)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Carrier Delay by Top 10 Airlines (Christmas Week 2020)",
    x = "Airlines",
    y = "Average Carrier Delay (mins)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Weather delay by top 10 airports -- Produces no meaningful results when using median (instead of mean) as the statistic calculated
ggplot(top10_flights_by_airport, aes(x = reorder(ORIGIN, -Avg_Weather_Delay), y = Avg_Weather_Delay)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Weather Delay by Top 10 Airports (Christmas Week 2020)",
    x = "Airports",
    y = "Average Weather Delay (mins)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Carrier delay by top 10 airports
ggplot(top10_flights_by_airport, aes(x = reorder(ORIGIN, -Avg_Carrier_Delay), y = Avg_Carrier_Delay)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Average Carrier Delay by Top 10 Airports (Christmas Week 2020)",
    x = "Airports",
    y = "Average Carrier Delay (mins)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
'''
#---------------------------------------------------------------------------------------------------------------#


# How do departure delays vary across the top 10 airlines during the Christmas week of 2020, and which airlines experience the most significant delays?"
top10_airlines <- top10_flights_by_airline$OP_UNIQUE_CARRIER

ChristmasWeekFlights2020 %>%
  filter(OP_UNIQUE_CARRIER %in% top10_airlines) %>%
  ggplot(aes(x = Airline_Name, y = DEP_DELAY)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.size = 2, outlier.colour = "red") +
  scale_y_log10() +  # Logarithmic scale for departure delay
  theme_minimal() +
  labs(
    title = "Departure Delay by Top 10 Airlines",
    x = "Airline",
    y = "Departure Delay (minutes, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), 
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14)
  )


# How do arrival delays vary across the top 10 airlines during the Christmas week of 2020, and which airlines experience the most significant delays?
top10_airlines <- top10_flights_by_airline$OP_UNIQUE_CARRIER

ChristmasWeekFlights2020 %>%
  filter(OP_UNIQUE_CARRIER %in% top10_airlines) %>%
  ggplot(aes(x = Airline_Name, y = ARR_DELAY)) +
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.size = 2, outlier.colour = "red") +
  scale_y_log10() +  # Logarithmic scale for arrival delay
  theme_minimal() +
  labs(
    title = "Arrival Delay by Top 10 Airlines",
    x = "Airline",
    y = "Arrival Delay (minutes, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14)
  )

# How do departure delays vary across the top 10 airports during the Christmas week of 2020, and which airports experience the most significant delays?
top10_airports <- top10_flights_by_airport$ORIGIN

ChristmasWeekFlights2020 %>%
  filter(ORIGIN %in% top10_airports) %>%
  ggplot(aes(x = ORIGIN, y = DEP_DELAY)) +  # Change ARR_DELAY to DEP_DELAY and x to ORIGIN
  geom_boxplot(fill = "skyblue", alpha = 0.7, outlier.size = 2, outlier.colour = "red") +
  scale_y_log10() +  # Logarithmic scale for departure delay
  theme_minimal() +
  labs(
    title = "Departure Delay by Top 10 Airports",
    x = "Airport",
    y = "Departure Delay (minutes, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14)
  )

# How do arrival delays vary across the top 10 airports during the Christmas week of 2020, and which airports experience the most significant delays?
# Remove rows with non-finite ARR_DELAY values (N/A, <= 0)
filtered_data_arr <- ChristmasWeekFlights2020 %>%
  filter(ORIGIN %in% top10_airports, !is.na(ARR_DELAY), ARR_DELAY > 0)

ggplot(filtered_data_arr, aes(x = ORIGIN, y = ARR_DELAY)) + 
  geom_boxplot(fill = "lightgreen", alpha = 0.7, outlier.size = 2, outlier.colour = "red") +
  scale_y_log10() +  # Logarithmic scale for arrival delay
  theme_minimal() +
  labs(
    title = "Arrival Delay by Top 10 Airports During Christmas Week 2020",
    x = "Airport",
    y = "Arrival Delay (minutes, log scale)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14)
  )


# How do weather-related delays vary across the top 10 airlines during Christmas week in 2020?
ChristmasWeekFlights2020 %>%
  filter(OP_UNIQUE_CARRIER %in% top10_airlines) %>%
  ggplot(aes(x = reorder(Airline_Name, WEATHER_DELAY), y = WEATHER_DELAY)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Weather Delay by Top 10 Airlines",
       x = "Airline",
       y = "Weather Delay (minutes)") +
  theme_minimal()

# Which airlines among the top 10 experienced the highest carrier delays during Christmas week in 2020, and what might contribute to these delays?
ChristmasWeekFlights2020 %>%
  filter(OP_UNIQUE_CARRIER %in% top10_airlines) %>%
  ggplot(aes(x = reorder(Airline_Name, CARRIER_DELAY), y = CARRIER_DELAY)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Carrier Delay by Top 10 Airlines",
       x = "Airline",
       y = "Carrier Delay (minutes)") +
  theme_minimal()

# How do weather-related delays at the top 10 airports compare during Christmas week in 2020, and which airports are most affected by adverse weather conditions?
ChristmasWeekFlights2020 %>%
  filter(ORIGIN %in% top10_airports) %>%
  ggplot(aes(x = reorder(ORIGIN, WEATHER_DELAY), y = WEATHER_DELAY)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Weather Delay by Top 10 Airports",
       x = "Airport (IATA Code)",
       y = "Weather Delay (minutes)") +
  theme_minimal() 

# How does the average departure delay change from the 20th to the 27th of December 2020, and are there any noticeable trends or patterns in the delays during this period?
ChristmasWeekFlights2020 %>%
  group_by(DAY_OF_MONTH) %>%
  summarise(Avg_Dep_Delay = median(DEP_DELAY, na.rm = TRUE)) %>%
  ggplot(aes(x = DAY_OF_MONTH, y = Avg_Dep_Delay)) +
  geom_line(color = "blue") +
  geom_point(size = 2, color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Average Departure Delay by Day (20th-27th)",
    x = "Day of the Month",
    y = "Average Departure Delay (minutes)"
  )

# What trends are observed in the average arrival delay by day of the month from December 20th to 27th, 2020, and how do these trends relate to the overall travel demand during the holiday season?
ChristmasWeekFlights2020 %>%
  group_by(DAY_OF_MONTH) %>%
  summarise(Avg_Arr_Delay = median(ARR_DELAY, na.rm = TRUE)) %>%
  ggplot(aes(x = DAY_OF_MONTH, y = Avg_Arr_Delay)) +
  geom_line(color = "blue") +
  geom_point(size = 2, color = "darkblue") +
  theme_minimal() +
  labs(
    title = "Average Arrival Delay by Day (20th-27th)",
    x = "Day of the Month",
    y = "Average Arrival Delay (minutes)"
  )


# How does the distribution of median delays (departure, arrival, carrier, and weather) vary across different days of the month (20th-27th) for the top 10 busiest airports during the Christmas week of 2020?"

top10_airports <- top10_flights_by_airport$ORIGIN

# Filter data for the top 10 airports and days 20th to 27th, calculate median delays for each delay type
ChristmasWeekFlights2020 %>%
  filter(ORIGIN %in% top10_airports, DAY_OF_MONTH >= 20 & DAY_OF_MONTH <= 27) %>%
  mutate(
    Delay_Type = case_when(
      CARRIER_DELAY > 0 ~ "Carrier Delay",
      WEATHER_DELAY > 0 ~ "Weather Delay",
      #ARR_DELAY > 0 ~ "Arrival Delay",
      #DEP_DELAY > 0 ~ "Departure Delay",
      TRUE ~ "No Delay"
    )
  ) %>%
  group_by(DAY_OF_MONTH, ORIGIN, Delay_Type) %>%
  summarise(Median_Delay = median(c(#DEP_DELAY, ARR_DELAY,
                                    CARRIER_DELAY, WEATHER_DELAY), na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = DAY_OF_MONTH, y = Median_Delay, fill = Delay_Type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ ORIGIN, ncol = 2) +
  theme_minimal() +
  labs(
    title = "Median Delay by Delay Type, Day of the Month, and Airport",
    x = "Day of the Month",
    y = "Median Delay (minutes)",
    fill = "Delay Type"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Heat map of delay correlation of the top 10 airports
# Do departure delays have a cascading effect? How do average departure delays correlate across the top 10 busiest airports during Christmas week in 2020, and are there patterns of delay propagation between airports in the same network or region?

# Install ggcorplot if not already installed
# install.packages("ggcorrplot")


# 1. Identify the top 10 busiest airports (already created in 'top10_flights_by_airport')
top10_airports <- top10_flights_by_airport$ORIGIN

# 2. Filter data for top 10 airports and select relevant columns
top10_data <- ChristmasWeekFlights2020 %>%
  filter(ORIGIN %in% top10_airports) %>%
  select(FL_DATE, ORIGIN, DEP_DELAY) %>%
  na.omit()

# 3. Summarize daily average delays per airport
daily_avg_delays <- top10_data %>%
  group_by(FL_DATE, ORIGIN) %>%
  summarise(Avg_DEP_Delay = median(DEP_DELAY, na.rm = TRUE)) %>%
  ungroup()

# 4. Reshape data into a wide format for correlation calculation
wide_data <- daily_avg_delays %>%
  pivot_wider(names_from = ORIGIN, values_from = Avg_DEP_Delay)

# 5. Calculate the correlation matrix for the top 10 airports
cor_matrix <- cor(wide_data %>% select(-FL_DATE), use = "pairwise.complete.obs")

# 6. Melt the correlation matrix for plotting
cor_data <- melt(cor_matrix)

# 7. Plot the heatmap
ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "white", high = "red", mid = "lightpink",
                       midpoint = 0.5, limit = c(0, 1), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        axis.title = element_text(size = 12)) +
  labs(title = "Heatmap of Delay Correlation Between Top 10 Airports",
       x = "Origin Airport", y = "Destiation Airport")
