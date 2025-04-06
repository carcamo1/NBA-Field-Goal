getwd()
setwd("/Users/alexander_carcamo/Documents/Data670")
# Install the required packages if not already installed
install.packages("tidyverse")  # This includes ggplot2, dplyr, readr, and more
install.packages("ggplot2")    # Individual installation for ggplot2
install.packages("dplyr")      # Individual installation for dplyr
install.packages("kableExtra")
# Load the libraries
library(tidyverse)   # Loading the complete tidyverse package
library(ggplot2)     # For visualization
library(dplyr)       # For data manipulation
library(readr)
library(gridExtra)
library(knitr)
library(kableExtra)

# Load the 2004-2006 data
shots_2004 <- read_csv("NBA_2004_Shots.csv")
shots_2005 <- read_csv("NBA_2005_Shots.csv")
shots_2006 <- read_csv("NBA_2006_Shots.csv")

# Combine 2004, 2005, and 2006 data into one dataset
shots_2004_2006 <- bind_rows(shots_2004, shots_2005, shots_2006)

table(shots_2004_2006$BASIC_ZONE)

# Load 2021-2023 datasets 
shots_2021 <- read_csv("NBA_2021_Shots.csv")
shots_2022 <- read_csv("NBA_2022_Shots.csv")
shots_2023 <- read_csv("NBA_2023_Shots.csv")

# Combine 2021, 2022, and 2023 data into another dataset
shots_2021_2023 <- bind_rows(shots_2021, shots_2022, shots_2023)
print(summary(shots_2004_2006))
print(summary(shots_2021_2023))

# Define Court Plotting Function
circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data.frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}

# Define Court Dimensions and Features
width = 50
height = 94 / 2
key_height = 19
inner_key_width = 12
outer_key_width = 16
backboard_width = 6
backboard_offset = 4
neck_length = 0.5
hoop_radius = 0.75
hoop_center_y = backboard_offset + neck_length + hoop_radius
three_point_radius = 23.75
three_point_side_radius = 22
three_point_side_height = 14

# Create the Court Points Data
court_points = data.frame(
  x = c(width / 2, width / 2, -width / 2, -width / 2, width / 2),
  y = c(height, 0, 0, height, height),
  desc = "perimeter"
)

court_points = rbind(court_points, data.frame(
  x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
  y = c(0, key_height, key_height, 0),
  desc = "outer_key"
))

court_points = rbind(court_points, data.frame(
  x = c(-backboard_width / 2, backboard_width / 2),
  y = c(backboard_offset, backboard_offset),
  desc = "backboard"
))

court_points = rbind(court_points, data.frame(
  x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
))

foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
foul_circle_top = subset(foul_circle, y > key_height)
foul_circle_top$desc = "foul_circle_top"

foul_circle_bottom = subset(foul_circle, y < key_height)
foul_circle_bottom$desc = "foul_circle_bottom"

hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius)
hoop$desc = "hoop"

restricted = circle_points(center = c(0, hoop_center_y), radius = 4)
restricted = subset(restricted, y >= hoop_center_y)
restricted$desc = "restricted"

three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius)
three_point_circle = subset(three_point_circle, y >= three_point_side_height & y >= hoop_center_y)

three_point_line = data.frame(
  x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
  y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
  desc = "three_point_line"
)

court_points = rbind(court_points, foul_circle_top, foul_circle_bottom, hoop, restricted, three_point_line)

# Function to Plot Court with Efficiency Data
plot_court_with_data <- function(data, title) {
  
  # Separate 2-point and 3-point shots
  two_point_summary <- data %>%
    filter(BASIC_ZONE != "Above the Break 3" & BASIC_ZONE != "Left Corner 3" & BASIC_ZONE != "Right Corner 3") %>%
    group_by(ZONE_NAME) %>%
    summarise(
      Made = sum(SHOT_MADE == "TRUE"),
      Attempted = n(),
      Efficiency = round(Made / Attempted, 2),
      LOC_X = mean(LOC_X),
      LOC_Y = mean(LOC_Y)
    )
  
  three_point_summary <- data %>%
    filter(BASIC_ZONE == "Above the Break 3" | BASIC_ZONE == "Left Corner 3" | BASIC_ZONE == "Right Corner 3") %>%
    group_by(BASIC_ZONE) %>%
    summarise(
      Made = sum(SHOT_MADE == "TRUE"),
      Attempted = n(),
      Efficiency = round(Made / Attempted, 2),
      LOC_X = mean(LOC_X),
      LOC_Y = mean(LOC_Y)
    )
  
  # Combine Both Summaries
  combined_summary <- bind_rows(
    two_point_summary %>% rename(Zone = ZONE_NAME),
    three_point_summary %>% rename(Zone = BASIC_ZONE)
  )
  
  # Plot Court with Shot Efficiency Overlay
  ggplot() +
    # Draw Court Lines
    geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1.2) +
    # Add Efficiency Zones
    geom_tile(data = combined_summary, aes(x = LOC_X, y = LOC_Y, fill = Efficiency), width = 5, height = 5, alpha = 0.7) +
    # Add Efficiency Labels
    geom_text(data = combined_summary, aes(x = LOC_X, y = LOC_Y, label = paste0(Made, "/", Attempted, "\n", Efficiency * 100, "%")),
              color = "black", size = 4) +
    # Customize Colors
    scale_fill_gradient(low = "red", high = "green", na.value = "gray", name = "Efficiency") +
    # Title and Theme
    ggtitle(title) +
    coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "floralwhite"),
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    )
}

# Plot the 2004-2006 Data
plot_2004_2006 <- plot_court_with_data(shots_2004_2006, "2004-2006 Shot Efficiency by Zone")
# Print Visualization
print(plot_2004_2006)

# Efficiency Data for All Zones (including Backcourt)
zone_efficiency_data <- data.frame(
  Zone = c("Backcourt", "Above the Break 3", "Left Corner 3", "Right Corner 3", 
           "In the Paint (Non-RA)", "Restricted Area", "Left Side", "Right Side", "Mid-Range"),
  Shots_Made = c(30, 28032, 5880, 6193, 23663, 145488, 12183, 11633, 26764),
  Shots_Attempted = c(1404, 80825, 15215, 16278, 61855, 278011, 30631, 28784, 69074),
  Efficiency = c(2, 35, 39, 38, 38, 52, 40, 40, 39)
)

# Create Visual Table using kable with Conditional Formatting
zone_efficiency_data %>%
  kable("html", caption = "2004-2006 Shot Efficiency by Zone (Including Backcourt)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE, color = "black", background = "#f7f7f7") %>%
  column_spec(2, color = "black", background = "#f7f7f7") %>%
  column_spec(3:4, width = "8em") %>%
  column_spec(4, color = "white", bold = TRUE,
              background = spec_color(zone_efficiency_data$Efficiency, end = 0.7, 
                                      option = "D", direction = 1, na_color = "gray")) %>%
  add_header_above(c(" " = 1, "Shot Efficiency Table" = 3)) %>%
  add_header_above(c(" " = 1, "2004-2006 Shot Efficiency" = 3))

# Step 1: Calculate the efficiency for all zones in the 2021-2023 dataset
zone_efficiency_2021_2023 <- shots_2021_2023 %>%
  group_by(BASIC_ZONE, ZONE_NAME) %>%
  summarise(
    Shots_Made = sum(SHOT_MADE == TRUE),
    Shots_Attempted = n(),
    Efficiency = round(Shots_Made / Shots_Attempted * 100, 2)
  ) %>%
  filter(BASIC_ZONE %in% c("Above the Break 3", "Left Corner 3", "Right Corner 3", 
                           "In The Paint (Non-RA)", "Restricted Area", "Mid-Range", "Backcourt"))

# Step 2: Define the coordinates for each zone (manually added coordinates based on the previous approach)
zone_coordinates <- tibble(
  BASIC_ZONE = c("Above the Break 3", "Left Corner 3", "Right Corner 3", 
                 "In The Paint (Non-RA)", "Restricted Area", "Mid-Range", "Mid-Range", "Mid-Range", "Backcourt"),
  ZONE_NAME = c("Center", "Left Side", "Right Side", 
                "Center", "Center", "Center", "Left Side", "Right Side", "Back Court"),
  LOC_X = c(0, 23.2, -23.2, 0, 0, 0, 14.7, -14.5, -0.526),
  LOC_Y = c(30.4, 6.26, 6.45, 10.8, 5.47, 23.4, 8.71, 8.80, 35)
)

# Step 3: Join the efficiency data with the coordinates
zone_summary_2021_2023 <- zone_coordinates %>%
  left_join(zone_efficiency_2021_2023, by = c("BASIC_ZONE", "ZONE_NAME"))

# Step 4: Create the plot for the 2021-2023 data
plot_2021_2023 <- ggplot() +
  geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1.2) +
  geom_tile(data = zone_summary_2021_2023, aes(x = LOC_X, y = LOC_Y, fill = Efficiency), width = 5, height = 5, alpha = 0.7) +
  geom_text(data = zone_summary_2021_2023, aes(x = LOC_X, y = LOC_Y, label = paste0(Shots_Made, "/", Shots_Attempted, "\n", round(Efficiency, 1), "%")), 
            color = "black", size = 4) +
  scale_fill_gradient(low = "red", high = "green", na.value = "gray", name = "Efficiency") +
  ggtitle("2021-2023 Shot Efficiency by Zone") +
  coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "floralwhite"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Print the plot
print(plot_2021_2023)

# Step 5: Create the efficiency table for 2021-2023 data
efficiency_table_2021_2023 <- zone_summary_2021_2023 %>%
  select(BASIC_ZONE, ZONE_NAME, Shots_Made, Shots_Attempted, Efficiency) %>%
  arrange(desc(Efficiency))

# Display the efficiency table for the 2021-2023 data
efficiency_table_2021_2023 %>%
  select(BASIC_ZONE, Shots_Made, Shots_Attempted, Efficiency) %>%
  rename(Zone = BASIC_ZONE) %>%
  kable("html", caption = "2021-2023 Shot Efficiency Table") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE, color = "black", background = "#f7f7f7") %>%
  column_spec(2, color = "black", background = "#f7f7f7") %>%
  column_spec(3:4, width = "8em") %>%
  column_spec(4, color = "white", bold = TRUE, 
              background = spec_color(efficiency_table_2021_2023$Efficiency, end = 0.7, 
                                      option = "D", direction = 1, na_color = "gray")) %>%
  # Adjust the header to properly match the number of columns
  add_header_above(c(" " = 1, "Shots" = 2, "Efficiency" = 1)) %>%
  add_header_above(c("2021-2023 Shot Efficiency Table" = 4))

# Filter for only Atlanta Hawks players
hawks_shots_2021_2023 <- shots_2021_2023 %>%
  filter(TEAM_NAME == "Atlanta Hawks")  # or filter by TEAM_ID if known
# Calculate efficiency for the Atlanta Hawks by zone
hawks_zone_efficiency <- hawks_shots_2021_2023 %>%
  group_by(BASIC_ZONE, ZONE_NAME) %>%
  summarise(
    Shots_Made = sum(SHOT_MADE == TRUE),
    Shots_Attempted = n(),
    Efficiency = round(Shots_Made / Shots_Attempted, 2)
  ) %>%
  filter(
    (BASIC_ZONE == "Above the Break 3" & ZONE_NAME == "Center") |
      (BASIC_ZONE == "Left Corner 3" & ZONE_NAME == "Left Side") |
      (BASIC_ZONE == "Right Corner 3" & ZONE_NAME == "Right Side") |
      (BASIC_ZONE == "In The Paint (Non-RA)" & ZONE_NAME == "Center") |
      (BASIC_ZONE == "Restricted Area" & ZONE_NAME == "Center") |
      (BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Center") |
      (BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Left Side") |
      (BASIC_ZONE == "Mid-Range" & ZONE_NAME == "Right Side") |
      (BASIC_ZONE == "Backcourt" & ZONE_NAME == "Back Court")
  )
# Merge Hawks data with the zone coordinates
hawks_zone_summary <- zone_coordinates %>%
  left_join(hawks_zone_efficiency, by = c("BASIC_ZONE", "ZONE_NAME"))
# Create the plot for Atlanta Hawks shot efficiency
plot_hawks_efficiency <- ggplot() +
  geom_path(data = court_points, aes(x = x, y = y, group = desc), color = "black", size = 1.2) +
  geom_tile(data = hawks_zone_summary, aes(x = LOC_X, y = LOC_Y, fill = Efficiency), width = 5, height = 5, alpha = 0.7) +
  geom_text(data = hawks_zone_summary, aes(x = LOC_X, y = LOC_Y, label = paste0(Shots_Made, "/", Shots_Attempted, "\n", round(Efficiency * 100, 1), "%")), 
            color = "black", size = 4) +
  scale_fill_gradient(low = "red", high = "green", na.value = "gray", name = "Efficiency") +
  ggtitle("2021-2023 Atlanta Hawks Shot Efficiency by Zone") +
  coord_fixed(ylim = c(0, 45), xlim = c(-25, 25)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "floralwhite"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

# Print the plot
print(plot_hawks_efficiency)
# Create the efficiency table for Atlanta Hawks
hawks_efficiency_table <- hawks_zone_summary %>%
  select(BASIC_ZONE, Shots_Made, Shots_Attempted, Efficiency) %>%
  rename(Zone = BASIC_ZONE)

# Display the table
hawks_efficiency_table %>%
  kable("html", caption = "2021-2023 Atlanta Hawks Shot Efficiency Table") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  column_spec(1, bold = TRUE, color = "black", background = "#f7f7f7") %>%
  column_spec(2, color = "black", background = "#f7f7f7") %>%
  column_spec(3:4, width = "8em") %>%
  column_spec(4, color = "white", bold = TRUE, 
              background = spec_color(hawks_efficiency_table$Efficiency, end = 0.7, 
                                      option = "D", direction = 1, na_color = "gray")) %>%
  add_header_above(c(" " = 1, "Shots Made" = 1, "Shots Attempted" = 1, "Efficiency (%)" = 1)) %>%
  add_header_above(c("2021-2023 Atlanta Hawks Shot Efficiency Table" = 4))


