# Author Dr. Isaac Kipkemoi
# ikipkemoi00@gmail.com
# Load libraries
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(scales)

# Pivot to long format
gdp_long <- pivot_longer(gdp_data, cols = -Year, names_to = "City", values_to = "GDP")

# Partnership annotations
annotations <- data.frame(
  Year = c(1982, 2000, 2007, 2007),
  Label = c("Bad Vilbel", "Minneapolis", "Indianapolis", "Ithaca"),
  x_text = c(1983.5, 2001.5, 2008.5, 2008.5),
  y_text = c(12000, 55000, 145000, 200000),
  y_arrow = c(1000, 3000, 40000, 70000)
)

# Eldoret and Bad Vilbel GDP annotation points for 1984 and 2023
label_points <- data.frame(
  Year = c(1984, 1984, 2023, 2023),
  City = c("Eldoret", "Bad Vilbel", "Eldoret", "Bad Vilbel"),
  GDP = c(850, 1050, 3600, 4000),  # Estimated 1984 values interpolated
  x_label = c(1985, 1983, 2022, 2024),  # Offset x for label positioning
  y_label = c(1500, 3000, 6000, 8000),  # Offset y for label positioning
  label = c("Eldoret (GDP = $850)", "Bad Vilbel (GDP = $1,050)",
            "Eldoret (GDP = $3,600)", "Bad Vilbel (GDP = $4,000)")
)

# Plot
ggplot(gdp_long, aes(x = Year, y = GDP, color = City)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  
  # Vertical lines for partnerships
  geom_vline(data = annotations, aes(xintercept = Year), linetype = "dashed", color = "gray40") +
  
  # Partnership annotations
  geom_text(data = annotations,
            aes(x = x_text, y = y_text, label = paste(Label, "")),
            hjust = 0, size = 4, family = "Times", inherit.aes = FALSE) +
  geom_segment(data = annotations,
               aes(x = x_text, xend = Year, y = y_text, yend = y_arrow),
               arrow = arrow(length = unit(0.18, "cm")),
               inherit.aes = FALSE,
               color = "black") +
  
  # GDP label text for Eldoret and Bad Vilbel
  geom_text(data = label_points,
            aes(x = x_label, y = y_label, label = label),
            color = "black", size = 4, family = "Times", inherit.aes = TRUE) +

  # Dotted arrows to GDP points
  geom_segment(data = label_points,
               aes(x = x_label, xend = Year, y = y_label, yend = GDP),
               linetype = "dotted", color = "black",
               arrow = arrow(length = unit(2, "cm")),
               inherit.aes = FALSE) +

  # Y-axis in '000
  scale_y_continuous(labels = function(x) paste0(x / 1000, "k")) +
  
  # Titles and theme
  labs(
    title = "",
    subtitle = "Partnership years (dashed), key GDP comparisons (dotted arrows)",
    x = "Year",
    y = "GDP (in '000 USD)",
    color = "City"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

# //////////
# Focused data
gdp_pair <- gdp_data[, c("Year", "Eldoret", "Bad_Vilbel")]
gdp_long_pair <- pivot_longer(gdp_pair, cols = -Year, names_to = "City", values_to = "GDP")

label_points <- data.frame(
  Year = c(1984, 1984, 2023, 2023),
  City = c("Eldoret", "Bad_Vilbel", "Eldoret", "Bad_Vilbel"),
  GDP = c(850, 1050, 3600, 4000),
  x_label = c(1982, 1980, 2017.5, 2017),  # pull left to avoid clipping
  y_label = c(2500, 4000, 7000, 9500),
  label = c("Eldoret (GDP = $850)", "Bad Vilbel (GDP = $1,050)",
            "Eldoret (GDP = $3,600)", "Bad Vilbel (GDP = $4,000)")
)

ggplot(gdp_long_pair, aes(x = Year, y = GDP, color = City)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Eldoret" = "blue", "Bad_Vilbel" = "darkgreen")) +
  geom_text(data = label_points,
            aes(x = x_label, y = y_label, label = label),
            color = "black", size = 4, hjust = 0, family = "serif", inherit.aes = FALSE) +
  geom_segment(data = label_points,
               aes(x = x_label, xend = Year, y = y_label, yend = GDP),
               linetype = "dotted", color = "black",
               arrow = arrow(length = unit(0.25, "cm")),
               inherit.aes = FALSE) +
  scale_y_continuous(labels = function(x) paste0(x / 1000, "k")) +
  labs(
    title = "GDP Highlights: Eldoret and Bad Vilbel (1984 & 2023)",
    x = "Year",
    y = "GDP (in '000 USD)",
    color = "City"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave("all_cities_gdp.png", width = 10, height = 6, dpi = 300)
ggsave("eldoret_badvilbel_gdp.png", width = 10, height = 6, dpi = 300)

# ///////////////////



# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create the dataset
df <- data.frame(
  Year = c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017, 2022, 2023),
  Eldoret = c(NA, NA, NA, NA, 900, 1300, 1900, 2500, 3400, 3600),
  Bad_Vilbel = c(1000, 1200, 1350, 1450, 1600, 2100, 2600, 3200, 3900, 4000),
  Minneapolis = c(120000, 135000, 150000, 165000, 180000, 220000, 265000, 290000, 335000, 350710),
  Indianapolis = c(47000, 53000, 60000, 67000, 70000, 95000, 120000, 160000, 190000, 199198),
  Ithaca = c(2500, 2800, 3100, 3300, 3500, 4600, 5700, 6200, 7200, 7389)
)

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = -Year, names_to = "City", values_to = "GDP_USD")

# Plot
ggplot(df_long, aes(x = Year, y = GDP_USD)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  facet_wrap(~ City, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "",
    x = "Year",
    y = "GDP in millions (USD)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  )

# NEW GRAPH 

# Load libraries
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(scales)

# GDP data
gdp_data <- data.frame(
  Year = c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017, 2022, 2023),
  Eldoret = c(NA, NA, NA, NA, 900, 1300, 1900, 2500, 3400, 3600),
  Bad_Vilbel = c(1000, 1200, 1350, 1450, 1600, 2100, 2600, 3200, 3900, 4000),
  Minneapolis = c(120000, 135000, 150000, 165000, 180000, 220000, 265000, 290000, 335000, 350710),
  Indianapolis = c(47000, 53000, 60000, 67000, 70000, 95000, 120000, 160000, 190000, 199198),
  Ithaca = c(2500, 2800, 3100, 3300, 3500, 4600, 5700, 6200, 7200, 7389)
)

# Reshape data
gdp_long_all <- pivot_longer(gdp_data, cols = -Year, names_to = "City", values_to = "GDP")

# Partnership events
partnerships <- data.frame(
  Year = c(1982, 1995, 2000, 2006),
  Label = c("Bad Vilbel", "Indianapolis", "Minneapolis", "Ithaca")
)

# Add optional annotation text positions
partnerships$x_text <- partnerships$Year + 0.5
partnerships$y_text <- c(12000, 40000, 55000, 70000)
partnerships$y_arrow <- c(1000, 3000, 3000, 5000)

# Plot
ggplot(gdp_long_all, aes(x = Year, y = GDP, color = City)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Dark2") +
  
  # Vertical dotted lines for partnership years
  geom_vline(data = partnerships, aes(xintercept = Year), linetype = "dotted", color = "gray40") +
  
  # Labels + arrows
  geom_text(data = partnerships,
            aes(x = x_text, y = y_text, label = paste(Label, "Partnership")),
            hjust = 0, size = 4, family = "serif", inherit.aes = FALSE) +
  geom_segment(data = partnerships,
               aes(x = x_text, xend = Year, y = y_text, yend = y_arrow),
               arrow = arrow(length = unit(0.18, "cm")),
               inherit.aes = FALSE, color = "black") +
  
  scale_y_continuous(labels = function(x) paste0(x / 1000, "k")) +
  labs(
    title = "All Sister Cities: GDP Trends with Partnership Years",
    x = "Year",
    y = "GDP (in '000 USD)",
    color = "City"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")


# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Create the dataset
df <- data.frame(
  Year = c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017, 2022, 2023),
  Eldoret = c(NA, NA, NA, NA, 900, 1300, 1900, 2500, 3400, 3600),
  Bad_Vilbel = c(1000, 1200, 1350, 1450, 1600, 2100, 2600, 3200, 3900, 4000),
  Minneapolis = c(120000, 135000, 150000, 165000, 180000, 220000, 265000, 290000, 335000, 350710),
  Indianapolis = c(47000, 53000, 60000, 67000, 70000, 95000, 120000, 160000, 190000, 199198),
  Ithaca = c(2500, 2800, 3100, 3300, 3500, 4600, 5700, 6200, 7200, 7389)
)

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = -Year, names_to = "City", values_to = "GDP_USD")

# Partnership years per city
partnership_years <- data.frame(
  City = c("Bad_Vilbel", "Indianapolis", "Minneapolis", "Ithaca"),
  Partnership_Year = c(1982, 1995, 2000, 2006)
)

# Merge with df_long to get vertical lines in correct panels
df_long <- df_long %>%
  left_join(partnership_years, by = "City")

# Plot with vertical lines per city
ggplot(df_long, aes(x = Year, y = GDP_USD)) +
  geom_line(color = "orange", size = 1) +
  geom_point(color = "orange", size = 2) +
  geom_vline(aes(xintercept = Partnership_Year), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ City, scales = "free_y") +
  theme_minimal(base_size = 13) +
  labs(
    title = "",
    x = "Year",
    y = "GDP in millions (USD)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    strip.text = element_text(face = "bold")
  )





# CODE FOR STATISTICAL ANALYSIS \\\\\\\
# Load libraries
library(dplyr)

# Eldoret GDP data
eldoret_data <- data.frame(
  Year = c(1982, 1987, 1992, 1997, 2002, 2007, 2012, 2017, 2022, 2023),
  GDP_USD = c(NA, NA, NA, NA, 900, 1300, 1900, 2500, 3400, 3600)
)

# Partnership years
partnerships <- data.frame(
  City = c("Bad Vilbel", "Indianapolis", "Minneapolis", "Ithaca"),
  Year = c(1982, 1995, 2000, 2006)
)

# Function to run linear regression with post-partnership indicator
run_linear_model <- function(data, cutoff_year, city_name) {
  df <- data %>%
    mutate(Post_Partnership = ifelse(Year >= cutoff_year, 1, 0)) %>%
    filter(!is.na(GDP_USD))
  
  if (length(unique(df$Post_Partnership)) < 2) {
    cat("\n===== Partnership with", city_name, "(", cutoff_year, ") =====\n")
    cat("❌ Not enough data in both groups for regression.\n")
    return(NULL)
  }
  
  model <- lm(GDP_USD ~ Post_Partnership, data = df)
  
  cat("\n===== Partnership with", city_name, "(", cutoff_year, ") =====\n")
  summary_output <- summary(model)
  print(summary_output)
}

# Apply the regression approach for all partnerships
for (i in 1:nrow(partnerships)) {
  run_linear_model(eldoret_data, partnerships$Year[i], partnerships$City[i])
}
