############################################################
## MSc Analysis Script (Refined)
## Forest Loss & Conflict Data Integration
## Author: Isaac Kipkemoi
############################################################

# -------------------------------
# 1. Load required libraries
# -------------------------------
library(readxl)
library(dplyr)
library(lubridate)
library(zoo)

# -------------------------------
# 2. Define data path
# -------------------------------
data_path <- "F:/Notes/Projects/My Msc paper"

# -------------------------------
# 3. Load Forest Loss Data
# -------------------------------
forest_loss <- read_excel(
  path  = file.path(data_path, "Forest_Loss.xlsx"),
  sheet = 1
) %>%
  rename(Year = 1) %>%
  mutate(Year = as.numeric(Year))

# -------------------------------
# 4. Load Conflict Event Data
# -------------------------------
conflict_raw <- read_excel(
  path = file.path(data_path, "Conflict_eventcounts.xlsx")
)

# -------------------------------
# 5. Create Monthly Date Variable
# -------------------------------
# ACLED columns:
# YEAR   = numeric (e.g. 2020)
# MONTH  = character month name (e.g. "September")
# EVENTS = conflict counts

conflict_monthly <- conflict_raw %>%
  rename(
    Year           = YEAR,
    Month_name     = MONTH,
    Conflict_Count = EVENTS
  ) %>%
  mutate(
    # Standardise month names (protects against case issues)
    Month_name = stringr::str_to_title(Month_name),
    
    # Create a proper Date using month names
    Date = as.Date(
      paste(Year, Month_name, "01"),
      format = "%Y %B %d"
    )
  ) %>%
  arrange(Date)



# -------------------------------
# 6. Construct ±6-Month Conflict Index
# -------------------------------
# Window = 13 months (6 before + current + 6 after)

conflict_monthly <- conflict_monthly %>%
  mutate(
    conflict_index_13m = rollapply(
      data   = Conflict_Count,
      width  = 13,
      FUN    = sum,
      align  = "center",
      fill   = NA,
      na.rm  = TRUE
    )
  )

# -------------------------------
# 7. Aggregate to Annual Conflict Index
# -------------------------------
conflict_annual <- conflict_monthly %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarise(
    Conflict_Index = mean(conflict_index_13m, na.rm = TRUE)
  ) %>%
  ungroup()

# -------------------------------
# 8. Restrict to Analysis Period (2001–2024)
# -------------------------------
conflict_annual_v1 <- conflict_annual %>%
  filter(Year >= 2001, Year <= 2024)

# -------------------------------
# 9. Merge with Forest Loss Data
# -------------------------------
analysis_data <- forest_loss %>%
  left_join(conflict_annual_v1, by = "Year")

# -------------------------------
# 10. Final checks
# -------------------------------
str(conflict_annual_v1)
str(analysis_data)
summary(analysis_data)

############################################################
## MSc Thesis Analysis Script
## Forest Loss, Conflict, and Elections
## Author: Isaac Kipkemoi
## Period: 2001–2024
############################################################

# ==========================================================
# 0. Load required packages
# ==========================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
library(spdep)
library(spatialreg)
library(lme4)
library(viridis)

#install.packages("spdep")
#install.packages("spatialreg")
# ==========================================================
# 0.1 Assumptions
# ==========================================================
# - analysis_data exists in the environment
# - Columns:
#   * Year
#   * Conflict_Index
#   * Forest blocks as separate columns
# - Forest loss values are per 1000 hectares
# - Spatial polygons exist as forest_blocks_sf
# - Governance and tree cover lookups exist

# ==========================================================
# 1. Reshape data to long (panel) format
# ==========================================================
forest_long <- analysis_data %>%
  pivot_longer(
    cols = -c(Year, Conflict_Index),
    names_to = "Forest_Block",
    values_to = "Forest_Loss"
  )

# ==========================================================
# QUESTION 1: Election years vs non-election years
# ==========================================================

# ==========================================================
# 1.1 Define election and referendum years
# ==========================================================
election_years   <- c(2002, 2007, 2013, 2017, 2022)
referendum_years <- c(2005, 2010)

forest_long <- forest_long %>%
  mutate(
    Election_Year = ifelse(Year %in% election_years, 1, 0)
  )

# ==========================================================
# 1.2 Visualization: Time-series with election and referendum years
# ==========================================================
ggplot(forest_long, aes(x = Year, y = Forest_Loss, group = Forest_Block)) +
  geom_line(alpha = 0.3) +
  # Election years (red dashed)
  geom_vline(xintercept = election_years, linetype = "dashed", color = "red") +
  # Referendum years (orange dashed)
  geom_vline(xintercept = referendum_years, linetype = "dashed", color = "orange") +
  labs(
    title = "Annual Forest Loss with Election and Referendum Years Highlighted",
    y = "Forest Loss (per 1000 ha)",
    x = "Year"
  ) +
  theme_minimal()
##########################################################################################Springer Visulization 
library(ggplot2)
library(dplyr)
library(scales)

# ==========================================================
# 1. PREP: Create Data for Shading and Trends
# ==========================================================

# Create a dataframe for the shaded regions (Election Year +/- 0.5 years)
election_shading <- data.frame(
  xmin = election_years - 0.5,
  xmax = election_years + 0.5,
  ymin = -Inf, # Extend from bottom of plot
  ymax = Inf   # To top of plot
)

# Calculate Mean Trend
forest_summary <- forest_long %>%
  group_by(Year) %>%
  summarise(Mean_Loss = mean(Forest_Loss, na.rm = TRUE))

# ==========================================================
# 2. VISUALIZATION
# ==========================================================

election_years <- c(2002, 2007, 2013, 2017, 2022)
referendum_years <- c(2005, 2010)

forest_long <- forest_long %>%
  mutate(
    Election_Year = ifelse(Year %in% election_years, 1, 0)
  )

# Define distinct colors
event_colors <- c("Election Year" = "blue", "Referendum Year" = "#D55E00")
event_types  <- c("Election Year" = "dashed", "Referendum Year" = "dotted")

p <- ggplot() +
  
  # A. SHADING (Layer 1: Bottom Layer)
  # Faint grey box for 6 months before/after election
  geom_rect(data = election_shading,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "grey85", # Slightly darker than white background
            alpha = 0.5,     # Transparency
            inherit.aes = FALSE) + # Don't look for Forest_Loss data here
  
  # B. Background Raw Data (Layer 2)
  # Darker grey (grey40) and higher alpha (0.6) for visibility
  geom_line(data = forest_long, 
            aes(x = Year, y = Forest_Loss, group = Forest_Block), 
            color = "grey40", 
            alpha = 0.6, 
            size = 0.4) +
  
  # C. Event Lines (Layer 3)
  geom_vline(aes(xintercept = election_years, 
                 color = "Election Year", 
                 linetype = "Election Year"), 
             size = 0.8) +
  
  geom_vline(aes(xintercept = referendum_years, 
                 color = "Referendum Year", 
                 linetype = "Referendum Year"), 
             size = 0.8) +
  
  # D. Mean Trend Line (Layer 4: Top Layer)
  geom_line(data = forest_summary, 
            aes(x = Year, y = Mean_Loss), 
            color = "black", 
            size = 1.2) +
  
  # E. Scales and Axis Formatting
  scale_y_continuous(
    labels = scales::comma,
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 10)
  ) +
  
  # F. Legend Definition
  scale_color_manual(name = NULL, values = event_colors) +
  scale_linetype_manual(name = NULL, values = event_types) +
  
  # G. Theme and Labels
  labs(
    y = "Forest Loss (per 1,000 ha)",
    x = "Year"
  ) +
  theme_bw(base_size = 12, base_family = "sans") + 
  theme(
    # Axis
    axis.text = element_text(color = "black", size = 10),
    axis.title = element_text(face = "bold", size = 11),
    
    # Legend
    legend.position = "bottom",
    legend.key.width = unit(1.5, "cm"),
    
    # Grid (Make grid lines visible on top of the grey shading)
    panel.grid.major = element_line(size = 0.2, color = "grey90"),
    panel.grid.minor = element_blank(),
    
    # Bring the plot panel to the front if necessary, 
    # but usually geom_rect order handles this.
    panel.background = element_rect(fill = "transparent") 
  )

print(p)

#ggsave("Fig1_ForestLoss_Shaded.tiff", plot = p, width = 174, height = 100, units = "mm", dpi = 300)
#####################################################################################################################

# ----------------------------------------------------------
# 1.3 Visualization: Boxplot (Election vs Non-election)
# ----------------------------------------------------------
ggplot(forest_long,
       aes(x = factor(Election_Year),
           y = Forest_Loss,
           fill = factor(Election_Year))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("Non-election", "Election")) +
  labs(
    x = "",
    y = "Forest Loss (per 1000 ha)",
    title = "Forest Loss in Election vs Non-election Years"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# ==========================================================
# Enhanced Boxplot: Election vs Referendum vs Other years
# ==========================================================
library(ggplot2)
library(ggbeeswarm)
library(dplyr)

# Define year categories
election_years   <- c(2002, 2007, 2013, 2017, 2022)
referendum_years <- c(2005, 2010)

# Create a new Year_Category variable
forest_long <- forest_long %>%
  mutate(Year_Category = case_when(
    Year %in% election_years   ~ "Election",
    Year %in% referendum_years ~ "Referendum",
    TRUE                        ~ "Non-election/Non-referendum"
  ))

# Set factor levels for plotting order
forest_long$Year_Category <- factor(forest_long$Year_Category,
                                    levels = c("Non-election/Non-referendum",
                                               "Election",
                                               "Referendum"))

# Define colors for the three groups
cols <- c("Non-election/Non-referendum" = "#F76D5E",
          "Election"                    = "#FFFFBF",
          "Referendum"                  = "#FFA500")  # orange

# Plot
ggplot(forest_long, aes(x = Year_Category, y = Forest_Loss, fill = Year_Category)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +               # boxplot without default outliers
  geom_quasirandom(width = 0.2, size = 1.5, alpha = 0.5, color = "black") + # jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "blue") + # mean point
  scale_fill_manual(values = cols) +
  labs(
    title = "Forest Loss by Election, Referendum, and Other Years",
    x = "",
    y = "Forest Loss (per 1000 ha)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")



library(ggplot2)
library(ggbeeswarm)
library(dplyr)
# library(ggpubr)
# library(rstatix)

# Define election and referendum years
election_years   <- c(2002, 2007, 2013, 2017, 2022)
referendum_years <- c(2005, 2010)

# Create a new Year_Category variable
forest_long <- forest_long %>%
  mutate(Year_Category = case_when(
    Year %in% election_years   ~ "Election",
    Year %in% referendum_years ~ "Referendum",
    TRUE                        ~ "Non-election/Non-referendum"
  ))

# Set factor levels for plotting order
forest_long$Year_Category <- factor(forest_long$Year_Category,
                                    levels = c("Non-election/Non-referendum",
                                               "Election",
                                               "Referendum"))
####################################################################################### springer box plots
library(ggplot2)
library(ggbeeswarm)
library(dplyr)
library(scales)

# ==========================================================
# 1. PREP: Define Categories (Binary)
# ==========================================================
election_years <- c(2002, 2007, 2013, 2017, 2022)

# Create a binary variable: Election vs Non-Election
forest_long <- forest_long %>%
  mutate(
    Year_Category = ifelse(Year %in% election_years, "Election Year", "Non-Election Year")
  )

# Set factor levels: Put "Non-Election" first (left) as the baseline
forest_long$Year_Category <- factor(forest_long$Year_Category,
                                    levels = c("Non-Election Year", "Election Year"))

# ==========================================================
# 2. VISUALIZATION: Springer Style Boxplot
# ==========================================================

library(ggplot2)
library(ggbeeswarm)
library(dplyr)
library(scales)

# ==========================================================
# 1. PREP: Binary Classification (Strictly 2 Groups)
# ==========================================================
election_years <- c(2002, 2007, 2013, 2017, 2022)

# Logic: If it is an election year -> "Election". 
# EVERYTHING else (including referendums) -> "Non-Election"
forest_long <- forest_long %>%
  mutate(Year_Category = ifelse(Year %in% election_years, 
                                "Election", 
                                "Non-Election"))

# Force Factor Levels: This ensures only 2 distinct groups exist on the x-axis
forest_long$Year_Category <- factor(forest_long$Year_Category,
                                    levels = c("Non-Election", "Election"))

# ==========================================================
# 2. VISUALIZATION
# ==========================================================

# Colors: Grey for baseline (Non-Election), Orange for Event (Election)
# This is high-contrast and Springer-friendly.
binary_cols <- c("Non-Election" = "#E0E0E0", 
                 "Election"     = "#D55E00") 

p <- ggplot(forest_long, aes(x = Year_Category, y = Forest_Loss, fill = Year_Category)) +
  
  # A. Boxplot
  geom_boxplot(outlier.shape = NA,  # Hide outliers (shown in jitter)
               alpha = 0.8,         # Solid fill
               width = 0.5,         # Width of the box
               size = 0.4) +        # Thickness of lines
  
  # B. Jittered Points (Raw Data)
  geom_quasirandom(width = 0.2, 
                   size = 1.5, 
                   alpha = 0.4, 
                   color = "black") + 
  
  # C. Mean Marker (Diamond)
  stat_summary(fun = mean, 
               geom = "point", 
               shape = 23,          # Diamond
               size = 3, 
               fill = "white",      # White center
               color = "black") +   # Black border
  
  # D. Colors and Labels
  scale_fill_manual(values = binary_cols) +
  labs(
    y = "Forest Loss (per 1,000 ha)",
    x = NULL
  ) +
  
  # E. Enforce Y-Axis Limit (Zooming)
  # coord_cartesian is preferred over scale_y_continuous(limits=...) 
  # because it zooms the view without excluding data from the boxplot stats.
  coord_cartesian(ylim = c(0, 20)) +
  
  # F. Springer Theme
  theme_bw(base_size = 12) + 
  theme(
    legend.position = "none",           # No legend needed (x-axis labels cover it)
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(), # Clean background
    panel.grid.minor = element_blank()
  )

print(p)

# ggsave("Fig2_Boxplot_Binary.tiff", plot = p, width = 84, height = 100, units = "mm", dpi = 300)

# ggsave("Fig2_ForestLoss_Boxplot.tiff", plot = p, width = 84, height = 100, units = "mm", dpi = 300)

library(ggplot2)
library(ggbeeswarm)
library(dplyr)
library(scales)
library(ggbreak) # Required for the "wiggly line" axis break
installed.packages("ggbreak")
# ==========================================================
# 1. PREP: Data & Categories
# ==========================================================
election_years <- c(2002, 2007, 2013, 2017, 2022)

forest_long <- forest_long %>%
  mutate(Year_Category = ifelse(Year %in% election_years, "Election", "Non-Election"))

forest_long$Year_Category <- factor(forest_long$Year_Category,
                                    levels = c("Non-Election", "Election"))

# ==========================================================
# 2. VISUALIZATION with Axis Break
# ==========================================================
binary_cols <- c("Non-Election" = "#E0E0E0", "Election" = "#D55E00")

p <- ggplot(forest_long, aes(x = Year_Category, y = Forest_Loss, fill = Year_Category)) +
  
  # A. Boxplot
  geom_boxplot(outlier.shape = NA, alpha = 0.8, width = 0.5, size = 0.4) +
  
  # B. Jittered Points
  geom_quasirandom(width = 0.2, size = 1.5, alpha = 0.4, color = "black") + 
  
  # C. Mean Marker
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  
  # D. Colors & Labels
  scale_fill_manual(values = binary_cols) +
  labs(
    y = "Forest Loss (per 1,000 ha)",
    x = NULL
  ) +
  
  # E. THE AXIS BREAK (The "Wiggly Line")
  # ---------------------------------------------------------
# This creates the break. 
# You must adjust the vector c(start_break, end_break) based on your data.
# Below means: "Cut the Y-axis at 22, and resume it at 280"
scale_y_break(c(22, 280), 
              scales = 0.2,   # Allocate 20% of vertical space to the top part (outliers)
              ticklabels = c(280, 300, 320)) + # Optional: Manually set ticks for top part
  # ---------------------------------------------------------

# F. Theme
theme_bw(base_size = 12) + 
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black", size = 11),
    axis.title = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    # Specific fix for ggbreak to ensure the axis line looks clean
    axis.line.y.right = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.right = element_blank()
  )

print(p)

# ggsave("Fig2_Boxplot_BrokenAxis.tiff", plot = p, width = 84, height = 100, units = "mm", dpi = 300)




library(ggplot2)
library(dplyr)
library(ggpubr)  # For ggboxplot and publication themes
library(ggbreak) # For the axis break
# install.packages("ggpubr")
# install.packages("ggbreak")
# ==========================================================
# 1. PREP DATA
# ==========================================================
election_years <- c(2002, 2007, 2013, 2017, 2022)

# Create Binary Category
forest_long <- forest_long %>%
  mutate(Year_Category = ifelse(Year %in% election_years, "Election", "Non-Election"))

# Set Order (Non-Election first)
forest_long$Year_Category <- factor(forest_long$Year_Category, 
                                    levels = c("Non-Election", "Election"))

# ==========================================================
# 2. CALCULATE STATISTICS (Mean +/- SD)
# ==========================================================
# This creates a separate dataframe "ag" to hold the text labels
ag <- forest_long %>%
  group_by(Year_Category) %>%
  summarise(
    mean_val = mean(Forest_Loss, na.rm = TRUE),
    sd_val   = sd(Forest_Loss, na.rm = TRUE)
  ) %>%
  mutate(
    # Format: "12.5 ± 4.2"
    label = paste0(round(mean_val, 1), " \u00B1 ", round(sd_val, 1))
  )

# ==========================================================
# 3. GENERATE PLOT WITH GGBOXPLOT
# ==========================================================

# We define the break points here. 
# We will cut the axis at 60 (to leave room for text) and resume at 280 (for outliers).
y_cut_point <- 60 

p <- ggboxplot(forest_long, 
               x = "Year_Category", 
               y = "Forest_Loss",
               color = "Year_Category", 
               palette = c("#F76D5E", "#FFA500"), # Grey vs Vermilion
               add = "jitter",                    # Add raw points
               add.params = list(size = 1.5, alpha = 0.4),
               width = 0.5,
               outlier.shape = NA,                # Hide outliers (jitter shows them)
               bxp.errorbar = TRUE) +             # Add caps to whiskers
  
  # A. Add the Mean +/- SD Text Labels
  # We place them at y = 55 (just below the axis break cut)
  geom_text(data = ag, 
            aes(x = Year_Category, y = 25, label = label, color = Year_Category),
            vjust = 0, fontface = "bold", size = 4, show.legend = FALSE) +
  
  # B. Add the Mean Diamond Marker
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  
  # C. Axis Formatting
  labs(y = "Forest Loss (per 1,000 ha)", x = NULL) +
  theme(legend.position = "none") + 
  
  # D. Apply Axis Break (The "Wiggly Line")
  # Cuts axis at 60, resumes at 280. Outlier section gets 20% of vertical space.
  scale_y_break(c(30, 280), scales = 1, ticklabels = c(280, 300))

print(p)

library(ggplot2)
library(ggpubr)
library(ggbreak)

# ==========================================================
# REFINED PLOT FOR SPRINGER
# ==========================================================

# Springer Standard: Use Neutral for Baseline (Grey), Bold for Event (Vermilion)
# This ensures distinctness in both color and grayscale printing.
springer_palette <- c("#7F7F7F", "#D55E00")

p <- ggboxplot(forest_long, 
               x = "Year_Category", 
               y = "Forest_Loss",
               color = "Year_Category", 
               palette = springer_palette,        # Updated colors
               add = "jitter",
               add.params = list(size = 1.5, alpha = 0.4),
               width = 0.5,
               outlier.shape = NA,
               bxp.errorbar = TRUE) +
  
  # A. Add the Mean +/- SD Text Labels
  # Positioned at y = 28 to sit cleanly between the whisker and the axis break
  geom_text(data = ag, 
            aes(x = Year_Category, y = 28, label = label, color = Year_Category),
            vjust = 0, 
            fontface = "bold", 
            size = 4, 
            show.legend = FALSE) +
  
  # B. Mean Diamond Marker
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
  
  # C. Formatting
  labs(y = "Forest Loss (per 1,000 ha)", x = NULL) +
  theme(
    legend.position = "none",
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(color = "black", size = 10) # Ensure text is absolute black
  ) + 
  
  # D. Axis Break
  # Cut at 35 (just above text). Resume at 280.
  # scales = 0.4: Top section gets 40% of space, Bottom gets 60% (Better focus on boxes)
  scale_y_break(c(35, 280), scales = 0.4, ticklabels = c(280, 300, 320))

print(p)

ggsave("Fig2_ForestLoss_ggpubr_Stats.tiff", plot = p, width = 84, height = 100, units = "mm", dpi = 300)

##################################################################################################
# Define colors
cols <- c("Non-election/Non-referendum" = "#F76D5E",
          "Election"                    = "#FFFFBF",
          "Referendum"                  = "#FFA500")  # orange

# Plot with y-axis capped at 50
ggplot(forest_long, aes(x = Year_Category, y = Forest_Loss, fill = Year_Category)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +               # hide outliers to focus on main data
  geom_quasirandom(width = 0.2, size = 1.5, alpha = 0.5, color = "black") + # jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "blue") + # mean
  scale_fill_manual(values = cols) +
  coord_cartesian(ylim = c(0, 10)) +  # set upper limit to 50
  labs(
    title = "Forest Loss by Election, Referendum, and Other Years (capped at 50)",
    x = "",
    y = "Forest Loss (per 1000 ha)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")




# ----------------------------------------------------------
# 1.3 (Alternative) Visualization:
# Density plot of forest loss by election status
# ----------------------------------------------------------

library(ggplot2)

# Define custom colors
cols <- c("0" = "#F76D5E",   # Non-election years
          "1" = "#FFFFBF")  # Election years

ggplot(forest_long,
       aes(x = Forest_Loss,
           fill = factor(Election_Year))) +
  geom_density(
    alpha = 0.8,
    color = NA
  ) +
  scale_fill_manual(
    values = cols,
    labels = c("Non-election years", "Election years")
  ) +
  labs(
    title = "Distribution of Forest Loss in Election vs Non-election Years",
    x = "Forest Loss (per 1000 ha)",
    y = "Density",
    fill = ""
  ) +
  theme_minimal()
# ----------------------------------------------------------
# Density plot with log-transformed forest loss
# ----------------------------------------------------------

cols <- c("0" = "#F76D5E",
          "1" = "#FFFFBF")

forest_long <- forest_long %>%
  mutate(
    log_Forest_Loss = log1p(Forest_Loss)
  )

ggplot(forest_long,
       aes(x = log_Forest_Loss,
           fill = factor(Election_Year))) +
  geom_density(alpha = 0.8, color = NA) +
  scale_fill_manual(
    values = cols,
    labels = c("Non-election years", "Election years")
  ) +
  labs(
    title = "Distribution of Forest Loss (log-transformed)",
    x = "log(Forest Loss + 1)",
    y = "Density",
    fill = ""
  ) +
  theme_minimal()


ggplot(forest_long,
       aes(x = Forest_Loss,
           fill = factor(Election_Year))) +
  geom_density(alpha = 0.8, color = NA) +
  scale_x_log10() +
  scale_fill_manual(values = cols) +
  labs(
    title = "Distribution of Forest Loss (log-scaled x-axis)",
    x = "Forest Loss (log scale)",
    y = "Density",
    fill = ""
  ) +
  theme_minimal()


library(ggplot2)

# Define custom colors
cols <- c("Non-election years" = "#F76D5E",
          "Election years"     = "#FFFFBF")

# Map Election_Year to descriptive factor
forest_long <- forest_long %>%
  mutate(
    Election_Label = factor(Election_Year,
                            levels = c(0,1),
                            labels = c("Non-election years", "Election years"))
  )

# Density plot with log-scaled x-axis
ggplot(forest_long,
       aes(x = Forest_Loss,
           fill = Election_Label)) +
  geom_density(alpha = 0.8, color = NA) +
  scale_x_log10() +
  scale_fill_manual(values = cols) +
  labs(
    title = "Distribution of Forest Loss (log-scaled x-axis)",
    x = "Forest Loss (log scale)",
    y = "Density",
    fill = ""
  ) +
  theme_minimal()




# ----------------------------------------------------------
# 1.4 Statistical test: Difference in means
# ----------------------------------------------------------
t.test(Forest_Loss ~ Election_Year, data = forest_long)
wilcox.test(Forest_Loss ~ Election_Year, data = forest_long)

# ----------------------------------------------------------
# 1.5 Regression: Election effect (panel mixed model)
# ----------------------------------------------------------
model_q1 <- lmer(
  Forest_Loss ~ Election_Year + Conflict_Index +
    (1 | Forest_Block),
  data = forest_long
)

summary(model_q1)

# ==========================================================
# QUESTION 2: Spatial correlation between conflict and forest loss
# ==========================================================

# ----------------------------------------------------------
# 2.1 Visualization: Conflict intensity vs forest loss
# ----------------------------------------------------------
ggplot(forest_long,
       aes(x = Conflict_Index, y = Forest_Loss)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(
    title = "Conflict Intensity vs Forest Loss",
    x = "Conflict Intensity Index (±6 months)",
    y = "Forest Loss (per 1000 ha)"
  ) +
  theme_minimal()


# ----------------------------------------------------------
# 2.2 Prepare dataset: average values per forest block
# (Non-spatial version)
# ----------------------------------------------------------

forest_avg <- forest_long %>%
  group_by(Forest_Block) %>%
  summarise(
    Forest_Loss    = mean(Forest_Loss, na.rm = TRUE),
    Conflict_Index = mean(Conflict_Index, na.rm = TRUE),
    .groups = "drop"
  )

head(forest_avg)


# ----------------------------------------------------------
# 2.3 Spatial autocorrelation: Moran's I
# ----------------------------------------------------------
nb <- poly2nb(forest_spatial)
lw <- nb2listw(nb, style = "W")

moran.test(forest_spatial$Forest_Loss, lw)
moran.test(forest_spatial$Conflict_Index, lw)

# ----------------------------------------------------------
# 2.4 Spatial regression (Spatial Lag Model)
# ----------------------------------------------------------
sar_model <- lagsarlm(
  Forest_Loss ~ Conflict_Index,
  data = forest_spatial,
  listw = lw
)

summary(sar_model)

# ==========================================================
# QUESTION 3: Governance regimes and tree cover density
# ==========================================================

library(ggplot2)

# Make Election_Label if not already created
forest_long <- forest_long %>%
  mutate(
    Election_Label = factor(Election_Year,
                            levels = c(0,1),
                            labels = c("Non-election year", "Election year"))
  )

# ----------------------------------------------------------
# 3.1 Faceted time-series: forest loss trends by election cycle
# ----------------------------------------------------------
ggplot(forest_long, aes(x = Year, y = Forest_Loss, group = Forest_Block, color = Election_Label)) +
  geom_line(alpha = 0.4) +
  facet_wrap(~ Election_Label) +
  labs(
    title = "Forest Loss Trends by Election Cycle",
    x = "Year",
    y = "Forest Loss (per 1000 ha)",
    color = "Election Cycle"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Non-election year" = "#F76D5E",
                                "Election year" = "#FFFFBF"))

# Summarize forest loss per year and election cycle
forest_summary <- forest_long %>%
  group_by(Year, Election_Label) %>%
  summarise(
    Mean_Loss = mean(Forest_Loss, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(forest_summary, aes(x = Year, y = Election_Label, fill = Mean_Loss)) +
  geom_tile() +
  scale_fill_viridis(option = "C") +
  labs(
    title = "Mean Forest Loss by Year and Election Cycle",
    x = "Year",
    y = "",
    fill = "Mean Loss"
  ) +
  theme_minimal()




# ----------------------------------------------------------
# 3.2 Visualization: Faceted time-series by governance
# ----------------------------------------------------------
ggplot(forest_long,
       aes(x = Year, y = Forest_Loss, color = Governance)) +
  geom_line() +
  facet_wrap(~ Governance) +
  labs(
    title = "Forest Loss Trends by Governance Regime",
    y = "Forest Loss (per 1000 ha)"
  ) +
  theme_minimal()

# ----------------------------------------------------------
# 3.3 Visualization: Heatmap (Governance × Tree cover)
# ----------------------------------------------------------
forest_summary <- forest_long %>%
  group_by(Governance, Tree_Cover_Class) %>%
  summarise(
    Mean_Loss = mean(Forest_Loss, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(forest_summary,
       aes(x = Tree_Cover_Class,
           y = Governance,
           fill = Mean_Loss)) +
  geom_tile() +
  scale_fill_viridis(option = "C") +
  labs(
    title = "Forest Loss by Governance Regime and Tree Cover Density",
    fill = "Mean Loss"
  ) +
  theme_minimal()




############################################################
## END OF SCRIPT
############################################################

library(dplyr)

# Define election years
election_years <- c(2002, 2003, 2007, 2008, 2012, 2013, 2017, 2028, 2022, 2023)

# Reassign Year_Category: Election vs Non-election (referendum included in Non-election)
forest_long <- forest_long %>%
  mutate(
    Year_Category = ifelse(Year %in% election_years, "Election", "Non-election")
  )

# Convert to factor for plotting order
forest_long$Year_Category <- factor(forest_long$Year_Category,
                                    levels = c("Non-election", "Election"))


library(ggplot2)
library(ggbeeswarm)

# Define colors
cols <- c("Non-election" = "#F76D5E",
          "Election"     = "#FFFFBF")

# Boxplot
ggplot(forest_long, aes(x = Year_Category, y = Forest_Loss, fill = Year_Category)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +               # hide default outliers
  geom_quasirandom(width = 0.2, size = 1.5, alpha = 0.5, color = "black") + # jittered points
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "blue") + # mean
  scale_fill_manual(values = cols) +
  coord_cartesian(ylim = c(0, 10)) +  # cap y-axis at 50
  labs(
    title = "Forest Loss by Election vs Non-election Years",
    x = "",
    y = "Forest Loss (per 1000 ha)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "none")



library(ggplot2)
library(dplyr)

# Summarize by Year and Year_Category
forest_summary <- forest_long %>%
  group_by(Year, Year_Category) %>%
  summarise(
    Mean_Loss     = mean(Forest_Loss, na.rm = TRUE),
    Mean_Conflict = mean(Conflict_Index, na.rm = TRUE),
    .groups = "drop"
  )

# Convert Year to factor for discrete y-axis
forest_summary <- forest_summary %>%
  mutate(Year = factor(Year, levels = sort(unique(Year))))

# Lollipop plot for Forest Loss
ggplot(forest_summary, aes(x = Mean_Loss, y = Year)) +
  geom_segment(aes(x = 0, xend = Mean_Loss, y = Year, yend = Year), color = "grey50") +
  geom_point(aes(color = Year_Category), size = 3) +
  scale_color_manual(values = c("Non-election" = "#F76D5E",
                                "Election"     = "#f1c232")) +
  facet_wrap(~ Year_Category, scales = "free_y") +
  labs(
    title = "Average Forest Loss by Year and Election Cycle",
    x = "Mean Forest Loss (per 1000 ha)",
    y = "Year",
    color = "Year Type"
  ) +
  theme_minimal()



library(ggplot2)
library(dplyr)

# ----------------------------------------------------------
# Define election-related conflict years (including spillovers)
# ----------------------------------------------------------
election_conflict_years <- c(
  2002, 2003,
  2007, 2008,
  2012, 2013,
  2017, 2018,
  2022, 2023
)

# ----------------------------------------------------------
# Reclassify Year Category for conflict analysis
# ----------------------------------------------------------
forest_long_conflict <- forest_long %>%
  mutate(
    Year_Category = ifelse(
      Year %in% election_conflict_years,
      "Election-related conflict years",
      "Non-election years"
    )
  )

# Ensure ordering
forest_long_conflict$Year_Category <- factor(
  forest_long_conflict$Year_Category,
  levels = c("Non-election years", "Election-related conflict years")
)

# ----------------------------------------------------------
# Summarize conflict data by Year and Category
# ----------------------------------------------------------
conflict_summary <- forest_long_conflict %>%
  group_by(Year, Year_Category) %>%
  summarise(
    Mean_Conflict = mean(Conflict_Index, na.rm = TRUE),
    .groups = "drop"
  )

# Convert Year to factor for discrete y-axis
conflict_summary <- conflict_summary %>%
  mutate(Year = factor(Year, levels = sort(unique(Year))))

# ----------------------------------------------------------
# Lollipop plot: Election-related conflict counts
# ----------------------------------------------------------
ggplot(conflict_summary, aes(x = Mean_Conflict, y = Year)) +
  geom_segment(
    aes(x = 0, xend = Mean_Conflict, y = Year, yend = Year),
    color = "grey50"
  ) +
  geom_point(aes(color = Year_Category), size = 3) +
  scale_color_manual(values = c(
    "Non-election years"              = "#F76D5E",
    "Election-related conflict years" = "#f1c232"
  )) +
  facet_wrap(~ Year_Category, scales = "free_y") +
  labs(
    title = "Average Conflict Events in Election-related vs Non-election Years",
    x = "Mean Conflict Events (±6-month election window)",
    y = "Year",
    color = "Year Type"
  ) +
  theme_minimal()





# ----------------------------------------------------------
# 3.4 Statistical tests and interaction regression
# ----------------------------------------------------------
# install.packages("lmerTest")
library(lme4)
library(lmerTest)  # for p-values

# Log-transform forest loss to handle skewness
forest_long <- forest_long %>%
  mutate(Log_Forest_Loss = log1p(Forest_Loss))

# Mixed-effects model with forest block random intercepts
mixed_model <- lmer(
  Log_Forest_Loss ~ Year_Category + (1 | Forest_Block),
  data = forest_long
)

summary(mixed_model)



# Mixed-effects model including conflict intensity
conflict_mechanism_model <- lmer(
  Log_Forest_Loss ~ Conflict_Index + Year_Category + (1 | Forest_Block),
  data = forest_long
)

summary(conflict_mechanism_model)





# 95% confidence intervals for fixed effects
confint(mixed_model, level = 0.95, method = "Wald")
confint(conflict_mechanism_model, level = 0.95, method = "Wald")

# More reliable but computationally heavier
confint(mixed_model, method = "profile")
confint(conflict_mechanism_model, method = "profile")


library(ggplot2)

ggplot(forest_long,
       aes(x = Conflict_Index,
           y = Forest_Loss,
           color = Year_Category)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_y_continuous(trans = "log1p") +
  labs(
    title = "Forest Loss vs Conflict Intensity",
    x = "Conflict Events (±6-month election window)",
    y = "Forest Loss (log-scaled)",
    color = "Year Type"
  ) +
  theme_minimal()


library(emmeans)
# install.packages("emmeans")
# Marginal means by election category
emm_election <- emmeans(mixed_model, ~ Year_Category)

# Convert to data frame
emm_df <- as.data.frame(emm_election)


ggplot(forest_long,
       aes(x = Conflict_Index,
           y = Log_Forest_Loss,
           color = Year_Category)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Conflict Intensity and Forest Loss",
    x = "Conflict Events (±6-month election window)",
    y = "Log Forest Loss",
    color = "Year Type"
  ) +
  theme_minimal()



set.seed(123)

placebo_years <- sample(
  setdiff(unique(forest_long$Year),
          c(2002, 2003, 2007, 2008, 2012, 2013, 2017, 2018, 2022, 2023)),
  size = 6
)

forest_long <- forest_long %>%
  mutate(Placebo_Election =
           ifelse(Year %in% placebo_years, "Election", "Non-election"))

placebo_model <- lmer(
  Log_Forest_Loss ~ Placebo_Election + (1 | Forest_Block),
  data = forest_long
)

summary(placebo_model)


emm_placebo <- emmeans(placebo_model, ~ Placebo_Election)
emm_placebo_df <- as.data.frame(emm_placebo)

ggplot(emm_placebo_df,
       aes(x = Placebo_Election, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.15) +
  labs(
    title = "Placebo Test: No Effect in Random Non-Election Years",
    x = "Placebo Year Type",
    y = "Predicted Log Forest Loss (±95% CI)"
  ) +
  theme_minimal()



