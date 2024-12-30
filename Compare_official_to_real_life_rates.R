
library(ggplot2)
library(dplyr)

# 1. Read the CSV files
magen_2022          <- read.csv("magen_2022.csv", header = TRUE)
moreira_2_doses     <- read.csv("moreira_2022_2_doses.csv", header = TRUE)
moreira_3_doses     <- read.csv("moreira_2022_3_doses.csv", header = TRUE)

# 2. Assign labels for each dataset
magen_2022$Group      <- "Magen (3 doses)"
moreira_2_doses$Group <- "Moreira (2 doses)"
moreira_3_doses$Group <- "Moreira (3 doses)"

# 3. Combine all data into one data frame
all_data <- bind_rows(magen_2022,
                      moreira_2_doses,
                      moreira_3_doses)

# 4. Create the plot using ggplot2
ggplot(all_data, aes(x = Day, y = CumulativeIncidenceRate, color = Group)) +
  # Lines: make them thicker
  geom_line(size = 2) +
  
  # Points on each data value
  geom_point(size = 4) +
  
  # Data labels directly on the plot
  geom_text(
    aes(label = round(CumulativeIncidenceRate, 2)),
    vjust = -0.5,
    size = 5,
    show.legend = FALSE
  ) +
  
  # Manually set colors for each group
  scale_color_manual(
    values = c(
      "Moreira (3 doses)" = "darkblue",
      "Moreira (2 doses)" = "blue",
      "Magen (3 doses)"   = "purple"
    )
  ) +
  
  # Axis and legend labels
  labs(
    title = "Cumulative Incidence Rate / 100 000 Person-days Over Time",
    x = "Day",
    y = "Cumulative Incidence Rate (log10)",
    color = "Study & Dose Group"
  ) +
  
  # Use a larger base size in theme_minimal to get larger text overall
  theme_minimal(base_size = 20) +    # Increased from 14 to 18
  
  # Further theme customizations
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 35),
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    legend.title = element_text(size = 22),
    
    # Adjust axis text and titles separately, if desired
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18)
  ) +
  
  # Log-transform the y-axis
  scale_y_log10()

