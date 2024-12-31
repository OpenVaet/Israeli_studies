library(ggplot2)
library(dplyr)

# ----------------------------------------------------------------------------
#  Reads & Combines the data
# ----------------------------------------------------------------------------

# Reads the CSV files
dagan_2021          <- read.csv("dagan_2021.csv", header = TRUE)
magen_2022          <- read.csv("magen_2022.csv", header = TRUE)
moreira_2_doses     <- read.csv("moreira_2022_2_doses.csv", header = TRUE)
moreira_3_doses     <- read.csv("moreira_2022_3_doses.csv", header = TRUE)

# Assigns labels for each dataset
dagan_2021$Group      <- "Dagan (0 dose)"
magen_2022$Group      <- "Magen (3 doses)"
moreira_2_doses$Group <- "Moreira (2 doses)"
moreira_3_doses$Group <- "Moreira (3 doses)"

# Combines all data into one data frame
all_data <- bind_rows(
  dagan_2021,
  magen_2022,
  moreira_2_doses,
  moreira_3_doses
)

# ----------------------------------------------------------------------------
#  HELPER: GET CUMULATIVE INCIDENCE AT SPECIFIC DAY with a simple "closest match" approach
# ----------------------------------------------------------------------------

get_cuminc_at_day <- function(data, group_label, day) {
  df <- data %>% filter(Group == group_label)
  # If the exact day is present, return it.
  # Otherwise, approximates by nearest day or do a simple linear interpolation.
  
  if(day %in% df$Day) {
    return(df$CumulativeIncidenceRate[df$Day == day])
  } else {
    # For simplicity, approximates by the closest day in the dataset
    idx <- which.min(abs(df$Day - day))
    return(df$CumulativeIncidenceRate[idx])
  }
}

# ----------------------------------------------------------------------------
#  Computes Fractions for Dagan & Magen
# ----------------------------------------------------------------------------

# We assume day 32 is the 'final' day for both Dagan and Magen
final_day <- 32
intervals <- list(c(0,7), c(7,14), c(14,21), c(21,28))

groups_of_interest <- c("Dagan (0 dose)", "Magen (3 doses)")

# We'll store fraction of total for each interval, for each group
group_fractions <- data.frame()

for(g in groups_of_interest) {
  
  # final total at day 32
  total_32 <- get_cuminc_at_day(all_data, g, final_day)
  start_0  <- get_cuminc_at_day(all_data, g, 0)
  if(length(start_0) == 0 || is.na(start_0)) start_0 <- 0  # if day=0 wasn't in data
  
  # Safeguard if total_32 == 0
  if(total_32 <= 0) {
    # Then everything is zero => no fractions
    next
  }
  
  # For each sub-interval
  out <- c()
  cum_previous <- start_0
  for(intv in intervals) {
    d1 <- intv[1]
    d2 <- intv[2]
    
    val_d1 <- get_cuminc_at_day(all_data, g, d1)
    val_d2 <- get_cuminc_at_day(all_data, g, d2)
    if(is.na(val_d1)) val_d1 <- 0
    if(is.na(val_d2)) val_d2 <- val_d1
    
    incr     = val_d2 - val_d1  # how much was added in that interval
    totalNet = total_32 - start_0
    fraction = ifelse(totalNet > 0, incr / totalNet, 0)
    
    out <- c(out, fraction)
  }
  
  # Stores results
  group_fractions <- rbind(group_fractions,
                           data.frame(
                             Group = g,
                             # Each row => the fraction of total accrual in these intervals
                             frac_0_7   = out[1],
                             frac_7_14  = out[2],
                             frac_14_21 = out[3],
                             frac_21_28 = out[4]
                           ))
}

# Averages the fraction across the two groups
avg_fractions <- colMeans(group_fractions[, c("frac_0_7", "frac_7_14", "frac_14_21", "frac_21_28")], na.rm = TRUE)

# ----------------------------------------------------------------------------
#  Builds Bar-On Data using calculated fractions
# ----------------------------------------------------------------------------

# The final day-32 total for Bar-On
bar_on_final <- 85.46

# Starts with day=0 => 0
baron_days <- c(0, 7, 14, 21, 28, 32)
baron_vals <- numeric(length(baron_days))

baron_vals[1] <- 0  # day 0 => 0

# Adds increments for intervals [0-7], [7-14], [14-21], [21-28]
#    using the average fraction from dagan + magen
#    fraction_0_7 = fraction of the *total 85.46*
#    So after day=0, day=7 = fraction_0_7 * 85.46, etc.
accumulated <- 0
for(i in seq_along(intervals)) {
  frac_this <- avg_fractions[i]
  this_incr <- frac_this * bar_on_final
  new_value <- accumulated + this_incr
  baron_vals[i+1] <- new_value
  accumulated <- new_value
}

# For the last interval [28-32], just adds left-over
leftover <- bar_on_final - baron_vals[5]
baron_vals[6] <- baron_vals[5] + leftover
bar_on_inferred <- data.frame(
  Day = baron_days,
  CumulativeIncidenceRate = baron_vals,
  Group = "Bar-On (2 doses)"
)

# ----------------------------------------------------------------------------
# Combines with all data & builds plot
# ----------------------------------------------------------------------------

all_data_including_baron <- bind_rows(all_data, bar_on_inferred)

# Creates a new column "StudyType" to label 'Advertised' vs 'Real World'
all_data_including_baron <- all_data_including_baron %>%
  mutate(
    Legend = case_when(
      Group == "Moreira (2 doses)" ~ "Advertised: Moreira (2 doses)",
      Group == "Moreira (3 doses)" ~ "Advertised: Moreira (3 doses)",
      Group == "Dagan (0 dose)"    ~ "Real World: Dagan (0 dose)",
      Group == "Bar-On (2 doses)"  ~ "Real World: Bar-On (2 doses)",
      Group == "Magen (3 doses)"   ~ "Real World: Magen (3 doses)"
    )
  )

# Makes sure these factor levels are in the order we want them to appear in the legend
desired_levels <- c(
  "Advertised", 
  "Advertised: Moreira (2 doses)",
  "Advertised: Moreira (3 doses)",
  "Real World",
  "Real World: Dagan (0 dose)",
  "Real World: Bar-On (2 doses)",
  "Real World: Magen (3 doses)"
)
all_data_including_baron$Legend <- factor(all_data_including_baron$Legend, 
                                          levels = desired_levels)

# Creates DUMMY rows for "Advertised" and "Real World":
dummy <- data.frame(
  Day = NA,  # or 0, doesn't matter since it won't be drawn
  CumulativeIncidenceRate = NA,
  Group = NA,    # no real group
  Legend = c("Advertised", "Real World")
)
# Make sure they match the factor levels
dummy$Legend <- factor(dummy$Legend, levels = desired_levels)

# Bind them in
plot_df <- bind_rows(all_data_including_baron, dummy)

#------------------------------------------------------------
#    Now maps:
#    aes(color = Legend, linetype = Legend)
#    so that each distinct factor level 
#    gets its own color + line type in the legend.
#------------------------------------------------------------
ggplot(plot_df, 
       aes(x = Day, y = CumulativeIncidenceRate, 
           color = Legend, linetype = Legend)) +
  geom_line(size = 1.2, na.rm = TRUE) + 
  geom_point(size = 2, na.rm = TRUE) +
  
  #----------------------------------------------------------
#    Defines custom scale_*_manual with the EXACT same 'breaks' 
#    and matching 'labels' so we can label them like subheadings.
#    For "Advertised" & "Real World", we assign 'blank' linetype
#    or a color that won't appear on the plot (or override later).
#----------------------------------------------------------
scale_color_manual(
  name = NULL,  # no main title for the legend
  breaks = desired_levels,
  values = c(
    # sub-headings -> 'black' or anything, we will override
    "Advertised" = "black",
    "Advertised: Moreira (2 doses)" = "orange",
    "Advertised: Moreira (3 doses)" = "blue",
    "Real World" = "black",
    "Real World: Dagan (0 dose)" = "red",
    "Real World: Bar-On (2 doses)" = "orange",
    "Real World: Magen (3 doses)" = "blue"
  ),
  labels = c(
    "Advertised" = "Advertised",
    "Advertised: Moreira (2 doses)" = "   Moreira (2 doses)",
    "Advertised: Moreira (3 doses)" = "   Moreira (3 doses)",
    "Real World" = "Real World",
    "Real World: Dagan (0 dose)" = "   Dagan (0 dose)",
    "Real World: Bar-On (2 doses)" = "   Bar-On (2 doses)",
    "Real World: Magen (3 doses)" = "   Magen (3 doses)"
  )
) +
  scale_linetype_manual(
    name = NULL,
    breaks = desired_levels,
    values = c(
      "Advertised" = "blank",  # sub-heading => no line
      "Advertised: Moreira (2 doses)" = "solid",
      "Advertised: Moreira (3 doses)" = "solid",
      "Real World" = "blank",  # sub-heading => no line
      "Real World: Dagan (0 dose)" = "dashed",
      "Real World: Bar-On (2 doses)" = "dashed",
      "Real World: Magen (3 doses)" = "dashed"
    ),
    labels = c(
      "Advertised" = "Advertised",
      "Advertised: Moreira (2 doses)" = "   Moreira (2 doses)",
      "Advertised: Moreira (3 doses)" = "   Moreira (3 doses)",
      "Real World" = "Real World",
      "Real World: Dagan (0 dose)" = "   Dagan (0 dose)",
      "Real World: Bar-On (2 doses)" = "   Bar-On (2 doses)",
      "Real World: Magen (3 doses)" = "   Magen (3 doses)"
    )
  ) +
  
  #----------------------------------------------------------
#    Overrides the legend so that "Advertised" and "Real World" 
#    entries have no lines/points displayed.  They act 
#    like headings for the subsequent items.
#----------------------------------------------------------
guides(
  color = guide_legend(
    # Makes each legend key wider, so the dashed line is clearly displayed
    keywidth = 3,  # You can bump this up to 3 or 4 if needed
    override.aes = list(
      # We have 7 factor levels => 7 overrides
      linetype = c("blank", "solid", "solid", "blank", 
                   "dashed", "dashed", "dashed"),
      shape    = c(NA, NA, NA, NA, NA, NA, NA),
      # 2) Increase the line "size" to make the dashes thicker
      size     = c(0, 1.2, 1.2, 0, 1.2, 1.2, 1.2)
    )
  ),
  linetype = guide_legend(
    keywidth = 2,
    override.aes = list(
      shape    = c(NA, NA, NA, NA, NA, NA, NA),
      size     = c(0, 1.2, 1.2, 0, 1.2, 1.2, 1.2)
    )
  )
) +
  
  geom_text(
    data = subset(plot_df, !is.na(Day)),   # or use dplyr: filter(!is.na(Day))
    aes(label = round(CumulativeIncidenceRate, 2)),
    vjust = -0.5,
    size = 5,
    show.legend = FALSE
  )+
  
  #----------------------------------------------------------
# Then usual labels & theme
#----------------------------------------------------------
labs(
  title = "Cumulative Incidence Rate / 100,000 Person-days Over Time",
  x = "Day",
  y = "Cumulative Incidence Rate (log10)"
) +
  theme_minimal(base_size = 18) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 20),
    legend.position = "right"
  ) +
  scale_y_log10()
