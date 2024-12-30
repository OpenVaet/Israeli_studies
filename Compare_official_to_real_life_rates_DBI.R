library(ggplot2)
library(dplyr)

# ----------------------------------------------------------------------------
#  Reads & Combines the data
# ----------------------------------------------------------------------------

# 1. Reads the CSV files
dagan_2021          <- read.csv("dagan_2021.csv", header = TRUE)
magen_2022          <- read.csv("magen_2022.csv", header = TRUE)
moreira_2_doses     <- read.csv("moreira_2022_2_doses.csv", header = TRUE)
moreira_3_doses     <- read.csv("moreira_2022_3_doses.csv", header = TRUE)

# 2. Assigns labels for each dataset
dagan_2021$Group      <- "Dagan (0 dose)"
magen_2022$Group      <- "Magen (3 doses)"
moreira_2_doses$Group <- "Moreira (2 doses)"
moreira_3_doses$Group <- "Moreira (3 doses)"

# 3. Combines all data into one data frame
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

# Average the fraction across the two groups
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

ggplot(
  all_data_including_baron,
  aes(
    x = Day,
    y = CumulativeIncidenceRate,
    color = Group,
    linetype = Group
  )
) +
  # Lines
  geom_line(size = 2) +
  
  # Points
  geom_point(size = 4) +
  
  # Data labels
  geom_text(
    aes(label = round(CumulativeIncidenceRate, 2)),
    vjust = -0.5,
    size = 5,
    show.legend = FALSE
  ) +
  
  # COLORS: match 2 doses vs. 3 doses
  scale_color_manual(
    values = c(
      "Moreira (3 doses)" = "darkblue",
      "Moreira (2 doses)" = "orange",
      "Dagan (0 dose)"    = "red",
      "Magen (3 doses)"   = "darkblue",
      "Bar-On (2 doses)"  = "orange"
    )
  ) +
  
  # LINE TYPES: 3 doses vs. 2 doses vs. 0 dose
  scale_linetype_manual(
    values = c(
      "Moreira (3 doses)" = "solid",   # darkblue
      "Moreira (2 doses)" = "solid",   # orange
      "Dagan (0 dose)"    = "dashed",  # red
      "Magen (3 doses)"   = "dashed",  # darkblue
      "Bar-On (2 doses)"  = "dashed"   # orange
    )
  ) +
  
  # Axis and legend labels
  labs(
    title    = "Cumulative Incidence Rate / 100,000 Person-days Over Time",
    x        = "Day",
    y        = "Cumulative Incidence Rate (log10)",
    color    = "Study & Dose Group",
    linetype = "Study & Dose Group"
  ) +
  
  theme_minimal(base_size = 20) +
  theme(
    plot.title      = element_text(face = "bold", hjust = 0.5, size = 35),
    legend.position = "bottom",
    legend.text     = element_text(size = 20),
    legend.title    = element_text(size = 22),
    axis.title      = element_text(size = 20),
    axis.text       = element_text(size = 18)
  ) +
  
  scale_y_log10()
