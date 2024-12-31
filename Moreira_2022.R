# ---------------------------------------------------------
# Approximate Chart Digitization
# ---------------------------------------------------------
# Install/load required packages
# p.e install.packages("imager")
library(imager)
library(dplyr)
library(knitr)

# ----------------------------
# 1. Loads the image
# ----------------------------
img <- load.image("Moreira_2022.png")
width  <- width(img)
height <- height(img)
cat("Image size:", width, "x", height, "\n")

# ----------------------------
# 2. Converts to data frame
# ----------------------------
# imager::as.data.frame() yields:
#   x, y   = pixel coordinates
#   cc     = color channel index (1=R, 2=G, 3=B, 4=Alpha if present)
#   value  = intensity (0..1)
df <- as.data.frame(img, wide = "c") 

# The original coordinate system has y increasing from TOP to BOTTOM.
# We invert the y as we want y=0 at the bottom:
df$y <- height - df$y

# ----------------------------
# 3. Defines color thresholds
# ----------------------------
blue_ref   <- c(0.4078, 0.5569, 0.6431)
orange_ref <- c(0.8118, 0.6431, 0.3608)

# Measures Euclidean distance in RGB space:
color_distance <- function(pixel, ref) {
  sqrt((pixel[1] - ref[1])^2 + (pixel[2] - ref[2])^2 + (pixel[3] - ref[3])^2)
}

# Chooses distance thresholds that reliably pick out the lines
threshold_blue   <- 0.15
threshold_orange <- 0.15

# Creates a function to label each pixel as "blue", "orange", or "bg"
df <- df %>%
  rowwise() %>%
  mutate(
    dist_blue   = color_distance(c_across(c.1:c.3), blue_ref),
    dist_orange = color_distance(c_across(c.1:c.3), orange_ref),
    color_label = case_when(
      dist_blue   < threshold_blue   ~ "blue",
      dist_orange < threshold_orange ~ "orange",
      TRUE                           ~ "bg"     # background or something else
    )
  ) %>%
  ungroup()

# ----------------------------
# 4. Extracts each curve’s pixels
# ----------------------------
blue_pixels   <- df %>% filter(color_label == "blue")
orange_pixels <- df %>% filter(color_label == "orange")

# ----------------------------
# 5. Converts pixel coords to chart coords
# ----------------------------
#   * The horizontal axis runs from 0 days at x=0 to 42 days at x=width
#   * The vertical axis runs from 0% at y=0 to 3.5% at y=height

max_days       <- 42   
max_incidence  <- 3.5

pixel_to_days  <- function(x_pix)  x_pix * (max_days / width)
pixel_to_pct   <- function(y_pix)  y_pix * (max_incidence / height)

blue_pixels <- blue_pixels %>%
  mutate(
    day = pixel_to_days(x),
    pct = pixel_to_pct(y)
  )

orange_pixels <- orange_pixels %>%
  mutate(
    day = pixel_to_days(x),
    pct = pixel_to_pct(y)
  )

# ----------------------------
# 6. Aggregates/average by day 
# ----------------------------
# A chart line at each x has multiple “line” pixels (the thickness of the line).
# We can group by day “bins” or do a direct smoothing. For simplicity:
#   1) Round day to nearest 0.1 (or 0.5) to group nearby pixels
#   2) Average the y% within each bin

bin_width <- 0.5  # e.g. half a day
blue_binned <- blue_pixels %>%
  mutate(day_bin = round(day / bin_width) * bin_width) %>%
  group_by(day_bin) %>%
  summarize(pct_mean = mean(pct), .groups = "drop")

orange_binned <- orange_pixels %>%
  mutate(day_bin = round(day / bin_width) * bin_width) %>%
  group_by(day_bin) %>%
  summarize(pct_mean = mean(pct), .groups = "drop")

# ----------------------------
# 7. Interpolation
# ----------------------------
# Creates interpolation functions so we can query any day
blue_fun   <- approxfun(blue_binned$day_bin, blue_binned$pct_mean, rule = 2)
orange_fun <- approxfun(orange_binned$day_bin, orange_binned$pct_mean, rule = 2)

# Days of interest
days_to_check <- c(7, 14, 21, 28, 35, 42)

# Evaluates
blue_vals   <- blue_fun(days_to_check)
orange_vals <- orange_fun(days_to_check)

# ----------------------------
# 8. Prints results
# ----------------------------
results <- data.frame(
  Day        = days_to_check,
  Dose2Pct = blue_vals,
  Dose3Pct = orange_vals
)

print(results)

# ----------------------------
# 9. Adds day 0 = 0% row
# ----------------------------
# The question states that at day 0 the incidence is 0 for both.
results <- rbind(
  data.frame(Day = 0, Dose2Pct = 0, Dose3Pct = 0),
  results
)

# ----------------------------
# 10. Creates a 'persons at risk' DataFrame
# ----------------------------
persons_at_risk <- data.frame(
  Day     = c(0,  7,   14,  21,  28,  35,  42),
  Dose2 = c(4943, 4931, 4910, 4869, 4827, 4780, 4754),
  Dose3 = c(5003, 4995, 4990, 4990, 4988, 4978, 4975)
)

# ----------------------------
# 11. Merges 'results' with 'persons_at_risk' by Day
# ----------------------------
merged_df <- merge(results, persons_at_risk, by = "Day")

# ----------------------------
# 12. Computes number of events:
#     events = round( (Pct / 100) * at_risk )
# ----------------------------
merged_df$Dose2Events <- round((merged_df$Dose2Pct / 100) * merged_df$Dose2)
merged_df$Dose3Events <- round((merged_df$Dose3Pct / 100) * merged_df$Dose3)

# ----------------------------
# 13. Inspects & prints final DataFrame
# ----------------------------
print(merged_df)

# Re-labels columns:
table_for_paper <- merged_df %>%
  select(
    Day,
    "N at risk (2D)"          = Dose2,
    "N at risk (3D)"          = Dose3,
    "Cumulative Incidence (%) 2D" = Dose2Pct,
    "Cumulative Incidence (%) 3D" = Dose3Pct,
    "Cumulative Events (2D)"      = Dose2Events,
    "Cumulative Events (3D)"      = Dose3Events
  )

# Prints a formatted table.
kable(
  table_for_paper,
  digits   = 2,
  align    = "c",
  caption  = "Cumulative incidence and events by Day in Moreira 2022."
)

# ----------------------------
# 14. Formats final DataFrames (one for 2 doses, one for 3 doses)
# ----------------------------
two_doses_df <- merged_df %>%
  select(
    Day,
    PersonsAtRisk     = Dose2,
    CumulativeEvents  = Dose2Events
  )

three_doses_df <- merged_df %>%
  select(
    Day,
    PersonsAtRisk     = Dose3,
    CumulativeEvents  = Dose3Events
  )

# ---------------------------------------------------------
# 15. Incidence-rate calculation
# ---------------------------------------------------------
calc_incidence_rate <- function(df) {
  # df columns required: Day, PersonsAtRisk, CumulativeEvents
  
  # The final cumulative event count
  final_events <- df$CumulativeEvents[nrow(df)]
  
  # Creates intervals from row i to row i+1
  df_intervals <- df %>%
    mutate(
      EndDay         = lead(Day),
      StartPop       = PersonsAtRisk,
      EndPop         = lead(PersonsAtRisk),
      IntervalLength = EndDay - Day
    ) %>%
    # drops the last row because it doesn't have a next row
    filter(!is.na(EndPop)) %>%
    mutate(
      AvgPop             = (StartPop + EndPop) / 2,
      PartialPersonDays  = AvgPop * IntervalLength
    )
  
  # Sums the total person-days
  total_person_days <- sum(df_intervals$PartialPersonDays)
  
  # Overall incidence rate per 100,000 person-days
  incidence_rate <- (final_events / total_person_days) * 100000
  
  # Computes a time-specific cumulative incidence for each interval row
  df_intervals <- df_intervals %>%
    mutate(
      CumulativeIncidenceRate = (CumulativeEvents / total_person_days) * 100000
    )
  
  # Builds a final DF. The last row (Day=42) will have the final IR.
  final_df <- df_intervals %>%
    select(Day, CumulativeIncidenceRate)
  
  # Adds a row for the final day with the final incidence_rate
  final_df <- bind_rows(
    final_df,
    data.frame(
      Day = df$Day[nrow(df)],
      CumulativeIncidenceRate = incidence_rate
    )
  )
  
  list(
    total_person_days = total_person_days,
    final_events      = final_events,
    incidence_rate    = incidence_rate,
    intervals_df      = df_intervals,
    final_df          = final_df
  )
}

# ----- A) For 2 doses -----
res_2d <- calc_incidence_rate(two_doses_df)

cat("\n----- 2 DOSES -----\n")
cat("Total Person-Days:", round(res_2d$total_person_days, 1), "\n")
cat("Final cumulative events:", res_2d$final_events, "\n")
cat("Incidence rate (per 100,000 person-days):", round(res_2d$incidence_rate, 1), "\n")
cat("\nTime-specific incidence data:\n")
print(res_2d$final_df)

# Writes result to CSV:
write.csv(res_2d$final_df, file = "moreira_2022_2_doses.csv", row.names = FALSE)

write.csv(three_doses_df, file = "moreira_2022_3_doses.csv", row.names = FALSE)
# ----- B) For 3 doses -----
res_3d <- calc_incidence_rate(three_doses_df)

cat("\n----- 3 DOSES -----\n")
cat("Total Person-Days:", round(res_3d$total_person_days, 1), "\n")
cat("Final cumulative events:", res_3d$final_events, "\n")
cat("Incidence rate (per 100,000 person-days):", round(res_3d$incidence_rate, 1), "\n")
cat("\nTime-specific incidence data:\n")
print(res_3d$final_df)

# Write result to CSV (optional):
write.csv(res_3d$final_df, file = "moreira_2022_3_doses.csv", row.names = FALSE)
