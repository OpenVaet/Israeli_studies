# install.packages("dplyr")
library(dplyr)

# Creates a data frame with the data
df <- data.frame(
  Day = c(0, 7, 14, 21, 28, 35, 42),
  PersonsAtRisk = c(596518, 413052, 261625, 186553, 107209, 37164, 4132),
  CumulativeEvents = c(0, 2362, 3971, 5140, 5775, 6053, 6100)
)
print(df)

# Checks the final (cumulative) number of events
final_events <- df$CumulativeEvents[nrow(df)]

# Prepares an 'intervals' data frame:
#    - For each row i, defines the end day/Population as row i+1
#    - Computes interval length (7 days)
#    - Computes average population = (startPop + endPop) / 2
#    - Computes partial person-days = avgPop * interval length
df_intervals <- df %>%
  mutate(
    # The next row's day (end of interval)
    EndDay = lead(Day),
    StartPop = PersonsAtRisk,
    EndPop   = lead(PersonsAtRisk),
    IntervalLength = EndDay - Day
  ) %>%
  # Drops the last row because it doesn't have a "next row"
  filter(!is.na(EndPop)) %>%
  mutate(
    AvgPop = (StartPop + EndPop) / 2,
    PartialPersonDays = AvgPop * IntervalLength
  )

# Sums the person-days across all intervals
total_person_days <- sum(df_intervals$PartialPersonDays)
cat("Total Person-Days:", round(total_person_days, 1), "\n")

# Calculates the cumulative incidence to a given day.
df_intervals <- df_intervals %>%
  mutate(
    CumulativeIncidenceRate = (CumulativeEvents / total_person_days) * 100000
  )
df_intervals

# Calculates the incidence rate (events per 100,000 person-days)
incidence_rate <- (final_events / total_person_days) * 100000

# Prints results
cat("Final cumulative events:", final_events, "\n")
cat("Incidence rate (per 100,000 person-days):", round(incidence_rate, 1), "\n")

# Creates the final_df: fetch Day, CumulativeIncidenceRate from df_intervals
final_df <- df_intervals %>%
  select(Day, CumulativeIncidenceRate)

# Adds a row for Day=42 with the final incidence_rate
final_df <- bind_rows(
  final_df,
  data.frame(Day = 42, CumulativeIncidenceRate = incidence_rate)
)

# Inspects final_df
print(final_df)

# Writes to CSV
write.csv(final_df, file = "dagan_2021.csv", row.names = FALSE)
