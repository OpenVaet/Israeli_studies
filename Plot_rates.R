library(ggplot2)

# Creates a data frame with your data
df <- data.frame(
  StudyVaxGroup = c(
    "Dagan 2021\nUnvaccinated",
    "Bar-on 2021\nNon Booster (2 doses)",
    "Bar-On 2022\n3 doses",
    "Magen 2022\n3 doses (Control)"
  ),
  IncidenceRate = c(66.7, 85.46, 361, 348)
)

# Turns StudyVaxGroup into a factor with the specified order
df$StudyVaxGroup <- factor(
  df$StudyVaxGroup,
  levels = c(
    "Dagan 2021\nUnvaccinated",
    "Bar-on 2021\nNon Booster (2 doses)",
    "Bar-On 2022\n3 doses",
    "Magen 2022\n3 doses (Control)"
  )
)

# Makes the plot
p <- ggplot(df, aes(x = StudyVaxGroup, y = IncidenceRate)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = IncidenceRate),
            vjust = -0.5,       # Position the text above the bars
            size = 6) +         # Font size for labels
  theme_minimal(base_size = 18) + 
  labs(
    x = "Study & Vaccination Group",
    y = "Confirmed Infection Rate\n(per 100,000 person-days)",
    title = "Infection Rates per 100,000 person-days, by Study and Vaccination Group"
  ) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 22),
    axis.text.x = element_text(angle = 30, hjust = 1, size = 18),  # Slant x-axis labels if needed
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18)
  )

# Prints the plot
print(p)
