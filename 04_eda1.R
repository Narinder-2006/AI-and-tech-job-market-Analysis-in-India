library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)

jobs <- read.csv(
  "jobs_cleaned.csv",
  stringsAsFactors = FALSE
)

jobs$posted <- ymd_hms(
  jobs$posted,
  tz = "UTC"
)

## Job Role distribution
role_counts <- jobs |>
  count(role_group, sort = TRUE)

print(role_counts)

ggplot(
  role_counts,
  aes(
    x = reorder(role_group, n),
    y = n
  )
) +
  geom_col(fill = "steelblue", width = 0.7) +
  geom_text(
    aes(label = n),
    hjust = -0.2
  ) +
  coord_flip() +
  labs(
    title = "Distribution of Technology Job Roles",
    subtitle = "Number of job postings by role",
    x = "Job Role",
    y = "Number of Postings"
  ) +
  theme_minimal()


## Top Hiring Companies
top_companies <- jobs |>
  filter(company != "Unknown") |>
  count(company, sort = TRUE) |>
  head(10)

print(top_companies)

ggplot(
  top_companies,
  aes(
    x = reorder(company, n),
    y = n
  )
) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 10 Companies by Job Postings",
    x = "Company",
    y = "Number of Postings"
  ) +
  theme_minimal()

# Geographic Location
location_counts <- jobs |>
  count(city, sort = TRUE) |>
  head(10)

print(location_counts)

ggplot(
  location_counts,
  aes(
    x = reorder(city, n),
    y = n
  )
) +
  geom_col(fill = "seagreen") +
  coord_flip() +
  labs(
    title = "Top 10 Job Locations",
    x = "Location",
    y = "Number of Postings"
  ) +
  theme_minimal()

## Salary Analysis
salary_stats <- jobs |>
  filter(!is.na(salary_midpoint)) |>
  group_by(role_group) |>
  summarise(
    job_count = n(),
    average_salary = mean(salary_midpoint),
    median_salary = median(salary_midpoint),
    min_salary = min(salary_midpoint),
    max_salary = max(salary_midpoint),
    .groups = "drop"
  ) |>
  arrange(desc(median_salary))

print(salary_stats)

ggplot(
  jobs |>
    filter(
      !is.na(salary_midpoint),
      role_group != "Other"
    ),
  aes(
    x = reorder(role_group, salary_midpoint, median),
    y = salary_midpoint
  )
) +
  geom_boxplot(fill = "skyblue", outlier.alpha = 0.4) +
  coord_flip() +
  labs(
    title = "Salary Distribution Across Technology Roles",
    x = "Job Role",
    y = "Salary Midpoint (Source Units)"
  ) +
  theme_minimal()

# Job posting trends over time
monthly_jobs <- jobs |>
  filter(!is.na(posted)) |>
  mutate(
    month = floor_date(posted, "month")
  ) |>
  count(month)

ggplot(
  monthly_jobs,
  aes(
    x = month,
    y = n
  )
) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Job Posting Trends",
    x = "Month",
    y = "Number of Postings"
  ) +
  scale_x_datetime(date_labels = "%b %Y") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

if (!dir.exists("plots")) {
  dir.create("plots")
}

ggsave(
  "plots/job_roles.png",
  width = 10,
  height = 6,
  dpi = 300
)
