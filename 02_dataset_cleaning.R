library(dplyr)
library(stringr)
library(lubridate)
library(janitor)

jobs <- read.csv(
  "/home/a_a_tinysher/Desktop/jobs_combined_raw.csv",
  stringsAsFactors = FALSE
) |>
  clean_names()

dim(jobs)
colSums(is.na(jobs))


jobs_clean <- jobs |>
  mutate(
    title = str_squish(title),
    company = str_squish(company),
    location = str_squish(location),
    description = str_squish(
      str_replace_all(
        description,
        "<[^>]+>",
        " "
      )
    ),
    company = if_else(
      is.na(company) | company == "",
      "Unknown",
      company
    )
  )

# Check duplicates
sum(duplicated(jobs_clean$job_id))


# Handling Missing Salary
jobs_clean <- jobs_clean |>
  mutate(
    salary_min = as.numeric(salary_min),
    salary_max = as.numeric(salary_max),
    
    salary_midpoint = if_else(
      !is.na(salary_min) &
        !is.na(salary_max) &
        salary_min > 0 &
        salary_max >= salary_min,
      (salary_min + salary_max) / 2,
      NA_real_
    ),
    
    salary_available = !is.na(salary_midpoint)
  )

summary(jobs_clean$salary_midpoint)

table(jobs_clean$salary_available)


#standardise job roles
jobs_clean <- jobs_clean |>
  mutate(
    role_group = case_when(
      str_detect(
        title,
        regex("data engineer", ignore_case = TRUE)
      ) ~ "Data Engineer",
      
      str_detect(
        title,
        regex("data scientist|data science", ignore_case = TRUE)
      ) ~ "Data Scientist",
      
      str_detect(
        title,
        regex("data analyst|business analyst", ignore_case = TRUE)
      ) ~ "Data Analyst",
      
      str_detect(
        title,
        regex("mlops|machine learning operations", ignore_case = TRUE)
      ) ~ "MLOps Engineer",
      
      str_detect(
        title,
        regex("machine learning|ml engineer", ignore_case = TRUE)
      ) ~ "ML Engineer",
      
      str_detect(
        title,
        regex("ai engineer|artificial intelligence|generative ai|genai|llm engineer",
              ignore_case = TRUE)
      ) ~ "AI Engineer",
      
      TRUE ~ "Other"
    )
  )

table(jobs_clean$role_group)

# standardise locations
jobs_clean <- jobs_clean |>
  mutate(
    city = case_when(
      str_detect(location, regex("bangalore|bengaluru", TRUE)) ~ "Bangalore",
      str_detect(location, regex("hyderabad", TRUE)) ~ "Hyderabad",
      str_detect(location, regex("pune", TRUE)) ~ "Pune",
      str_detect(location, regex("chennai", TRUE)) ~ "Chennai",
      str_detect(location, regex("mumbai", TRUE)) ~ "Mumbai",
      str_detect(location, regex("noida", TRUE)) ~ "Noida",
      str_detect(location, regex("gurgaon|gurugram", TRUE)) ~ "Gurugram",
      str_detect(location, regex("delhi", TRUE)) ~ "Delhi",
      str_detect(location, regex("ahmedabad", TRUE)) ~ "Ahmedabad",
      str_to_lower(location) == "india" ~ "Unspecified India",
      TRUE ~ "Other"
    )
  )

table(jobs_clean$city)

#cleaned dataset
write.csv(
  jobs_clean,
  "jobs_cleaned.csv",
  row.names = FALSE,
  na = ""
)

cat("Cleaning complete!\n")
cat("Rows:", nrow(jobs_clean), "\n")
cat("Columns:", ncol(jobs_clean), "\n")


# verification
# Check missing values
colSums(is.na(jobs_clean))

# Check duplicate IDs
sum(duplicated(jobs_clean$job_id))

# Check job-role distribution
jobs_clean |>
  count(role_group, sort = TRUE)

# Check salary availability
jobs_clean |>
  count(salary_available)

# Check salary ranges
summary(jobs_clean$salary_midpoint)

# Check date conversion
sum(is.na(jobs_clean$posted))





















