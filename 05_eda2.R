library(dplyr); library(ggplot2); library(tidyr); library(purrr)

dir.create("plots", showWarnings = FALSE)

jobs <- read.csv("jobs_features.csv", stringsAsFactors = FALSE) |>
  filter(relevant_title) |>
  mutate(
    salary_lpa = ifelse(!is.na(salary_midpoint) & salary_midpoint >= 1.2e5 & salary_midpoint <= 2e7,
                        salary_midpoint / 1e5, NA),
    experience_band = factor(experience_band, levels = c("<1","1-2","3-5","6-10",">10"))
  )
d <- jobs |> filter(!is.na(salary_lpa))

# Figure 5.4: Salary by city
pD2 <- jobs |> filter(!is.na(salary_lpa), city_group != "Not specified") |>
  group_by(city_group) |> filter(n() >= 15) |>
  ggplot(aes(reorder(city_group, salary_lpa, median), salary_lpa)) +
  geom_boxplot(fill = "seagreen") +
  coord_flip() +
  labs(title = "Salary by city (cities with 15+ salaried postings)", x = NULL, y = "LPA") +
  theme_minimal()
ggsave("plots/salary_by_city.png", pD2, width = 10, height = 6, dpi = 300)

# Figure 5.5: Salary by required experience
pD1 <- jobs |> filter(!is.na(salary_lpa), !is.na(experience_band)) |>
  ggplot(aes(experience_band, salary_lpa)) + geom_boxplot(fill = "skyblue") +
  labs(title = "Salary by required experience", x = "Years", y = "LPA") + theme_minimal()
ggsave("plots/salary_by_experience.png", pD1, width = 10, height = 6, dpi = 300)

# Supporting statistics
cor.test(jobs$exp_mid, jobs$salary_lpa, method = "spearman", use = "complete.obs")
cor.test(jobs$skill_count, jobs$salary_lpa, method = "spearman", use = "complete.obs")