# ============================================================
# AI & Tech Jobs in India: Data Extraction (Adzuna API)
# ============================================================

library(httr); library(jsonlite); library(dplyr); library(purrr); library(tibble)

APP_ID  <- "564d3141"
APP_KEY <- "
8bce5fab0f5f5f96bfddf43775124dff"

dir.create("data/raw/cache", recursive = TRUE, showWarnings = FALSE)

# Remove the old cache made with date sorting (it had irrelevant jobs)
old <- list.files("data/raw/cache", full.names = TRUE)
if (length(old) > 0) file.remove(old)

# ---------- Function: fetch one page (cached, so no repeat calls) ----------
get_adzuna <- function(query, page) {
  f <- file.path("data/raw/cache", paste0(gsub(" ", "_", query), "_p", page, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  
  res <- GET(
    paste0("https://api.adzuna.com/v1/api/jobs/in/search/", page),
    query = list(app_id = APP_ID, app_key = APP_KEY,
                 what = query, results_per_page = 50)
  )
  
  if (status_code(res) != 200) {
    message("Failed: ", query, " page ", page, " | status ", status_code(res))
    return(NULL)
  }
  
  out <- fromJSON(content(res, "text", encoding = "UTF-8"), flatten = TRUE)$results
  if (length(out) == 0) return(NULL)
  
  df <- as_tibble(out) %>%
    transmute(
      job_id      = as.character(id),
      title,
      description,
      company     = company.display_name,
      location    = location.display_name,
      category    = category.label,
      salary_min,
      salary_max,
      posted      = created,
      url         = redirect_url,
      search_term = query
    )
  
  saveRDS(df, f)
  Sys.sleep(1)
  df
}

# ---------- Extraction: 4 roles x 5 pages = 20 calls ----------
roles <- c("machine learning", "data scientist", "generative AI", "data analyst")

jobs <- expand.grid(q = roles, p = 1:5, stringsAsFactors = FALSE) %>%
  pmap_dfr(function(q, p) get_adzuna(q, p))

# ---------- Save raw data ----------
write.csv(jobs, "data/raw/adzuna_india_raw.csv", row.names = FALSE)

# ---------- Extraction summary (for your lab report) ----------
cat("\n===== EXTRACTION SUMMARY =====\n")
cat("Extraction time :", format(Sys.time()), "\n")
cat("Total rows      :", nrow(jobs), "\n")
cat("Unique job IDs  :", n_distinct(jobs$job_id), "\n")
cat("Salary available:", round(100 * mean(!is.na(jobs$salary_min)), 1), "%\n\n")

cat("Rows per search term:\n");  print(table(jobs$search_term))
cat("\nMissing values per column:\n"); print(colSums(is.na(jobs)))
cat("\nTop 10 locations:\n"); print(head(sort(table(jobs$location), decreasing = TRUE), 10))
cat("\nTop categories:\n");   print(head(sort(table(jobs$category), decreasing = TRUE), 5))

# ---------- Relevance check ----------
jobs %>% select(title, company, location) %>% head(15) %>% print()
file.copy("data/raw/adzuna_india_raw.csv", "data/raw/adzuna_india_raw_BACKUP.csv")



library(dplyr); library(stringr); library(ggplot2)

jobs <- read.csv("data/raw/adzuna_india_raw.csv", stringsAsFactors = FALSE)

# 1. Look at the data
View(jobs)                        # opens a spreadsheet view
glimpse(jobs)
summary(jobs[, c("salary_min", "salary_max")])

# 2. How many unique, usable jobs?
jobs_u <- jobs %>% distinct(job_id, .keep_all = TRUE)
nrow(jobs_u)

# 3. Duplicates by content (same job reposted with different IDs)
jobs_u %>% count(title, company, location) %>% filter(n > 1) %>% arrange(desc(n)) %>% head(10)
nrow(distinct(jobs_u, title, company, location))

# 4. Relevance: how many titles mention AI/data/ML terms?
pattern <- "machine learning|ml|data|ai|artificial|nlp|deep learning|analytics|llm|generative|scientist"
jobs_u <- jobs_u %>% mutate(relevant = str_detect(tolower(title), pattern))
table(jobs_u$relevant)

# 5. Description length (are they full postings or snippets?)
summary(nchar(jobs_u$description))

# 6. Do skills actually appear in the text?
skills <- c("python", "sql", "r", "tensorflow", "pytorch", "aws", "llm", "excel", "tableau", "power bi", "spark")
sapply(skills, function(s) sum(str_detect(tolower(jobs_u$description), paste0("\\b", s, "\\b"))))

# 7. Experience mentions
sum(str_detect(tolower(jobs_u$description), "[0-9]+\\+?\\s*(years|yrs)"))

# 8. Quick plots
ggplot(jobs_u, aes(x = search_term)) + geom_bar(fill = "steelblue")
ggplot(jobs_u %>% filter(!is.na(salary_min)), aes(salary_min)) + geom_histogram(bins = 30)


getwd()