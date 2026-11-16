library(dplyr); library(stringr); library(lubridate)

jobs <- read.csv("jobs_cleaned.csv", stringsAsFactors = FALSE)

# ---------- 1. Dates ----------
jobs <- jobs |>
  mutate(posted_date  = as_date(ymd_hms(posted)),
         posted_month = format(posted_date, "%Y-%m"))

# ---------- 2. Cities grouped like the Quess report ----------
jobs <- jobs |>
  mutate(
    city_group = case_when(
      city %in% c("Bangalore")                                  ~ "Bengaluru",
      city %in% c("Delhi", "Noida", "Gurugram") |
        str_detect(location, regex("ghaziabad|faridabad", TRUE)) ~ "Delhi NCR",
      city == "Mumbai" | str_detect(location, regex("thane", TRUE)) ~ "Mumbai",
      str_detect(location, regex("kolkata", TRUE))              ~ "Kolkata",
      str_detect(location, regex("kochi|cochin|ernakulam", TRUE)) ~ "Kochi",
      str_detect(location, regex("coimbatore", TRUE))           ~ "Coimbatore",
      str_detect(location, regex("indore", TRUE))               ~ "Indore",
      str_detect(location, regex("jaipur", TRUE))               ~ "Jaipur",
      city == "Unspecified India"                               ~ "Not specified",
      TRUE                                                      ~ city
    ),
    city_tier = case_when(
      city_group %in% c("Bengaluru","Hyderabad","Delhi NCR","Mumbai","Pune","Chennai") ~ "Tier-1",
      city_group %in% c("Ahmedabad","Kolkata","Kochi","Coimbatore","Indore","Jaipur")  ~ "Tier-2",
      city_group == "Not specified"                                                    ~ "Unknown",
      TRUE                                                                             ~ "Other"
    )
  )

# ---------- 3. Relevance flag (does not delete rows) ----------
title_l <- str_to_lower(jobs$title)
jobs$relevant_title <- str_detect(title_l,
                                  "\\b(ai|ml|nlp|llm|llms|genai|mlops|machine learning|deep learning|data|analytics|scientist|generative|computer vision|artificial intelligence|agentic|rag|bi)\\b") &
  !str_detect(title_l, "data entry|sales|business development|recruit|telecall|bpo")

# ---------- 4. Years of experience (Quess bands) ----------
txt_l <- str_to_lower(paste(jobs$title, jobs$description))
m <- str_match(txt_l, "\\b(\\d{1,2})\\s*(\\+|-|–|to)?\\s*(\\d{1,2})?\\s*\\+?\\s*(?:years?|yrs?)\\b")
exp_min <- as.numeric(m[, 2])
exp_max <- ifelse(!is.na(as.numeric(m[, 4])) & m[, 3] %in% c("-", "–", "to"), as.numeric(m[, 4]), NA_real_)
ok <- str_detect(txt_l, "experience|\\bexp\\b") & !is.na(exp_min) & exp_min <= 30 &
  (is.na(exp_max) | exp_max >= exp_min)

exp_min[!ok] <- NA; exp_max[!ok] <- NA
exp_min <- ifelse(is.na(exp_min) & str_detect(txt_l, "\\bfreshers?\\b|entry[- ]level"), 0, exp_min)

jobs$exp_min <- exp_min
jobs$exp_max <- exp_max
jobs$exp_mid <- ifelse(!is.na(exp_max), (exp_min + exp_max) / 2, exp_min)
jobs$experience_band <- cut(jobs$exp_mid, breaks = c(-Inf, 1, 3, 6, 11, Inf),
                            labels = c("<1", "1-2", "3-5", "6-10", ">10"), right = FALSE)

# ---------- 5. Skill flags ----------
txt <- paste(jobs$title, jobs$description)
skills <- c(
  python = "\\bpython\\b", sql = "\\bsql\\b|\\bmysql\\b|\\bpostgres(ql)?\\b",
  tensorflow = "\\btensorflow\\b", pytorch = "\\bpytorch\\b", sklearn = "scikit[- ]?learn|\\bsklearn\\b",
  pandas = "\\bpandas\\b", xgboost = "\\bxgboost\\b", spark = "\\bpyspark\\b|\\bspark\\b",
  aws = "\\baws\\b|amazon web services", azure = "\\bazure\\b", gcp = "\\bgcp\\b|google cloud",
  docker = "\\bdocker\\b", kubernetes = "\\bkubernetes\\b|\\bk8s\\b", databricks = "\\bdatabricks\\b",
  tableau = "\\btableau\\b", powerbi = "power ?bi", excel = "\\bexcel\\b",
  llm = "\\bllms?\\b|large language model", genai = "gen ?ai|generative ai",
  rag = "\\brag\\b|retrieval[- ]augmented", langchain = "\\blangchain\\b", langgraph = "\\blanggraph\\b",
  mcp = "\\bmcp\\b|model context protocol", agentic = "agentic|ai agents?",
  vector_db = "vector (db|database|store)|pinecone|faiss|chroma|weaviate",
  mlops = "\\bmlops\\b|mlflow|kubeflow", nlp = "\\bnlp\\b|natural language processing",
  computer_vision = "computer vision|\\bopencv\\b", deep_learning = "deep learning|neural network"
)
for (s in names(skills)) {
  jobs[[paste0("skill_", s)]] <- str_detect(txt, regex(skills[[s]], ignore_case = TRUE))
}
# R language (approximate: a bare "R" is easy to mismatch)
jobs$skill_r <- str_detect(txt, "\\bR\\b(,|/| programming| language| and | & )|(Python|SQL)(,|/| and | & )R\\b")

skill_cols <- grep("^skill_", names(jobs), value = TRUE)
jobs$skill_count <- rowSums(jobs[, skill_cols])

# ---------- 6. Quess talent-gap themes ----------
themes <- c(
  genai = "gen ?ai|generative ai|\\bllms?\\b|large language model",
  agentic = "agentic|ai agents?|multi[- ]agent",
  mlops = "\\bmlops\\b|\\bllmops\\b|mlflow|kubeflow|model deployment|model serving",
  governance = "ai governance|responsible ai|model risk|explainab|ai ethics|model validation",
  ai_security = "ai security|model security|adversarial|red[- ]team",
  nlp = "\\bnlp\\b|natural language processing",
  fde = "forward[- ]deployed"
)
for (t in names(themes)) {
  jobs[[paste0("theme_", t)]] <- str_detect(txt, regex(themes[[t]], ignore_case = TRUE))
}

# ---------- 7. Save ----------
write.csv(jobs, "jobs_features.csv", row.names = FALSE)