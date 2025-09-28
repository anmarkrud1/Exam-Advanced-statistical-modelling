library(readxl)

#df <- read_excel("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Cost_Per_Inhabitant_HealthCare.xlsx")
df <- read.csv("https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Cost_Per_Inhabitant_HealthCare.csv")

library(dplyr)
library(stringr)
library(zoo)   # for rollapply

# 1) Normalize year columns: X2017 -> 2017
year_cols <- grep("^X?\\d{4}$", names(df), value = TRUE)
df_norm <- df %>% rename_with(~ sub("^X", "", .x), all_of(year_cols))

# 2) Build CPI_Health with lag and volatility features (keep munis; NA for missing years)
CPI_Health <- df_norm %>%
  mutate(Municipality = str_trim(str_remove(Municipality, "\\s*\\(.*\\)"))) %>%
  filter(str_detect(Municipality, "^\\d{4}")) %>%
  mutate(
    MunCode = str_extract(Municipality, "^\\d{4}"),
    MunName = str_trim(str_remove(Municipality, "^\\d{4}\\s*"))
  ) %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Cost"
  ) %>%
  mutate(
    Cost = as.character(Cost),
    Cost = na_if(Cost, "."),
    Cost = str_replace_all(Cost, "[\\s\\u00A0]", ""), # remove spaces incl. non-breaking
    Cost = str_replace_all(Cost, "\\.", ""),          # remove thousand dots
    Cost = str_replace_all(Cost, ",", "."),           # comma → dot
    Cost = as.numeric(Cost),
    Year = as.integer(Year)
  ) %>%
  group_by(MunName, Year) %>%
  summarise(
    # NA-preserving sum: if all rows are NA -> NA; else sum of non-NA
    TotalCost = if (all(is.na(Cost))) NA_real_ else sum(Cost, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(MunName) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    TotalCost_lag1 = dplyr::lag(TotalCost, 1),
    HCPI_diff = TotalCost - TotalCost_lag1,
    HCPI_pct  = if_else(!is.na(TotalCost_lag1) & TotalCost_lag1 > 0,
                        (TotalCost / TotalCost_lag1) - 1,
                        NA_real_),
    HCPI_vol3 = rollapply(TotalCost, width = 3, FUN = sd,
                          align = "right", fill = NA_real_, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(MunName, Year)

CPI_Health <- CPI_Health %>%
  mutate(
    Year   = as.integer(Year),
    MunKey = str_to_lower(str_squish(str_trim(MunName))),
    HCPI_change = abs(HCPI_pct)   # take absolute value
  )

library(here)

saveRDS(CPI_Health, here("Exam ASM", "CPI_Health.rds"))

