library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# Load the Excel file
#df <- read_excel("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Municipality_ operating_results.xlsx")
df <- read.csv("https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Municipality_%20operating_results.csv")


# 0) Normalize header names so years are plain "2017","2018",...
names(df) <- trimws(names(df))  # strip any stray spaces
year_cols <- grep("^X?\\d{4}$", names(df), value = TRUE)
df_norm <- df %>%
  rename_with(~ sub("^X", "", .x), all_of(year_cols))

df_clean <- df_norm %>%
  mutate(Municipality = str_trim(str_remove(Municipality, "\\s*\\(.*\\)"))) %>%
  filter(str_detect(Municipality, "^\\d{4}")) %>%
  mutate(
    MunCode = str_extract(Municipality, "^\\d{4}"),
    MunName = str_trim(str_remove(Municipality, "^\\d{4}\\s*"))
  ) %>%
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    # Work on the raw string
    Value_chr = as.character(Value),
    Value_chr = str_replace_all(Value_chr, "\u2212", "-"),
    is_paren_neg = str_detect(Value_chr, "^\\(.*\\)$"),
    Value_chr = str_replace_all(Value_chr, "[()]", ""),
    Value_chr = str_replace_all(Value_chr, "[\\s\\u00A0]", ""),
    Value_chr = str_replace_all(Value_chr, "\\.", ""),
    Value_chr = str_replace_all(Value_chr, ",", "."),
    Value_chr = str_replace_all(Value_chr, "[^0-9\\.-]", ""),
    Value_chr = if_else(is_paren_neg & !str_detect(Value_chr, "^-"),
                        paste0("-", Value_chr), Value_chr),
    Value = suppressWarnings(as.numeric(Value_chr)),
    
    # ---- Fix rounding: set 0.0 to 1, keep NA as NA ----
    Value = if_else(!is.na(Value) & Value == 0, 1, Value)
  ) %>%
  group_by(MunName, Year) %>%
  summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(TotalValue = round(TotalValue, 2)) %>%
  arrange(MunName, Year)


# Rename
MunOperatingResults <- df_clean



library(dplyr)

# Ensure Year is character everywhere (or integer everywhere) before joins
MunOperatingResults <- MunOperatingResults %>%
  mutate(
    Year = as.character(Year),
    Surplus = if_else(TotalValue > 0, 1L, 0L),
    Surplus_ge0 = if_else(TotalValue >= 0, 1L, 0L)  # optional robustness
  ) %>%
  arrange(MunName, Year) %>%
  group_by(MunName) %>%
  mutate(
    Surplus_lag1 = dplyr::lag(Surplus, 1)          # main spec: lagged surplus
  ) %>%
  ungroup()

# Save as RDS (recommended for reuse in R)
saveRDS(MunOperatingResults, here("Exam ASM", "MunOperatingResults.rds"))

