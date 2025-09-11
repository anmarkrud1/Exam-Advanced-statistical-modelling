library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# Load the Excel file
df <- read_excel("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Municipality_ operating_results.xlsx")

df_clean <- df %>%
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
    Value = as.character(Value),
    Value = na_if(Value, "."),
    # remove spaces/non-breaking spaces
    Value = str_replace_all(Value, "[\\s\\u00A0]", ""),
    # if numbers use comma as decimal -> replace
    Value = str_replace(Value, ",", "."),
    # keep only digits and one decimal dot
    Value = str_replace_all(Value, "[^0-9\\.]", ""),
    Value = as.numeric(Value)
  ) %>%
  group_by(MunName, Year) %>%
  summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(TotalValue = round(TotalValue, 2)) %>%
  group_by(MunName) %>%
  filter(all(TotalValue != 0)) %>%
  ungroup() %>%
  arrange(MunName, Year)
