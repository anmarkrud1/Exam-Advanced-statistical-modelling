library(readxl)

df <- read_excel("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Cost_Per_Inhabitant_HealthCare.xlsx")


library(dplyr)
library(stringr)
library(tidyr)

CPI_Health<- df %>%
  # Remove parentheses and content
  mutate(Municipality = str_trim(str_remove(Municipality, "\\s*\\(.*\\)"))) %>%
  
  # Keep only rows starting with 4 digits
  filter(str_detect(Municipality, "^\\d{4}")) %>%
  
  # Split code and name
  mutate(
    MunCode = str_extract(Municipality, "^\\d{4}"),
    MunName = str_trim(str_remove(Municipality, "^\\d{4}\\s*"))
  ) %>%
  
  # Pivot years into long format
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Cost"
  ) %>%
  
  # Clean numbers
  mutate(
    Cost = as.character(Cost),
    Cost = na_if(Cost, "."),
    Cost = str_replace_all(Cost, "[\\s\\u00A0]", ""), # remove spaces
    Cost = str_replace_all(Cost, "\\.", ""),          # remove thousand dots
    Cost = str_replace_all(Cost, ",", "."),           # comma → dot
    Cost = as.numeric(Cost)
  ) %>%
  
  # Group only by municipality name + year
  group_by(MunName, Year) %>%
  summarise(TotalCost = sum(Cost, na.rm = TRUE), .groups = "drop") %>%
  
  # ---- NEW STEP: remove municipalities with any 0 across years ----
group_by(MunName) %>%
  filter(all(TotalCost != 0)) %>%
  ungroup() %>%
  
  arrange(MunName, Year)
