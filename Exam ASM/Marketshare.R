#Load data
ms_data = read.csv("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Marketshare_data.csv")

names(ms_data) <- c(
  "OrgForm",              # Stat_helsemarked_organisasjonsform
  "Name",                 # Stat_helsemarked_Navn
  "LargerCompanies",      # Stat_helsemarked_Større_selskaper
  "Year",                 # Stat_helsemarked_År
  "OriginalMunicipalityID", # Stat_helsemarked_Original_kommunenr
  "NewMunicipalityID",    # Stat_helsemarked_Nytt_Kommunenr
  "Municipality",         # Stat_helsemarked_Kommune
  "CountyNumber",         # Stat_helsemarked_Fylkenummer
  "County",               # Stat_helsemarked_Fylke
  "Revenue",              # Stat_helsemarked_omsetning
  "NumInvoices",          # Stat_helsemarked_ant_fakturaer
  "Function",             # Stat_helsemarked_Funksjon
  "Account",              # Stat_helsemarked_Art
  "NCode1",               # Stat_helsemarked_nkode1
  "RegiFriv",             # Stat_helsemarked_regifriv
  "RegiVAT",              # Stat_helsemarked_regimva
  "MainArea",             # Stat_helsemarked_hovedområde
  "IndustryText",         # Stat_helsemarked_naerk_tekst
  "FunctionArea",         # Stat_helsemarked_funksjon_område
  "AccountName",          # Stat_helsemarked_art_navn
  "OriginalMunicipality", # Stat_helsemarked_kommunenavn_Original
  "Group",                # Stat_helsemarked_Konsern
  "OrgNumber"             # Stat_helsemarked_Orgnr
)


ms_data_staffing <- ms_data[ms_data$MainArea == "Bemanning", ]


library(dplyr)
library(stringr)
library(readr)
library(tidyr)

top4_suppliers = c(
  "ECURA CARE AS",
  "DEDICARE AS",
  "KONSTALI HELSENOR AS",
  "FOCUS CARE NORGE AS"
)

normalize_revenue = function(x) {
  x %>%
    str_squish() %>%
    # Normalize weird spaces (NBSP, thin space, etc.) to normal space
    str_replace_all("[\\u00A0\\u2000-\\u200B\\u202F\\u205F\\u3000]", " ") %>%
    # Keep only digits, signs, separators
    str_replace_all("[^0-9,\\.\\- ]", "") %>%
    str_squish()
}

to_numeric_revenue = function(x) {
  # Heuristic:
  # 1) If comma is the decimal mark (ends with ,dd): drop dots (thousands), make comma -> dot
  # 2) Else if dot is the decimal mark (ends with .dd): drop commas (thousands)
  # 3) Else: treat as integer; drop spaces/commas/dots
  case_when(
    str_detect(x, ",\\d{1,2}$") ~ suppressWarnings(as.numeric(str_replace_all(str_replace_all(x, "\\.", ""), ",", "."))),
    str_detect(x, "\\.\\d{1,2}$") ~ suppressWarnings(as.numeric(str_replace_all(x, ",", ""))),
    TRUE                          ~ suppressWarnings(as.numeric(str_replace_all(x, "[\\s,\\.]", "")))
  )
}

ms_rev_all =
  ms_data_staffing %>%
  mutate(
    Group        = replace_na(Group, ""),
    Revenue_str  = normalize_revenue(Revenue),
    Revenue_num  = to_numeric_revenue(Revenue_str)
  ) %>%
  group_by(Name, Year, Municipality, FunctionArea, County) %>%
  summarise(Revenue = sum(Revenue_num, na.rm = TRUE), .groups = "drop")

# 2) Supplier revenue by Year × Municipality (still ALL suppliers)
supplier_rev =
  ms_rev_all %>%
  group_by(Name, Year, Municipality) %>%
  summarise(Revenue = sum(Revenue, na.rm = TRUE), .groups = "drop")

# 3) TOTAL revenue by Year × Municipality (ALL suppliers)
total_rev =
  supplier_rev %>%
  group_by(Year, Municipality) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = "drop")

# 4) Market share = supplier / total (denominator is ALL suppliers)
marketshare =
  supplier_rev %>%
  left_join(total_rev, by = c("Year", "Municipality")) %>%
  mutate(MarketShare = round(Revenue / TotalRevenue, 4)) %>%
  arrange(Year, Municipality, desc(MarketShare))

# 5) Overall ranking by total revenue across all years/municipalities
supplier_ranking =
  supplier_rev %>%
  group_by(Name) %>%
  summarise(TotalRevenue = sum(Revenue, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(TotalRevenue)) %>%
  mutate(Rank = row_number())

# ---- Optional: view top-4 using ALL-supplier denominator ----
marketshare_top4 =
  marketshare %>% filter(Name %in% top4_suppliers)



