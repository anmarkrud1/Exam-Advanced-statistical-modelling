#Load data
#ms_data = read.csv("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Marketshare_data.csv")

Marketshare_data = read.csv("https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Marketshare.csv")

# Count suppliers per Year × Municipality using ONLY `marketshare`
supplier_counts_yearly <-
  Marketshare_data %>%
  distinct(Year, Municipality, Name) %>%
  count(Year, Municipality, name = "NumSuppliers")


# Add NumSuppliers as a column to `marketshare`
Marketshare_data <-
  Marketshare_data %>%
  distinct(Year, Municipality, Name, .keep_all = TRUE) %>%  # safety against duplicates
  add_count(Year, Municipality, name = "NumSuppliers") %>%
  arrange(Year, Municipality, desc(MarketShare))

library(dplyr)
library(tidyr)

# --- 1) Choose/share column & normalize to sum=1 each muni-year ---
ms_share <-
  Marketshare_data %>%
  mutate(
    # Prefer MarketShare if present; fall back to Revenue / TotalRevenue
    Share_raw = dplyr::coalesce(MarketShare, Revenue / TotalRevenue),
    # If someone stored % (0–100), convert to 0–1
    Share = ifelse(Share_raw > 1, Share_raw / 100, Share_raw)
  ) %>%
  select(Municipality, Year, Name, Share) %>%
  group_by(Municipality, Year) %>%
  mutate(Share = Share / sum(Share, na.rm = TRUE)) %>%  # ensure sums to 1
  ungroup()

# --- 2) Complete supplier universe across consecutive years (missing -> 0) ---
ms_complete <-
  ms_share %>%
  complete(Municipality, Year, Name, fill = list(Share = 0)) %>%
  group_by(Municipality, Year) %>%
  mutate(Share = Share / sum(Share, na.rm = TRUE)) %>%  # re-normalize after completing
  ungroup()

# --- 3) TVD and Loyalty per municipality-year ---
loyalty_df <-
  ms_complete %>%
  group_by(Municipality, Name) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(Share_lag = dplyr::lag(Share, default = 0)) %>%
  ungroup() %>%
  group_by(Municipality, Year) %>%
  summarise(
    TVD     = 0.5 * sum(abs(Share - Share_lag), na.rm = TRUE),
    Loyalty = (1 - TVD) * 100,                   # 0–100 scale
    .groups = "drop"
  ) %>%
  arrange(Municipality, Year)

# --- Optional: keep first year as NA (no previous year to compare) ---
loyalty_df <-
  loyalty_df %>%
  group_by(Municipality) %>%
  mutate(Loyalty = ifelse(Year == min(Year), NA_real_, Loyalty)) %>%
  ungroup()

# View / save
print(head(loyalty_df, 12))
# write.csv(loyalty_df, "loyalty_scores_by_municipality_year.csv", row.names = FALSE)


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




