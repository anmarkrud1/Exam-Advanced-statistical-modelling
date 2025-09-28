#Load data
#ms_data = read.csv("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Marketshare_data.csv")
# ---- Packages ----
library(dplyr); library(stringr); library(tidyr); library(here)

# ---- Read data ----
Marketshare_data <- read.csv(
  "https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Marketshare.csv",
  check.names = FALSE
)
inhabitant_data <- read.csv(
  "https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Inhabitants%20per%20municipality.csv",
  check.names = FALSE
)

# ---- Harmonize marketshare keys ----
ms <- Marketshare_data %>%
  mutate(
    Municipality = str_trim(str_to_upper(Municipality)),
    Year = as.integer(Year)
  ) %>%
  distinct(Year, Municipality, Name, .keep_all = TRUE)

# ==== Loyalty (TVD) per Municipality-Year ====
# Prefer MarketShare (0..1); else Revenue/TotalRevenue; else SharePct/100
ms_shares <- ms %>%
  mutate(
    Share_raw = dplyr::coalesce(
      suppressWarnings(as.numeric(MarketShare)),
      ifelse(is.finite(TotalRevenue) & TotalRevenue > 0, Revenue / TotalRevenue, NA_real_),
      suppressWarnings(as.numeric(SharePct))/100
    ),
    Share = pmin(pmax(Share_raw, 0), 1)
  ) %>%
  select(Municipality, Year, Name, Share)

# Align t-1 shares to year t and FULL join so we count entries/exits
shares_tm1 <- ms_shares %>%
  transmute(Municipality, Year = Year + 1L, Name, Share_tm1 = Share)

loyalty_df <- full_join(
  ms_shares %>% rename(Share_t = Share),
  shares_tm1,
  by = c("Municipality","Year","Name")
) %>%
  mutate(
    Share_t   = dplyr::coalesce(Share_t, 0),
    Share_tm1 = dplyr::coalesce(Share_tm1, 0)
  ) %>%
  group_by(Municipality, Year) %>%
  summarise(
    TVD     = 0.5 * sum(abs(Share_t - Share_tm1), na.rm = TRUE),
    Loyalty = pmax(0, pmin(1, 1 - TVD)),
    .groups = "drop"
  )

# ---- Suppliers per muni-year ----
suppliers_yearly <- ms %>%
  group_by(Municipality, Year) %>%
  summarise(NumSuppliers = n_distinct(Name), .groups = "drop")

# ---- Revenue per muni-year ----
revenue_yearly <- ms %>%
  group_by(Municipality, Year) %>%
  summarise(TotalRevenue = max(TotalRevenue, na.rm = TRUE), .groups = "drop") %>%
  mutate(TotalRevenue = ifelse(is.finite(TotalRevenue), TotalRevenue, NA_real_))

# ---- Inhabitants: wide -> long ----
inhabitant_long <- inhabitant_data %>%
  mutate(Municipality = str_trim(str_to_upper(Municipality))) %>%
  rename_with(~ sub("^X", "", .x), matches("^X\\d{4}$")) %>%  # X2017 -> 2017
  pivot_longer(
    cols = matches("^\\d{4}$"),
    names_to = "Year",
    values_to = "Inhabitants"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Inhabitants = suppressWarnings(as.integer(Inhabitants))
  )

# ---- Complete Municipality × Year key (keeps zeros) ----
key_all <- inhabitant_long %>% distinct(Municipality, Year)

# ---- Build base and join everything ----
ms_agg <- key_all %>%
  left_join(suppliers_yearly, by = c("Municipality","Year")) %>%
  left_join(revenue_yearly,  by = c("Municipality","Year")) %>%
  left_join(inhabitant_long, by = c("Municipality","Year")) %>%
  left_join(loyalty_df,      by = c("Municipality","Year"))

# ---- Final merged_df (includes Loyalty & Loyalty_adj) ----
merged_df <- ms_agg %>%
  arrange(Municipality, Year) %>%
  group_by(Municipality) %>%
  mutate(
    NumSuppliers = dplyr::coalesce(NumSuppliers, 0L),
    TotalRevenue = ifelse(is.finite(TotalRevenue), TotalRevenue, 0),
    Active       = TotalRevenue > 0,
    Active_lag   = dplyr::lag(Active),
    Loyalty_adj  = dplyr::if_else(Active & Active_lag, Loyalty, NA_real_)
  ) %>%
  ungroup() %>%
  group_by(Municipality) %>%
  mutate(
    Delta_NumSuppliers = NumSuppliers - dplyr::lag(NumSuppliers)
  ) %>%
  ungroup() %>%
  mutate(
    Revenue_per_capita = ifelse(!is.na(TotalRevenue) & !is.na(Inhabitants) & Inhabitants > 0,
                                TotalRevenue / Inhabitants, NA_real_),
    log_NumSuppliers   = log1p(NumSuppliers)    # safe if 0
  )

# ---- Minimum 2 Loyalty_adj per municipality (analysis sample) ----
min_pairs <- 2L
keep_munis <- merged_df %>%
  group_by(Municipality) %>%
  summarise(n_Ladj = sum(!is.na(Loyalty_adj)), .groups = "drop") %>%
  filter(n_Ladj >= min_pairs) %>%
  pull(Municipality)

analysis_min2 <- merged_df %>%
  filter(Municipality %in% keep_munis, !is.na(Loyalty_adj))

# ---- Save ----
dir.create(here("Exam ASM"), recursive = TRUE, showWarnings = FALSE)
saveRDS(merged_df,    here("Exam ASM", "merged_df.rds"))
saveRDS(analysis_min2, here("Exam ASM", "analysis_min2.rds"))

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




