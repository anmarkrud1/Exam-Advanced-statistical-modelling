#Load data
#ms_data = read.csv("C:/Users/Anmar/OneDrive/Skrivebord/Exam ASM/Marketshare_data.csv")

Marketshare_data = read.csv("https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Marketshare.csv")
inhabitant_data = read.csv("https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Inhabitants%20per%20municipality.csv")
# ---- Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# =========================
# 1) Shares + Loyalty
# =========================
# (Keeps your NumSuppliers part in case you need it later)
supplier_counts_yearly <- Marketshare_data %>%
  distinct(Year, Municipality, Name) %>%
  count(Year, Municipality, name = "NumSuppliers")

Marketshare_data <- Marketshare_data %>%
  distinct(Year, Municipality, Name, .keep_all = TRUE) %>%
  add_count(Year, Municipality, name = "NumSuppliers") %>%
  arrange(Year, Municipality, desc(MarketShare))

# Build shares per Municipality–Year–Supplier and complete missing suppliers as 0
ms_complete <- Marketshare_data %>%
  mutate(
    Share_raw = dplyr::coalesce(MarketShare, Revenue / TotalRevenue),
    Share = ifelse(Share_raw > 1, Share_raw/100, Share_raw)
  ) %>%
  select(Municipality, Year, Name, Share) %>%
  group_by(Municipality, Year) %>%
  mutate(Share = Share / sum(Share, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Municipality, Year, Name, fill = list(Share = 0)) %>%
  group_by(Municipality, Year) %>%
  mutate(Share = Share / sum(Share, na.rm = TRUE)) %>%
  ungroup()

# TVD and Loyalty per municipality-year
loyalty_df <- ms_complete %>%
  group_by(Municipality, Name) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(Share_lag = dplyr::lag(Share, default = 0)) %>%
  ungroup() %>%
  group_by(Municipality, Year) %>%
  summarise(
    TVD     = 0.5 * sum(abs(Share - Share_lag), na.rm = TRUE),
    Loyalty = (1 - TVD) * 100,
    .groups = "drop"
  ) %>%
  group_by(Municipality) %>%
  mutate(Loyalty = ifelse(Year == min(Year), NA_real_, Loyalty)) %>%
  ungroup()

# =========================
# 2) Clean inhabitants (wide -> long -> average per muni)
# =========================
inhabitant_long <- inhabitant_data %>%
  rename_with(~ sub("^X", "", .x), starts_with("X")) %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "Year",
    values_to = "Inhabitants"
  ) %>%
  mutate(
    Year = as.integer(Year),
    Inhabitants = as.numeric(gsub("[^0-9\\-\\.]", "", as.character(Inhabitants)))
  )

inhabitant_avg <- inhabitant_long %>%
  group_by(Municipality) %>%
  summarise(avg_inhabitants = mean(Inhabitants, na.rm = TRUE), .groups = "drop")

# =========================
# 3) Average loyalty per muni & normalize join keys
# =========================
avg_loyalty <- loyalty_df %>%
  group_by(Municipality) %>%
  summarise(avg_loyalty = mean(Loyalty, na.rm = TRUE), .groups = "drop")

norm_key <- function(x){
  x %>%
    str_to_lower() %>%
    str_replace_all("\\s*\\([^)]*\\)", "") %>%     # drop text in parentheses e.g. "Våler (Østfold)"
    str_replace_all("[\\u00A0\\u2000-\\u200B\\u202F\\u205F\\u3000]", " ") %>%
    str_replace_all("[^a-zæøå\\- ]", " ") %>%
    str_squish()
}

avg_loyalty_key    <- avg_loyalty    %>% mutate(MuniKey = norm_key(Municipality))
inhabitant_avg_key <- inhabitant_avg %>% mutate(MuniKey = norm_key(Municipality))

# =========================
# 4) Join, bucket into tertiles, plot
# =========================
plot_data <- avg_loyalty_key %>%
  inner_join(inhabitant_avg_key %>% select(MuniKey, avg_inhabitants), by = "MuniKey") %>%
  filter(is.finite(avg_loyalty), is.finite(avg_inhabitants)) %>%
  mutate(size_group = factor(dplyr::ntile(avg_inhabitants, 3),
                             levels = 1:3, labels = c("Small","Medium","Large")))

ggplot(plot_data, aes(x = size_group, y = avg_loyalty)) +
  geom_boxplot() +
  labs(
    x = "Municipality size (tertiles by avg inhabitants)",
    y = "Average loyalty score",
    title = "Average loyalty vs municipality size"
  ) +
  theme_minimal()


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




