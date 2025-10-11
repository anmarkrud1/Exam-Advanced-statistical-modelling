library(dplyr)
library(tidyr)
library(readr)
library(stringr)

Ele_2019 = read_csv2(
  "https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Election_Results_2019%20.csv",
  locale = locale(encoding = "UTF-8"),
  name_repair = "minimal"
)

Ele_2015 = read_csv2(
  "https://raw.githubusercontent.com/anmarkrud1/Exam-Advanced-statistical-modelling/refs/heads/main/Exam%20ASM/Election_Results_2015.csv",
  locale = locale(encoding = "UTF-8"),
  name_repair = "minimal"
)

Left_parties  <- c("A", "SV", "RØDT", "MDG", "SP") 
Right_parties <- c("H", "FrP", "V", "KrF")



# --- 2019 ---
Ele_2019_clean <- Ele_2019 %>%
  filter(`Antall mandater` > 0) %>%
  mutate(Bloc = case_when(
    Partikode %in% Left_parties  ~ "Left",
    Partikode %in% Right_parties ~ "Right",
    TRUE ~ "Other"
  )) %>%
  group_by(Kommunenavn, Bloc) %>%
  summarise(Mandates = sum(`Antall mandater`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Bloc, values_from = Mandates, values_fill = 0) %>%
  mutate(
    Majority_2019 = case_when(
      Left > Right ~ "Left",
      Right > Left ~ "Right",
      TRUE ~ "Other"
    )
  )

# --- 2015 ---
Ele_2015_clean <- Ele_2015 %>%
  filter(`Antall mandater` > 0) %>%
  mutate(Bloc = case_when(
    Partikode %in% Left_parties  ~ "Left",
    Partikode %in% Right_parties ~ "Right",
    TRUE ~ "Other"
  )) %>%
  group_by(Kommunenavn, Bloc) %>%
  summarise(Mandates = sum(`Antall mandater`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Bloc, values_from = Mandates, values_fill = 0) %>%
  mutate(
    Majority_2015 = case_when(
      Left > Right ~ "Left",
      Right > Left ~ "Right",
      TRUE ~ "Other"
    )
  )



BlocChange <- Ele_2015_clean %>%
  select(Kommunenavn, Majority_2015) %>%
  mutate(Kommunenavn = str_to_upper(Kommunenavn)) %>%
  left_join(
    Ele_2019_clean %>%
      select(Kommunenavn, Majority_2019) %>%
      mutate(Kommunenavn = str_to_upper(Kommunenavn)),
    by = "Kommunenavn"
  ) %>%
  mutate(
    ChangeMajority = case_when(
      Majority_2015 == "Left"  & Majority_2019 == "Right" ~ "Left_to_Right",
      Majority_2015 == "Right" & Majority_2019 == "Left"  ~ "Right_to_Left",
      Majority_2015 == Majority_2019                      ~ "NoChange",
      TRUE                                                ~ "Unclear"
    )
  )






