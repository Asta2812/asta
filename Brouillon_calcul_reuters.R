#Installation des packages 

install.packages("readxl")
install.packages("janitor")
install.packages("skirm")
install.packages("tydiverse")
install.packages("tidylog")
install.packages("here")
install.packages("data.table")

#library 
library(readxl)
library(janitor)
library(skimr)
library(tidyverse)
library(tidylog)
library(here)
library(data.table)


# 1. Importer données et paramètres -----------------------------------------------
getwd()
setwd("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/")
# Données Reuters
#reuters <- read_xlsx(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/REUTERS_2023/MEGABASE Màj 22.07.2024 VF.xlsx"), sheet = 1, na = "NULL")
reuters <- read_xlsx("REUTERS_2023/MEGABASE_Maj_22072024_VF.xlsx", sheet = 1, na = "NULL")
#reuters <- read_xlsx("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/REUTERS_2023/MEGABASE Màj 22.07.2024 VF.xlsx", sheet = 1, na = "NULL")

# Facteurs conversion electricité (OWID)
#owid_elec_intens <- read_csv(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/carbon-intensity-electricity.csv"))
owid_elec_intens <- read_csv("annexes/carbon-intensity-electricity.csv")

# Nomenclature des pays et régions (UN depuis OWID)
#world_regions <- read_csv(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/world-regions-according-to-the-world-bank.csv"))
world_regions <- read_csv("annexes/world-regions-according-to-the-world-bank.csv")

# Taux de change Banque de France
#taux_de_change <- read.csv2(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/BDF_taux_change_2022_export.csv"))
taux_de_change <- read.csv2("annexes/BDF_taux_change_2022_export.csv")

# nomenclature GICS
#gics_table <- readRDS(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/nomenclature_gics.RDS"))
gics_table <- readRDS("annexes/nomenclature_gics.RDS")

# nommebnclature nace to gics

nace_to_gics <- readRDS("C:/Users/M22527/OneDrive - Bpifrance/Raphael COTTIN - 2024-08_intensites_Asta/data/annexes/nace_to_gics.RDS")


# 2. Nettoyer données Reuters -----------------------------------------------------

#Calcul du nombre de valeurs manquates dans la base Reuters 
library(skimr)
na <- skim(reuters) # données incomplète pour les émissions



reuters <- 
  # clean names
  clean_names(reuters)

reuters <- reuters %>%
  rename_with(~ str_replace(.x, "scope_", "scope"), contains('scope_'))

reuters <- 
  reuters %>%
  # clean gics_4 categories
  rename(country = country_headquater) %>%
  mutate(gics_4 = str_to_lower(gics_4),
         gics_3 = str_to_lower(gics_3),
         gics_2 = str_to_lower(gics_2),
         gics_1 = str_to_lower(gics_1), 
         country = str_to_lower(country))

# correction des gics dans reuters 

reuters <- reuters %>%   # rectification
  rename(code_gics_4 = code_gics_1, 
         gics_4      = gics_1,
         code_gics_2 = code_gics_3, 
         gics_2      = gics_3,
         code_gics_3 = code_gics_2,
         gics_3 = gics_2,
         code_gics_1 = code_gics_4,
         gics_1 = gics_4)


# reuters <- reuters %>% 
#   # variable revenue_usd est au format au numérique (au lieu de caractère)
#   mutate(revenue_usd = na_if(revenue_usd_11, "--"))

reuters <- reuters %>% 
  mutate_at(
    vars(contains('ca') | contains('double') | contains('total') | contains('scope') | contains("revenue")),
    as.double
  ) 

# Enlever total returns et sharpe 
reuters <- reuters %>%
  select(- contains('sharpe_'), -contains('returns'))  # pas de changement

# Renommer les classifications Naics 

reuters <- reuters %>% 
  rename(
    naics_industry_group = naics_idustry_group, 
    #naics_industry_group_code = naics_idustry_group_code,
    trbc_business = trbc_buisness,
    trbc_business_code = trbc_buisness_code
  )

# Changer les chiffres d'affaire à 0 à valeur manquante
reuters <- reuters %>%
  mutate_at(
    vars(contains('ca'), revenue_usd_11),
    ~ ifelse(. == 0, NaN, .)
  )

# Renomme le mode_estimation_2022
# reuters <- reuters %>% 
#   rename(
#     mode_estimation_2022 = report pas de colonne report dans reuters
#   )

# Enlever les doublons de la base reuters 

# Find duplicate names
duplicates <- reuters %>%
  group_by(name_or_code) %>%
  filter(n() > 1) %>%
  ungroup()

    
# Identify rows with the least number of NAs within each duplicate group
reuters_new <- duplicates %>%
  mutate(n_na = rowSums(is.na(across(where(is.numeric))))) %>%
  group_by(name_or_code) %>%
  slice(which.min(n_na)) %>%
  ungroup() %>%
  select(-n_na)

# Remove duplicate rows from the original dataframe
reuters_without_duplicates <- reuters %>%
  anti_join(duplicates, by = "name_or_code") # 32 lignes 

# Concatenate the selected duplicates and non-duplicate rows
reuters <- bind_rows(reuters_new, reuters_without_duplicates)

reuters |>  # RC: certaines lignes ont des GICS Manquants mais NACE présents
  filter(is.na(gics_4)) |> 
  count(nace) |> print(n=Inf)

reuters <- reuters |> #RC: extraction des codes NACE à partir de la description NACE
  mutate(
    nace_code = str_split_i(nace, "\\(", -1),
    nace_code = str_remove(nace_code, "\\)"),
    nace_code = str_remove(nace_code, "\\."),
  )

#importation de la table gics

# nace_gics <- readRDS("data/annexes/nace_to_gics.RDS")
# nace_gics <- nace_to_gics 
# rm(nace_to_gics)
# nace_gics <- nace_to_gics
# rm(nace_to_gics)

reuters <- nace_to_gics |> #RC: récupération des codes 
  select(nace_code, code_gics_table = code_gics_4) |> 
  distinct() |> 
  right_join(reuters, by = "nace_code")

reuters <- reuters |>  #RC: modification des gics 4 NA  
  mutate(code_gics_4 = if_else(
    is.na(code_gics_4) & !is.na(code_gics_table), 
    code_gics_table, 
    code_gics_4
  ))

#RC: vérif: le nombre de GICS 4 NA a diminué 
reuters |> count(is.na(code_gics_4))  #61 valeurs de gics NA 


#RC: nouvelles catégories GICS => à reclassifier

#RC: on reclassifie les nouveaux codes GICS pour coller à la nomenclature pré-2023
reuters <- reuters |> 
  mutate(code_gics_4 = case_when(
    code_gics_4 == 20202030 ~ 20202020, # "Data Processing & Outsourced Services" -> "Research & Consulting Services"
    code_gics_4 == 20304030 ~ 20304020, # Cargo Ground Transportation -> trucking
    code_gics_4 == 20304040 ~ 20304020, # Passenger Ground Transportation -> trucking
    code_gics_4 == 25503030 ~ 25503020, # Broadline Retail -> General Merchandise Store
    code_gics_4 == 40201050 ~ 40201020, # Commercial & Residential Mortgage Finance (New)
    code_gics_4 == 40201060 ~ 40201020, # Transaction & Payment Processing Services (New)
    code_gics_4 == 60102510 ~ 60101020, # Industrial REITs (New Code)
    code_gics_4 == 60103010 ~ 60101030, # Hotel & Resort REITs (New Code), 
    code_gics_4 == 60104010 ~ 60101040, # Office REITs (New Code)
    code_gics_4 == 60105010 ~ 60101050, # Health Care REITs (New Code)
    code_gics_4 == 60106010 ~ 60101060, # Multi-Family Residential REITs (New)
    code_gics_4 == 60106020 ~ 60101060, # Single-Family Residential REITs (New),
    code_gics_4 == 60107010 ~ 60101070, # Retail REITs (New Code)
    code_gics_4 == 60108010 ~ 60101080, # Other Specialized REITs (New Name/ New Code/Definition Update)
    code_gics_4 == 60108020 ~ 60101080, # Self-Storage REITs (New)
    code_gics_4 == 60108030 ~ 60101080, # Telecom Tower REITs (New)
    code_gics_4 == 60108040 ~ 60101080, # Timber REITs (New)
    code_gics_4 == 60108050 ~ 60101080, # Data Center REITs (New)
    code_gics_4 == 60201010 ~ 60102010, # Diversified Real Estate Activities (New Code)
    code_gics_4 == 60201020 ~ 60102020, # Real Estate Operating Companies (New Code), 
    code_gics_4 == 60201030 ~ 60102030, # Real Estate Development (New Code)
    code_gics_4 == 60201040 ~ 60102040, # Real Estate Services (New Code)
    TRUE                    ~ code_gics_4
  ))





# 3. Passage au format 'long' et nettoyage -------------------------------------



temp <- reuters %>%
  pivot_longer(cols = c(contains("scope"), contains("intensite"), 
                        contains("total_"), contains("ca_")),
               names_to = c("variable", "year"), 
               names_sep = "_",
               values_to = "value")

reuters_long <- 
  temp %>%
  pivot_wider(names_from = "variable", 
              values_from = "value") %>% 
  relocate(year, .after = name_or_code)                       

#rm(temp)

#Change le type de la variable year en integer

reuters_long <- reuters_long %>% 
  mutate_at(
    vars(year),
    as.integer
  ) 

# Nettoyage de base au format long

reuters_long <- reuters_long %>%     ## rectification de "faux zeros" sur l'intensité"
  mutate(intensite = ifelse(
    is.na(scope1) &  intensite == 0, 
    NA, 
    intensite)
  )

strings1 <- c("name_or_code", "revenue_usd", "ric", "isin", "year", "country", "revenue_usd")
patterns <- c("nace", "gics", "scope", "naics", "trbc")
strings2 <- c("intensite", "total", "currency", "ca", "mode_estimation_2021", "mode_estimation_2022")

reuters_long <- reuters_long %>% 
  # selectionner variables 
  select(
    any_of(strings1),
    any_of(names(reuters_long)[grepl(paste(patterns, collapse= '|'), names(reuters_long))]),
    any_of(strings2)
  )

#réordonnancement des colonnes de classification
# reuters_long <- reuters_long %>% 
#   relocate(naics_international,
#            naics_sector,
#            naics_subsector,
#            naics_subsector_code,
#            naics_industry_group,
#            naics_industry_group_code,
#            naics_national_industry,
#            naics_national_industry_code, 
#            .after = code_gics_1)
# 
# reuters_long <- reuters_long %>% 
#   relocate(trbc_economic_sector, 
#            trbc_economic_sector_code,
#            trbc_business,
#            trbc_business_code,
#            trbc_industry_group,
#            trbc_industry_group_code,
#            trbc_industry_34,
#            trbc_industry_35,
#            trbc_industry_code_39,
#            trbc_industry_code_40, 
#            .after = naics_national_industry_code
#   )

reuters_long <- reuters_long %>% 
  # certaines valeurs d'émissions totales sont nulles => rectifications en NA
  mutate(total = na_if(total, 0))

reuters_long <- reuters_long %>% 
  # ne garder que les lignes où au moins un des scopes non nul
  filter(!if_all(c(contains("scope"), intensite, total), is.na)) 

# Filter out rows with missing values in the 'country' column
#reuters_long <- reuters_long[complete.cases(reuters_long$country), ] # supprime toutes les lignes des pays où il ya les valeurs manquantes
sum(is.na(reuters_long$country))  # pas de NA
# Rectification des pays (rattacher les paradis fiscaux à un vrai pays)
# reuters_long[reuters_long$country == "china (mainland)", "country"]          <- "china" # rectifier nom pays
# reuters_long[reuters_long$country == "bermuda", "country"]        <- "united kingdom"
# reuters_long[reuters_long$country == "jersey", "country"]         <- "united kingdom"
# reuters_long[reuters_long$country == "guernsey", "country"]       <- "united kingdom"
# reuters_long[reuters_long$country == "gibraltar", "country"]      <- "united kingdom"
# reuters_long[reuters_long$country == "isle of man", "country"]    <- "united kingdom"
# reuters_long[reuters_long$country == "monaco", "country"]         <- "france"
# reuters_long[reuters_long$country == "liechtenstein", "country"]  <- "austria"
# reuters_long[reuters_long$country == "faroe islands", "country"]  <- "denmark"
# reuters_long[reuters_long$country == "macau", "country"]          <- "hong kong"
# reuters_long[reuters_long$country == "czech republic", "country"] <- "czechia"

#Mise en forme et renommage des colonnes 

# reuters <- reuters %>%   # rectification
#   select(-c(gics_4, code_gics_4, code_gics_2, gics_2)) |> 
#   rename(code_gics_4 = code_gics_1, 
#          gics_4      = gics_1,
#          code_gics_2 = code_gics_3, 
#          gics_2      = gics_3)


#RC: Rectification des pays (rattacher les paradis fiscaux à un vrai pays) (#RC: qq modifications ici)
reuters_long[reuters_long$country == "bermuda", "country"]              <- "united kingdom"
reuters_long[reuters_long$country == "jersey", "country"]               <- "united kingdom"
reuters_long[reuters_long$country == "guernsey", "country"]             <- "united kingdom"
reuters_long[reuters_long$country == "gibraltar", "country"]            <- "united kingdom"
reuters_long[reuters_long$country == "isle of man", "country"]          <- "united kingdom"
reuters_long[reuters_long$country == "cayman islands", "country"]       <- "united kingdom"
reuters_long[reuters_long$country == "monaco", "country"]               <- "france"
reuters_long[reuters_long$country == "liechtenstein", "country"]        <- "austria"
reuters_long[reuters_long$country == "faroe islands", "country"]        <- "denmark"
reuters_long[reuters_long$country == "macau", "country"]                <- "hong kong"
reuters_long[reuters_long$country == "czech republic", "country"]       <- "czechia"
reuters_long[reuters_long$country == "ireland; republic of", "country"] <- "ireland"
reuters_long[reuters_long$country == "korea; republic (s. korea)", "country"] <- "korea"



# Rectification de nomenclature GICS 4 dans Reuters

reuters_long <- reuters_long %>% 
  mutate(gics_2 = ifelse(gics_2 == "commercial  & professional services", 
                         "commercial & professional services", gics_2)) %>% 
  select(-c(gics_4, gics_3, gics_2, gics_1, code_gics_3, code_gics_2, code_gics_1)) %>% 
  left_join(gics_table, by = "code_gics_4") %>% 
  relocate(
    code_gics_1,
    gics_2,
    code_gics_2,
    gics_3, 
    code_gics_3,
    gics_4,
    .before = code_gics_4
  ) 


reuters_long |> 
  filter(is.na(gics_4)) |>  # il ne reste que trois valeurs manquantes dans le code_gics4

# RC: vérification 
reuters_long %>% 
  filter(is.na(code_gics_3)) |> 
  count(gics_4) # les lignes qui ne matchent pas sont celles sans code GICS

reuters_long |>  # RC: certaines lignes ont des GICS Manquants mais NACE présents
  filter(is.na(gics_4)) |> 
  count(nace) |> print(n=Inf)
reuters_long |> filter(is.na(gics_4))

# Création d'une intensité calculée
reuters_long <- reuters_long %>%
  mutate(
    intensite_computed = total/ ca*1e6
  ) 

# Création d'une value de CA en MUSD
reuters_long <- reuters_long %>%
  mutate(
    ca_MUSD = ca/1e6
  ) 
# garder uniquement reporté
# reuters_long <- reuters_long %>%
#   filter(mode_estimation_2021 == "Reported" | mode_estimation_2022 == "Reported")


# Remplacement de l'intensite Reuters par l'intensité calculée                

reuters_long <- reuters_long %>%
  mutate(
    intensite = ifelse(
      !is.na(intensite_computed), 
      intensite_computed,
      intensite                    
    )
  )


# 4. Nettoyage de données OWID  ------------------------------------------------



# Utilisation des noms d'origine comme labels de variables
labels <- sjlabelled::get_label(owid_elec_intens, def.value = colnames(owid_elec_intens))
labelled::var_label(owid_elec_intens) <- labels

# Noms de variable en snakecase
owid_elec_intens <- clean_names(owid_elec_intens)

# Renommer variables
owid_elec_intens <- owid_elec_intens %>% 
  rename(intens_elec = carbon_intensity_of_electricity_g_co2_k_wh, 
         country = entity)

# Nom de pays en minuscule
owid_elec_intens <- owid_elec_intens %>% 
  mutate(country = str_to_lower(country))

# Mettre les valeurs pour regions dans base à part
owid_elec_intens_groups <- owid_elec_intens %>% 
  filter(is.na(code) | code == "OWID_WRL") 


# Garder uniquement les pays (enlever les régions)
owid_elec_intens <- owid_elec_intens %>% filter(!is.na(code))

owid_elec_intens <- owid_elec_intens %>%
  filter(code != "OWID_WRL")


# 5. Nettoyage du fichier world_regions ----------------------------------------

# Nettoyer 
world_regions <- clean_names(world_regions)

# Régions 
world_regions <- world_regions %>% 
  rename(region = world_region_according_to_the_world_bank) %>% 
  mutate(country = str_to_lower(entity), .after = entity) %>% 
  select(-c(year, entity))

# Ajouter le champ regions aux intensités
owid_elec_intens <- world_regions %>% select(-country) %>% 
  right_join(owid_elec_intens) %>% 
  relocate(region, .after = "country")

# la région est "NA" pour Taiwan
owid_elec_intens[owid_elec_intens$country == "taiwan","region"] <- "East Asia and Pacific"


# 6. Calcul du facteur de retraitement scope 2 ---------------------------------

owid_dt = as.data.table(owid_elec_intens)


# Calculer la moyenne des trois année pré-crise

mean_intens_dt <-  owid_dt[year %in% c(2021,2022,2023), 
                           .(intens_elec_mean = mean(intens_elec, na.rm = T)), 
                           by = country]

# ajouter labels
labelled::var_label(mean_intens_dt$intens_elec_mean) <-
  "intensité moyenne de l'électricité, 2021-2023 (gCO2/kWh)"

# ajouter code pays
mean_intens_dt <-  owid_elec_intens %>%  as.data.table() %>% 
  select(country, code, region) %>% distinct() %>% 
  merge(mean_intens_dt, all.y = TRUE, by = "country")


# Calculer le facteur de retraitement de l'électricité

intens_FR <- mean_intens_dt[country == "france", intens_elec_mean]
mean_intens_dt[,fact_retr := intens_elec_mean/intens_FR][]





# 7. Passage au format Long Est  -----------------------------------------------





# récuperer l'intensité  scope 1 implicite dans l'intensité totale
reuters_long_est <- reuters_long %>% 
  mutate(
    intens_sc1 = (scope1*intensite)/(scope1+scope2)
  )


# calculer l'intensité scope 2 implicite
reuters_long_est <- reuters_long_est %>% 
  mutate(
    intens_sc2_base = intensite - intens_sc1,
    intens_sc2_bis  = scope2 / ca_MUSD
  ) %>% 
  mutate(
    intens_sc2 = case_when(
      !is.na(intens_sc2_base)   ~ intens_sc2_base, 
      is.na(intens_sc2_base)    ~ intens_sc2_bis
    )
  ) %>% 
  select(-c(intens_sc2_base, intens_sc2_bis))

# windsorisation des intensités scope 1 et scope 2
reuters_long_est <- reuters_long_est %>% 
  # extraction des 5e et 99e centiles par gics 1
  group_by(gics_1) %>% 
  summarise(
    sc1_p05       = quantile(intens_sc1, probs = 0.05, na.rm = T),
    sc1_p99       = quantile(intens_sc1, probs = 0.99, na.rm = T),
    sc2_p05       = quantile(intens_sc2, probs = 0.05, na.rm = T),
    sc2_p99       = quantile(intens_sc2, probs = 0.99, na.rm = T)
  ) %>% 
  # ajouter à base de reuters_long
  right_join(reuters_long_est, by = "gics_1") %>% 
  # création de variable windsorisée
  mutate(
    intens_sc1_w = case_when(
      intens_sc1 < sc1_p05               ~ sc1_p05, 
      intens_sc1 > sc1_p99               ~ sc1_p99, 
      TRUE                               ~ intens_sc1),
    intens_sc2_w = case_when(
      intens_sc2 < sc2_p05               ~ sc2_p05, 
      intens_sc2 > sc2_p99               ~ sc2_p99, 
      TRUE                               ~ intens_sc2)
  ) %>% 
  # enlever variables intermédiaires
  select(-c(sc1_p05, sc1_p99, sc2_p05, sc2_p99
  ))
# Duplication de la bd reuters 


# Retraitement du scope 2 à partir de données OWID  -----------------------------



# ajouter le facteur retraitement scope 2 à base reuters
reuters_long_est <- mean_intens_dt %>% as_tibble() %>% 
  select(country, fact_retr) %>% 
  right_join(reuters_long_est, by  = "country")

# appliquer le facteur de retraitement au scope 2
 reuters_long_est <- reuters_long_est %>% 
   mutate(
     intens_sc2_w_retr = intens_sc2_w / fact_retr,
   )

# additionner scope 1 et 2 retraités
 reuters_long_est <- reuters_long_est %>% 
   mutate(
     intensite_retr  = intens_sc1_w + intens_sc2_w_retr #ok
   )
reuters_long_est <- reuters_long_est %>% 
  mutate(
    intensite_retr  = intens_sc1_w + intens_sc2_w 
  )

# compléter données d'intensités retraitées
reuters_long_est <- reuters_long_est %>% 
  mutate(intensite_retr = case_when(
    is.na(intensite_retr) & !is.na(intensite)  ~ intensite,
    TRUE                                       ~ intensite_retr)
  ) %>% 
  mutate(intens_sc2_flag = case_when(
    is.na(intensite_retr) & !is.na(intensite)  ~ "pas de retraitement scope2",
    TRUE                                       ~ "scope 2 retraité"
  ))


# Variabilité excessive d'une année sur l'autre --------------------------

statdesc <- function(data, var){
  data %>% 
    summarise(
      n = n(), 
      missing = sum(is.na({{ var }})), 
      mean = mean({{ var }}, na.rm = T),
      sd = sd({{ var }}, na.rm = T), 
      min = min({{ var }}, na.rm = T), 
      p01 = quantile({{ var }}, probs = 0.01, na.rm = T),
      p10 = quantile({{ var }}, probs = 0.10, na.rm = T),
      p25 = quantile({{ var }}, probs = 0.25, na.rm = T),
      median = median({{ var }}, na.rm = T),
      p75 = quantile({{ var }}, probs = 0.75, na.rm = T),
      p90 = quantile({{ var }}, probs = 0.90, na.rm = T),
      p99 = quantile({{ var }}, probs = 0.99, na.rm = T),
      max = max({{ var  }}, na.rm = T)
    ) %>% 
    mutate(across(
      where(is.numeric), 
      round)
    )
}


reduce_variation <- function(reuters_long_est) {
  # création d'intensité lead/lag dans base "long"
  reuters_long_est <- reuters_long_est %>% 
    arrange(name_or_code, year) %>% 
    group_by(name_or_code) %>% 
    mutate(intensite_lag = lag(intensite_retr, order_by = year),
           intensite_lead = lead(intensite_retr, order_by = year)
    ) %>% 
    ungroup() 
  
  reuters_long_est <-  reuters_long_est %>% 
    # calcul de la variation des intensités laggées
    mutate(var_intens_left     = (intensite_retr / intensite_lag - 1)*100,
           var_intens_right = (intensite_lead / intensite_retr - 1)*100
    )
  
  # Les deux distributions sont très proches par définition
  summary_var_intens_left <- reuters_long_est %>% statdesc(var_intens_left)
  summary_var_intens_right <- reuters_long_est %>% statdesc(var_intens_right)
  
  reuters_long_est <- reuters_long_est %>% 
    # remplacer les valeurs par NA lorsque variation trop forte                  
    mutate(
      intensite_retr = case_when(
        var_intens_left      > pull(summary_var_intens_left['p99'])   ~ NA_real_,
        var_intens_left      < pull(summary_var_intens_left['p01'])   ~ NA_real_,
        
        var_intens_right  > pull(summary_var_intens_right['p99'])   ~ NA_real_,
        var_intens_right  < pull(summary_var_intens_right['p01'])   ~ NA_real_,
        TRUE                      ~ intensite_retr)
    ) %>% 
    # supprimer valeurs intermédiaires
    select(-c(intensite_lag, intensite_lead, var_intens_left, var_intens_right)
    )
  
  return(reuters_long_est)
}


previous_reuters_long_est <- NULL
iteration <- 0

# tant que la fonction ajoute des NA à intensites_retr, on itère 
# ici on sort de la boucle while au bout de 2 itérations pour reproduire le 
# comportement passé

while (!identical(reuters_long_est, previous_reuters_long_est)) {
  previous_reuters_long_est <- reuters_long_est
  reuters_long_est <- reduce_variation(reuters_long_est)
  
  iteration = iteration + 1
  
  if(iteration == 2){
    break
  }
}

# éliminer les lignes qui contiennent des NA à intensité retr : le principe de notre algorithme précédent pour limiter la variabilité

reuters_long_est <- reuters_long_est %>% 
  filter(!is.na(intensite_retr))



#Possibilité d'aller jusqu'à la convergence : no changes : !check_changes(old_df, new_df) & iterate in a while pour aller au bout de la logique

#save.image(here("./1-INTENSITES/temp/reuters_clean_long_winds_retr.Rdata"))

# conversion en euros  --------------------------------------------------

#load(here("./1-INTENSITES/temp/reuters_clean_long_winds_retr.Rdata"))


# taux de change USD/EUR 2021 (source Banque de France) # où trouver les taux de change 2022
#moyenne des mensuels

import_taux_USD_EUR <- function(year, file) {                                
  
  file <- file[file$year >= year, ]
  
  names(file)[2] <- "tx_USD_EUR"
  
  file <- file %>% 
    mutate(
      year = as.integer(year),
      tx_USD_EUR = as.double(tx_USD_EUR)
    )
  
  return(file)
  
}

taux_USD_EUR <- import_taux_USD_EUR(2020, taux_de_change) %>%             
  print(digits = 3)


# ajout de region
reuters_long_est <- world_regions %>% 
  right_join(reuters_long_est, by = "country") %>% 
  # rectifier valeur de region pour taiwan 
  mutate(
    region = case_when(
      is.na(region)  ~ "East Asia and Pacific",
      TRUE           ~ region
    )
  )

# variables en Euros (originales et retraitées)
reuters_long_est <- reuters_long_est %>%
  left_join(taux_USD_EUR, by = "year") %>% 
  mutate(
    intensite_retr_EUR = intensite_retr * tx_USD_EUR
  ) %>% 
 # select(-fact_retr) %>%  
  relocate(country, .after = "isin") %>%
  relocate(code, .after = "country") %>%
  relocate(region, .after = "code")


# création d'une variable "nom_bis" nettoyée pour récupérer les valeurs

reuters_long_est <- reuters_long_est %>% 
  mutate(nom_bis = str_to_lower(name_or_code)) %>% 
  mutate(nom_bis = str_remove(nom_bis, " ag")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " spa")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " sarl")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " sas")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " ste")) %>% 
  mutate(nom_bis = str_remove(nom_bis, "ste ")) %>% 
  mutate(nom_bis = str_remove(nom_bis, "sarl ")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " sa")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " ltd")) %>% 
  mutate(nom_bis = str_remove(nom_bis, " inc"))


# Ajout de la colonne gics2bis

# Proxies à maille hybride -----------------------------------------------------


# secteurs à mettre à part dans maille hybride
sect_intens <- c(
  "coal & consumable fuels",
  "construction materials",
  "independent power producers & energy traders",
  "steel",
  "industrial gases",
  "marine",
  "airlines",
  "fertilizers & agricultural chemicals",
  "automotive retail",
  "gas utilities",
  "aluminum", 
  "renewable electricity",
  "packaged foods & meats",                  # scope 3
  "aerospace & defense",                     # scope 3
  "construction machinery & heavy trucks",   # scope 3 
  "trading companies & distributors",        # scope 3
  "agricultural products"                    # scope 3
)

# check that the selected elements of gics_4 in sect_intens are all in gics_table
all(sect_intens %in% gics_table$gics_4)

# adds gics2bis to gics_table
gics_table <- gics_table %>% 
  # création d'une nomenclature hybride aux niveaux gics 4 et gics 2
  mutate(
    gics2bis = case_when(
      gics_4 %in% sect_intens  ~  gics_4,
      TRUE                     ~  gics_2)
  )

reuters_long_est <- reuters_long_est %>% 
  # création d'une nomenclature hybride aux niveaux gics 4 et gics 2
  mutate(
    gics2bis = case_when(
      gics_4 %in% sect_intens  ~  gics_4,
      TRUE                     ~  gics_2)
  ) 

# Exporter données FR -----------------------------------------------------

# reuters_entreprises_FR <- reuters_long  %>% # les colonnes mode estimation 2021 et 2022 n'existe pas dans le dataframe
#   filter(country == "france") %>% 
#   select(name_or_code, ric, isin, year, code_gics_4, scope1, scope2, total, intensite, ca_MUSD, mode_estimation_2021, mode_estimation_2022)

reuters_entreprises_FR <- reuters_long  %>% # les colonnes mode estimation 2021 et 2022 n'existe pas dans le dataframe
  filter(country == "france") %>% 
  select(name_or_code, ric, isin, year, code_gics_4, scope1, scope2, total, intensite, ca_MUSD)


# Ao commentaire sur les grosses intensités des entreprises de la france 

# reuters_entreprises <- reuters_long_est  %>% # les colonnes mode estimation 2021 et 2022 n'existe pas dans le dataframe
#   filter(country == "france") %>% 
#   select(name_or_code, ric, isin, year,gics_2, gics2bis, gics_4, intens = intensite_retr)%>%
#   arrange(desc(intens))%>%
#   pivot_wider(names_from = "year", values_from = c("intens")) 
# 
# years <- c('2023', '2022', '2021')
# 
# reuters_entreprises <- reuters_entreprises %>%
#   rename_with(~ paste('intens', ., sep = "_"), all_of(years))
# 
# reuters_entreprises <- reuters_entreprises%>%
#rm(reuters_entreprises)


# write_csv2(reuters_entreprises_FR, here("./1-INTENSITES/output/reuters_entreprises_FR.csv"))
# 
# rm(reuters_entreprises_FR)

# sauvegarde données ---------------------------------------------

# données reuters au format long
# saveRDS(reuters_long_est,    file = here("./1-INTENSITES/output/reuters_long.RDS"))
# 
# # données reuters nettoyées au format wide
# saveRDS(reuters,    file = here("./1-INTENSITES/output/reuters_wide.RDS"))
# 
# # données OWID
# saveRDS(owid_elec_intens, file = here("./1-INTENSITES/output/owid_elect_intens.RDS"))
# saveRDS(world_regions, file = here("./1-INTENSITES/output/owid_wb_regions.RDS"))


unlink(here("./1-INTENSITES/temp/", "reuters*"))       # efface données temporaires

#ok 
reuters_hybr_2023 <- reuters_long_est %>% ## échantillon total depuis 2018
  group_by(gics2bis) %>% 
  summarise(
    n_tot_since_2018        = sum(!is.na(intensite_retr_EUR)),
    med_int_tot_since_2018  = median(intensite_retr_EUR, na.rm = T)
  ) 

# temp_wide <- reuters_long %>% 
#   select(name_or_code, ric, isin, year, gics_2,  gics2bis, gics_4, code_gics_4, 
#          intens = intensite_retr_EUR, mode_estimation_2021, mode_estimation_2022) %>% 
#   pivot_wider(names_from = "year", values_from = c("intens")) 
# 
# years <- c('2022', '2021', '2020', '2019', '2018')
# 
# temp_wide <- temp_wide %>% rename_with(~ paste('intens', ., sep = "_"), all_of(years))
# 

name_entreprises <- temp_wide %>% 
  select(gics2bis, name_or_code )


temp_wide <- reuters_long_est %>% 
  select(name_or_code, ric, isin, year, gics_2,  gics2bis, gics_4, code_gics_4, 
         intens = intensite_retr_EUR) %>% 
  pivot_wider(names_from = "year", values_from = c("intens"))
  view()

years <- c('2023', '2022', '2021')

temp_wide <- temp_wide %>% rename_with(~ paste('intens', ., sep = "_"), all_of(years))

# AO temp wide bis avec le chiffer d'affaire representant ==> determination de l'empreinte sc 12

temp_wide_bis <- reuters_long_est %>% 
  select(name_or_code, ric, isin, year, gics_2, gics2bis,ca_MUSD,
           intens = intensite_retr_EUR) %>% 
  pivot_wider(names_from ="year", values_from = c("intens", "ca_MUSD"))
# temp wide bis 2024
temp_wide_bis_2024 <- reuters_long_est %>% 
  select(name_or_code, ric, isin, year, gics_2, gics2bis,ca_MUSD,
         intens = intensite_retr_EUR) %>% 
  pivot_wider(names_from ="year", values_from = c("intens", "ca_MUSD"))

years <- c('2023', '2022', '2021')

temp_wide_bis_2024 <- temp_wide_bis_2024 %>% rename_with(~ paste('intens', ., sep = "_"), all_of(years))
skim(temp_wide_bis_2024)
 # cylindrage des intensités 2023 et 2022
temp_wide_bis_gics2_cyl_from_2024 <- 
  temp_wide_bis %>% 
  filter(!is.na(intens_2023) & !is.na(ca_MUSD)) %>%
  group_by(gics_2) %>% 
  summarise(
    n_cyl_2021_from_2024 = sum(!is.na(intens_2023)),
    med_int_cyl_2021_from_2024 = median(intens_2023, na.rm = T),
    empr_sc_12_2023 = intens_2023 * ca_MUSD, na.rm = T
  
  ) 


#les entreprises ayant les plus grandes variations dans airlines 

airlines <- temp_wide %>% 
  filter(gics2bis == "airlines" & !is.na(intens_2023) & !is.na(intens_2022)) %>% 
  mutate(
    var_2023_2022 = (intens_2023 - intens_2022) / intens_2022
  ) %>% 
  arrange(desc(var_2023_2022)) 
#les plus grosses variations sont les entreprises de transports chinoises 

#les entreprises qui s'éloigne très fortement de la médiane pour les intens 2022 et 2023
# Calcul des médianes
median_intens_2023 <- median(airlines$intens_2023)
median_intens_2022 <- median(airlines$intens_2022)

# Calcul de l'écart interquartile (IQR)
iqr_intens_2023 <- IQR(airlines$intens_2023)
iqr_intens_2022 <- IQR(airlines$intens_2022)

# Définir les seuils pour identifier les valeurs aberrantes
threshold_2023 <- 1.5 * iqr_intens_2023
threshold_2022 <- 1.5 * iqr_intens_2022

# Identifier les entreprises avec des valeurs aberrantes
outliers <- airlines[
  (abs(airlines$intens_2023 - median_intens_2023) > threshold_2023) |
    (abs(airlines$intens_2022 - median_intens_2022) > threshold_2022), 
] %>% 
  View()

# Afficher les entreprises avec des valeurs aberrantes
print(outliers)
#



#dectection des entreprises uniquement pour 2023 et 2022

outliers_2023 <- airlines[
  (airlines$intens_2023 - median_intens_2023) > threshold_2023
  , ] 

#enregistrement des resultats pour les valaurs extrêmes de 2023
openxlsx::write.xlsx(outliers_2023, "outliers_2023.xlsx")

#enregitrement des resutats pour les valeurs extêmes de 2022
openxlsx::write.xlsx(outliers_2022, "outliers_2022.xlsx")


#détection des valaurs abberantes pour les intensités de 2022
outliers_2022 <- airlines[
  (airlines$intens_2022 - median_intens_2022) > threshold_2022
  , ] 

# Enregistrement des deux detaframes de resultats dans un fichier xlsx




airlines[
  ((airlines$intens_2023 - median_intens_2023) > threshold_2023) |
    ((airlines$intens_2022 - median_intens_2022) > threshold_2022), 
] %>% 
  View()


#pour le secteurs de la marine
marine <- temp_wide %>% 
  filter(gics2bis == "marine" & !is.na(intens_2023) & !is.na(intens_2022)) %>% 
  mutate(
    var_2023_2022 = (intens_2023 - intens_2022) / intens_2022
  ) %>% 
  arrange(desc(var_2023_2022)) #les plus grandes variation relative des entreprises 

median_marine_intens_2023 <- median(marine$intens_2023)
median_marine_intens_2022 <- median(marine$intens_2022)

# Calcul de l'écart interquartile (IQR)
iqr_marine_intens_2023 <- IQR(marine$intens_2023)
iqr_marine_intens_2022 <- IQR(marine$intens_2022)

#
# Définir les seuils pour identifier les valeurs aberrantes
threshold_marine_2023 <- 1.5 * iqr_marine_intens_2023
threshold_marine_2022 <- 1.5 * iqr_marine_intens_2022

#detections des valaurs abberantes pour marine 2022
outliers_marine_2022 <- marine[
  (marine$intens_2022 - median_marine_intens_2022) > threshold_marine_2022
  , ] #6 entreprises pour 2023 et 2022 avec les intensités 2022 qui s'éloigne de la valaur interquartile
#détection de valaur abberantes pour marine 2023
outliers_marine_2023 <- marine[
  (marine$intens_2023 - median_marine_intens_2023) > threshold_marine_2023
  , ] # 6 entreprises pour 2023 et 2022 dans le secteur marine

#enregistrement des resultas
openxlsx::write.xlsx(outliers_marine_2022, "outliers_marine_2022.xlsx")
openxlsx::write.xlsx(outliers_marine_2023, "ouliers_marine_2023.xlsx")


#6 entreprises dans les intesnssc12 pour les années 2022 et 2023
#

#les entreprises qui s'éloi

sum(airlines$intens_2023)
mean(airlines$intens_2023)
sum(airlines$intens_2022)
mean(airlines$intens_2022)
median(airlines$intens_2023)
median(airlines$intens_2022)  #la mediane et les moyennes ne sont pas eloignées 

#détéction des valaurs extremes dans les deux colonnes 

#détection de valeurx extrêmes dans airlines 



# Itérer sur chaque colonne
detecter_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Appliquer la fonction à la colonne1
valeur_extreme <- detecter_outliers(airlines$intens_2023) %>% 
  View()



# Afficher les valeurs extrêmes et leurs informations textuelles
valeurs_extremes_par_colonne

#les entreprises se situant au dessus de la mediane

#les plus grosses variations ne termes absolue de la distribution



# calcul sur extract 2023 - cylindrage sur 2021 et 2022

temp_gics2bis_cyl_from_2024 <- 
  temp_wide %>% 
  filter(!is.na(intens_2023) & !is.na(intens_2022)) %>%
  group_by(gics2bis) %>% 
  summarise(
    n_cyl_2021_from_2024 = sum(!is.na(intens_2021)),
    med_int_cyl_2021_from_2024 = median(intens_2021, na.rm = T),
    n_cyl_2022_from_2024 = sum(!is.na(intens_2022)),
    med_int_cyl_2022_from_2024 = median(intens_2022, na.rm = T),
    n_cyl_2023_from_2024 = sum(!is.na(intens_2023)),
    med_int_cyl_2023_from_2024 = median(intens_2023, na.rm = T)
    ) 

jointure_entreprise <- left_join(name_entreprises, temp_gics2bis_cyl_from_2024,
                                 by = "gics2bis") %>% 
  View()


anti_join(name_entreprises, temp_gics2bis_cyl_from_2024, by = "gics2bis")

jointure_entreprise <- jointure_entreprise %>% 
  mutate(var_2023_2022 = (med_int_cyl_2023_from_2024 - med_int_cyl_2022_from_2024) / med_int_cyl_2022_from_2024) %>% 
  arrange(var_2023_2022) %>% 
  View()  # ok mais on ne voit pas distinctement les intensités par entreprises


# calcul sur extract 2023 - échantillon total

temp_gics2bis_from_2024 <- 
  temp_wide%>% 
  group_by(gics2bis) %>% 
  summarise(
    n_2021_from_2024 = sum(!is.na(intens_2021)),
    med_int_2021_from_2023 = median(intens_2021, na.rm = T),
    n_2022_from_2024 = sum(!is.na(intens_2022)),
    med_int_2022_from_2024 = median(intens_2022, na.rm = T),
    n_2023_from_2024 = sum(!is.na(intens_2023)),
    med_int_2023_from_2024 = median(intens_2023, na.rm = T)
  ) 

reuters_hybr_2024 <- reuters_hybr_2023 %>% 
  left_join(temp_gics2bis_cyl_from_2024, by = "gics2bis") 

#rm(temp_gics2bis_cyl_from_2023, temp_gics2bis_from_2023, temp_wide)


# save.image(here("./1-INTENSITES/temp/reuters_co2stat_calc.Rdata"))
# 
# 
# # CO2STAT INRATE (SCOPES 3)  ###################################################
# 
# # secteurs à mettre à part dans maille hybride
# sect_intens <- c(
#   "coal & consumable fuels",
#   "construction materials",
#   "independent power producers & energy traders",
#   "steel",
#   "industrial gases",
#   "marine",
#   "airlines",
#   "fertilizers & agricultural chemicals",
#   "automotive retail",
#   "gas utilities",
#   "aluminum", 
#   "renewable electricity",
#   "packaged foods & meats",                  # scope 3
#   "aerospace & defense",                     # scope 3
#   "construction machinery & heavy trucks",   # scope 3 
#   "trading companies & distributors",        # scope 3
#   "agricultural products"                    # scope 3
# )
# 
# # adds gics2bis to gics_table
# gics_table <- gics_table %>% 
#   # création d'une nomenclature hybride aux niveaux gics 4 et gics 2
#   mutate(
#     gics2bis = case_when(
#       gics_4 %in% sect_intens  ~  gics_4,
#       TRUE                     ~  gics_2)
#   )
# 
# co2stat_inrate <- function(inrate, sect_intens){
#   
#   inrate <- inrate %>% # nomenclature hybride au niveau gics 2
#     mutate(
#       gics2bis = case_when(
#         gics_4 %in% sect_intens  ~  gics_4,
#         TRUE                     ~  gics_2)
#     )
#   
#   
#   
#   inrate_hybr2 <- inrate %>% ## échantillon total
#     group_by(gics2bis) %>% 
#     summarise(
#       n = sum(!is.na(int_sc3up_EUR)),
#       int_sc3down_EUR = median(int_sc3down_EUR, na.rm = T),
#       int_sc3up_EUR   = median(int_sc3up_EUR, na.rm = T)
#     ) %>% 
#     print()
#   
#   return(inrate_hybr2)
# }
# 
# inrate_hybr2_2022 <- co2stat_inrate(inrate_2022, sect_intens)
# 
# inrate_hybr2_2021 <- co2stat_inrate(inrate_2021, sect_intens)
# 
# 
# # sauvegarde
# save.image(here("./1-INTENSITES/temp/reuters_inrate_co2stat.Rdata"))
# 
# # EXPORT  PROXIES STAT #########################################################
# 
# load(here("./1-INTENSITES/temp/reuters_inrate_co2stat.Rdata"))
# 
# 
# # Table d'intensité stats tous scopes -----------------------------------------
# 
# 
# # 2023
# par maille GICS 2 hybride

co2stat_2023_hybr <- 
  reuters_hybr_2024 %>% 
  select(gics2bis, med_int_cyl_2023_from_2024, n_cyl_2023_from_2024)  %>% 
  rename(int_sc12_EUR_2023 = med_int_cyl_2023_from_2024,
         n_2023 = n_cyl_2023_from_2024) 

# co2stat_2022_hybr <- 
#   inrate_hybr2_2022 %>% 
#   full_join(co2stat_2022_hybr, by = "gics2bis") %>% 
#   relocate(c(int_sc3up_EUR, int_sc3down_EUR), .after = int_sc12_EUR)
# 
# suffix <- "_2022"
# 
# co2stat_2022_hybr <- co2stat_2022_hybr %>%
#   rename_at(vars(-all_of("gics2bis")), ~paste0(., suffix))


# 2022
# par maille GICS 2 hybride
co2stat_2022_hybr <- 
  reuters_hybr_2024 %>% 
  select(gics2bis, med_int_cyl_2022_from_2024, n_cyl_2022_from_2024)  %>% 
  rename(int_sc12_EUR_2022 = med_int_cyl_2022_from_2024,
         n_2022 = n_cyl_2022_from_2024)


# 2021
# par maille GICS 2 hybride
co2stat_2021_hybr <- 
  reuters_hybr_2024 %>% 
  select(gics2bis, med_int_cyl_2021_from_2024, n_cyl_2021_from_2024)  %>% 
  rename(int_sc12_EUR_2021 = med_int_cyl_2021_from_2024,
         n_2021 = n_cyl_2021_from_2024) 
# co2stat_2021_hybr <- 
#   inrate_hybr2_2021 %>% 
#   full_join(co2stat_2021_hybr, by = "gics2bis") %>% 
#   relocate(c(int_sc3up_EUR, int_sc3down_EUR), .after = int_sc12_EUR)

# suffix <- "_2021"
# 
# co2stat_2021_hybr <- co2stat_2021_hybr %>%
#   rename_at(vars(-all_of("gics2bis")), ~paste0(., suffix))

# Table unique pour l'extract 2023 à la maille GICS 4 (pour la DR)
co2stat_sc123_extract_2024 <-  co2stat_2021_hybr %>% 
  full_join(co2stat_2022_hybr, by="gics2bis") %>% 
  full_join(co2stat_2023_hybr, by = "gics2bis")

# Calcul des varations des intensités entre les trois années



co2stat_sc12_extract_2024 <- co2stat_sc123_extract_2024 %>% 
  sjlabelled::var_labels(
    gics2bis                = "secteur",
    n_2021                  = "effectif 2021",
    n_2022                  = "effectif 2022",
    n_2023                  = "effectif 2023",
    int_sc12_EUR_2021       = "intensité scope 1+2, 2021 (tCO2/M€ CA)",
    int_sc12_EUR_2022       = "intensité scope 1+2, 2022 (tCO2/M€ CA)",
    int_sc12_EUR_2023       = "intensité scope 1+2, 2023 (tCO2/M€ CA)"
  ) 

# Enregistrement des résultats 
openxlsx::write.xlsx(co2stat_sc12_extract_2024, "co2stat_sc12_extract_2024_vAO.xlsx")
openxlsx::write.xlsx(reuters_hybr_2024 , "reuters_hybr_2024_vAO.xlsx")

#jointure de co2stat en gics2



# Ajout de la table calculée et diffusée en 2021 (à partir de l'extract 2022)
# co2stat_sc123_extract_2022 <- co2stat_sc123_2022 %>% 
#   rename(
#     int_sc12_EUR_2021 = int_sc12,
#     int_sc3up_EUR_2021 = int_sc3up,
#     int_sc3down_EUR_2021 = int_sc3down
#   ) 
# 
# co2stat_sc123_extract_2022 <- co2stat_sc123_extract_2022 %>% 
#   sjlabelled::var_labels(
#     int_sc12_EUR_2021       = "intensité scope 1+2, 2021 (tCO2/M€ CA)",
#     int_sc3up_EUR_2021      = "intensité scope 3 amont, 2021 (tCO2/M€ CA)",
#     int_sc3down_EUR_2021    = "intensité scope 3 aval, 2021 (tCO2/M€ CA)",
#   )

# Création du Excel

# exporter en format excel
# dico_extract_2023 <- labelled::look_for(co2stat_sc123_extract_2023, details = "none")
# dico_extract_2022 <- labelled::look_for(co2stat_sc123_extract_2022, details = "none")
# 
# l = 
#   list(
#     "intensités extract 2023"  = co2stat_sc123_extract_2023,
#     "intensités extract 2022"  = co2stat_sc123_extract_2022,
#     "descr. variables extract 2023" = dico_extract_2023, 
#     "descr. variables extract 2022" = dico_extract_2022
#   )

# écrire au format excel
# openxlsx::write.xlsx(l, file = here("1-INTENSITES/output", "co2stat_sc123_2022_gics4.xlsx"), asTable = F)
# 
# 
# # base CO2 stat par annee ------------------------------------------------------
# saveRDS(inrate_hybr2_2022,    here("./1-INTENSITES/output/co2stat_inrate_2022.RDS"))
# saveRDS(inrate_hybr2_2021,    here("./1-INTENSITES/output/co2stat_inrate_2021.RDS"))
# saveRDS(reuters_hybr_2023,   here("./1-INTENSITES/output/co2stat_reuters_2022.RDS"))
# 
# write_csv2(inrate_hybr2_2022,    here("./1-INTENSITES/output/co2stat_inrate_2022.csv"))
# write_csv2(inrate_hybr2_2021,    here("./1-INTENSITES/output/co2stat_inrate_2021.csv"))
# write_csv2(reuters_hybr_2023,   here("./1-INTENSITES/output/co2stat_reuters_2022.csv"))
# 
# unlink(here("./1-INTENSITES/temp/", "*co2stat*"))   # effacer fichier temporaires
# 












# Prédiction de l'emprente carbone des entreprises (partie modélisatio)


library(glmnet)

#indentification des variables constantes 

contante <- sapply(reuters_long_est, function(x) length(unique(x)) == 1)

print(names(reuters_long_est[contante])) 
reuters_long_est_bis <- reuters_long_est

#"intens_sc2_flag" est une varaible constante qui doit être enlever dans le df
reuters_long_est_bis <- reuters_long_est_bis %>% 
 

# Pour faire l'elastic net il faut que le df soit sans valeurs manquantes 
df_sans_na <- na.omit(reuters_long_est_bis)
str(without_na) #type de mes variables dans mon datframe 
# cons
cons <- sapply(reuters_long_est_bis, function(x) length(unique(x)) == 1)
print(names(reuters_long_est_bis[cons])) # aucune varaible constante dna le df
facteur_un_niveau <- sapply(reuters_long_est_bis, function(x) is.factor(x) && length(unique(x)) == 1)
print(names(reuters_long_est_bis)[facteur_un_niveau])

X <- model.matrix(intensite_retr ~ . - 1, data = without_na)  # création d'une matrice pour toutesles variables)
y <- without_na$intensite_retr  # Variable cible (mon y)

# Effectuer la régression Elastic Net
# Définir une grille pour lambda et alpha
grid <- 10^seq(10, -2, length = 100)

# vérification du nobre de ligne et de colonne dans le datframe  ok 
nrow(X)
length(y)
#filtrage des NA dans le df
reuters_long_est_bis <-
  reuters_long_est_bis[!is.na(reuters_long_est_bis$intensite_retr), ]

without_na <- na.omit(reuters_long_est_bis) 
elastic_net_model <- cv.glmnet(X, y, alpha = 0.5, lambda = grid )

# Visualisation du modèle pour voir la valeur de lambda qui minimise l'erreur de validation croisée
plot(elastic_net_model) #interpretation ok (il trouve paraletre adoté qui minimise l'erreur)

# Extraire la meilleure valeur de lambda
best_lambda <- elastic_net_model$lambda.min

# Modèle final utilisant la meilleure valeur de lambda  ==> faible plutot rassurant 
final_model <- glmnet(X, y, alpha = 0.5, lambda = best_lambda)
# plutot bon qui minimise l'erreur dans le modèle 

print(final_model)

# Prédiction ==< bonne prédiction avec bonne performance ok  
predictions <- predict(final_model, s = best_lambda, newx = X)

# 
without_na$predicted_intensite <- predictions
without_na$predicted_intensite
# 
saveRDS(final_model, "elastic_net_model.rds")
write.csv(without_na, "predicted_intensites_elastic_net.csv", row.names = FALSE)

# comparaison des capacités des valeurs actuelles et des valeurs prédites

library(Metrics)

# dissociation des deux modèles 
actuals <- without_na$intensite_retr
predicted <- without_na$predicted_intensite

# Calcul de l'erreur quadratique moyenne (MSE)
mse_value <- mse(actuals, predicted)
mse_value
mse_predicted <- mse(predicted)


