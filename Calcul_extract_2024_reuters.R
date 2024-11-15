#installation des packages 
library(readxl)
library(janitor)
library(skimr)
library(tidyverse)
library(tidylog)
library(here)
library(data.table)
library(dplyr)

# 1. Importer données et paramètres -----------------------------------------------

# Données Reuters
reuters <- read_xlsx(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/REUTERS_2023/Mega Base Titre 09042024.xlsx"), sheet = 1, na = "NULL")

# Facteurs conversion electricité (OWID)
owid_elec_intens <- read_csv(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/carbon-intensity-electricity.csv"))

# Nomenclature des pays et régions (UN depuis OWID)
world_regions <- read.csv("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/world-regions-according-to-the-world-bank.csv", sep =",")

# Taux de change Banque de France
taux_de_change <- read.csv2(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/BDF_taux_change_2022_export.csv"))

# nomenclature GICS
gics_table <- readRDS(here("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/annexes/nomenclature_gics.RDS"))




# 2. Nettoyer données Reuters -----------------------------------------------------



reuters <- 
  # clean names
  clean_names(reuters)

reuters <- reuters %>%
  rename_with(~ str_replace(.x, "scope_", "scope"), contains('scope_'))

reuters <- 
  reuters %>%
  # clean gics_4 categories
  rename(country = country_of_issuer) %>%
  mutate(gics_4 = str_to_lower(gics_4),
         gics_3 = str_to_lower(gics_3),
         gics_2 = str_to_lower(gics_2),
         gics_1 = str_to_lower(gics_1), 
         country = str_to_lower(country))

reuters <- reuters %>% 
  # variable revenue_usd est au format au numérique (au lieu de caractère)
  mutate(revenue_usd = na_if(revenue_usd, "--"))

reuters <- reuters %>% 
  mutate_at(
    vars(contains('ca') | contains('double') | contains('total') | contains('scope') | contains("revenue")),
    as.double
  ) 

# Enlever total returns et sharpe 
reuters <- reuters %>%
  select(- contains('sharpe_'), -contains('returns'))

# Renommer les classifications Naics 

reuters <- reuters %>% 
  rename(
    naics_industry_group = naics_idustry_group, 
    naics_industry_group_code = naics_idustry_group_code,
    trbc_business = trbc_buisness,
    trbc_business_code = trbc_buisness_code
  )

# Changer les chiffres d'affaire à 0 à valeur manquante
reuters <- reuters %>%
  mutate_at(
    vars(contains('ca'), revenue_usd),
    ~ ifelse(. == 0, NaN, .)
  )

# Renomme le mode_estimation_2022
reuters <- reuters %>% 
  rename(
    mode_estimation_2022 = report
  )

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
  anti_join(duplicates, by = "name_or_code")

# Concatenate the selected duplicates and non-duplicate rows
reuters <- bind_rows(reuters_new, reuters_without_duplicates)



# 3. Passage au format 'long' et nettoyage -------------------------------------


library(tidyr)

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
reuters_long <- reuters_long %>% 
  relocate(naics_international,
           naics_sector,
           naics_subsector,
           naics_subsector_code,
           naics_industry_group,
           naics_industry_group_code,
           naics_national_industry,
           naics_national_industry_code, 
           .after = code_gics_1)

reuters_long <- reuters_long %>% 
  relocate(trbc_economic_sector, 
           trbc_economic_sector_code,
           trbc_business,
           trbc_business_code,
           trbc_industry_group,
           trbc_industry_group_code,
           trbc_industry_34,
           trbc_industry_35,
           trbc_industry_code_39,
           trbc_industry_code_40, 
           .after = naics_national_industry_code
  )

reuters_long <- reuters_long %>% 
  # certaines valeurs d'émissions totales sont nulles => rectifications en NA
  mutate(total = na_if(total, 0))

reuters_long <- reuters_long %>% 
  # ne garder que les lignes où au moins un des scopes non nul
  filter(!if_all(c(contains("scope"), intensite, total), is.na)) 

# Filter out rows with missing values in the 'country' column
reuters_long <- reuters_long[complete.cases(reuters_long$country), ]

# Rectification des pays (rattacher les paradis fiscaux à un vrai pays)
reuters_long[reuters_long$country == "china (mainland)", "country"]          <- "china" # rectifier nom pays
reuters_long[reuters_long$country == "bermuda", "country"]        <- "united kingdom"
reuters_long[reuters_long$country == "jersey", "country"]         <- "united kingdom"
reuters_long[reuters_long$country == "guernsey", "country"]       <- "united kingdom"
reuters_long[reuters_long$country == "gibraltar", "country"]      <- "united kingdom"
reuters_long[reuters_long$country == "isle of man", "country"]    <- "united kingdom"
reuters_long[reuters_long$country == "monaco", "country"]         <- "france"
reuters_long[reuters_long$country == "liechtenstein", "country"]  <- "austria"
reuters_long[reuters_long$country == "faroe islands", "country"]  <- "denmark"
reuters_long[reuters_long$country == "macau", "country"]          <- "hong kong"
reuters_long[reuters_long$country == "czech republic", "country"] <- "czechia"


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

# Création d'une intensité calculée
reuters_long <- reuters_long %>%
  mutate(
    intensite_computed = total/ ca*1e6
  ) 

# Création d'une value de CA en MUSD (je ne l'ai pas diviser par 1 million pour pour avoir des chiffres cohérents)
reuters_long <- reuters_long %>%
  mutate(
    ca_MUSD = ca
  )

# garder uniquement reporté ( il n'ya pas de colonne reported dans le dataframe)
#reuters_long <- reuters_long %>%
  #filter(mode_estimation_2021 == "Reported" | mode_estimation_2022 == "Reported")


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
library(janitor)
owid_elec_intens <-
  clean_names(owid_elec_intens)

# Renommer variables
owid_elec_intens <- owid_elec_intens %>% 
  rename(intens_elec = carbon_intensity_of_electricity_g_co2_k_wh, 
         country = entity)

# Nom de pays en minuscule
library(tidyverse)
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
library(data.table)
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
    intensite_retr  = intens_sc1_w + intens_sc2_w_retr
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



# taux de change USD/EUR 2023 (source Banque de France) # où trouver les taux de change 2022
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

taux_USD_EUR <- import_taux_USD_EUR(2023, taux_de_change) %>%             
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
  select(-fact_retr) %>%  
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


reuters_entreprises_FR <- reuters_long  %>% 
  filter(country == "france") %>% 
  select(name_or_code, ric, isin, year, code_gics_4, scope1, scope2, total, intensite, ca_MUSD)


#Calcul des intensités extract2024


co2stat_sc123_2022   <- read.csv2("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/donnees_2022/co2stat_reuters_2022.csv")


#save.image(here("./1-INTENSITES/temp/temp_co2stat.Rdata"))

# CO2STAT REUTERS ############################################################


reuters_hybr_2023 <- reuters_long_est %>% ## échantillon total depuis 2018
  group_by(gics2bis) %>% 
  summarise(
    n_tot_since_2018        = sum(!is.na(intensite_retr)),
    med_int_tot_since_2018  = median(intensite_retr, na.rm = T)
  ) 

temp_wide <- reuters_long_est %>% 
  select(name_or_code, ric, isin, year, gics_2,  gics2bis, gics_4, code_gics_4, 
         intens = intensite_retr) %>% 
  pivot_wider(names_from = "year", values_from = c("intens")) 

years <- c('2023', '2022', '2021')

temp_wide <- temp_wide %>% rename_with(~ paste('intens', ., sep = "_"), all_of(years))


# calcul sur extract 2023 - cylindrage sur 2021 et 2022

temp_gics2bis_cyl_from_2024 <- 
  temp_wide %>% 
  filter(!is.na(intens_2023) & !is.na(intens_2022) & !is.na(intens_2021) ) %>% 
  group_by(gics2bis) %>% 
  summarise(
    n_cyl_2023_from_2024 = sum(!is.na(intens_2023)),
    med_int_cyl_2023_from_2024 = median(intens_2023, na.rm = T),
    n_cyl_2022_from_2024 = sum(!is.na(intens_2022)),
    med_int_cyl_2022_from_2024 = median(intens_2022, na.rm = T),
    n_cyl_2021_from_2024 = sum(!is.na(intens_2021)),
    med_int_cyl_2021_from_2024 = median(intens_2021, na.rm = T)
  )

# calcul sur extract 2023 - échantillon total

temp_gics2bis_from_2024 <- 
  temp_wide%>% 
  group_by(gics2bis) %>% 
  summarise(
    n_2021_from_2024 = sum(!is.na(intens_2021)),
    med_int_2021_from_2024 = median(intens_2021, na.rm = T),
    n_2022_from_2024 = sum(!is.na(intens_2022)),
    med_int_2022_from_2024 = median(intens_2022, na.rm = T),
    n_2023_from_2024 = sum(!is.na(intens_2023)),
    med_int_2023_from_2024 = median(intens_2023, na.rm = T)
  )

reuters_hybr_2024 <- reuters_hybr_2023 %>% 
  left_join(temp_gics2bis_cyl_from_2024, by = "gics2bis") %>%
  left_join(temp_gics2bis_from_2024, by = "gics2bis") 


rm(temp_gics2bis_cyl_from_2023, temp_gics2bis_from_2023, temp_wide)


save.image(here("./1-INTENSITES/temp/reuters_co2stat_calc.Rdata"))


# CO2STAT INRATE (SCOPES 3)  ###################################################

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

# adds gics2bis to gics_table
gics_table <- gics_table %>% 
  # création d'une nomenclature hybride aux niveaux gics 4 et gics 2
  mutate(
    gics2bis = case_when(
      gics_4 %in% sect_intens  ~  gics_4,
      TRUE                     ~  gics_2)
  )

#rectification des ordres de grandeurs des colonnes intensités

# sélection des colonnes à muliplier
cols_to_modify <- c("med_int_tot_since_2018", "med_int_cyl_2023_from_2024", "med_int_cyl_2022_from_2024", "med_int_cyl_2021_from_2024","med_int_2023_from_2024",
                    "med_int_2022_from_2024", "med_int_2021_from_2024")

# Multiplication des colonnes sélectionnées par 1 million
reuters_hybr_2024[cols_to_modify] <- reuters_hybr_2024[cols_to_modify] * 1000000


#exportation des données extact 2024

library("xlsx")
library("writexl")
write_xlsx(reuters_hybr_2024, "Extract_reuters_2024.xlsx")

# EXPORT  PROXIES STAT #########################################################


# Table d'intensité stats tous scopes -----------------------------------------


# 2023
# par maille GICS 2 hybride
co2stat_2023_hybr <- 
  reuters_hybr_2024 %>% 
  select(gics2bis, med_int_cyl_2023_from_2024)  %>% 
  rename(int_sc12_EUR_2023 = med_int_cyl_2023_from_2024) 



#co2stat_2022_hybr <- 
#inrate_hybr2_2022 %>% 
#full_join(co2stat_2022_hybr, by = "gics2bis") %>% 
#relocate(c(int_sc3up_EUR, int_sc3down_EUR), .after = int_sc12_EUR)

#suffix <- "_2023"

#co2stat_2023_hybr <- co2stat_2023_hybr %>%
#rename_at(vars(-all_of("gics2bis")), ~paste0(., suffix))

# 2022
# par maille GICS 2 hybride
co2stat_2022_hybr <- 
  reuters_hybr_2024 %>% 
  select(gics2bis, med_int_cyl_2022_from_2024)  %>% 
  rename(int_sc12_EUR_2022 = med_int_cyl_2022_from_2024)


# 2021
# par maille GICS 2 hybride
co2stat_2021_hybr <- 
  reuters_hybr_2024 %>% 
  select(gics2bis, med_int_cyl_2021_from_2024)  %>% 
  rename(int_sc12_EUR_2021 = med_int_cyl_2021_from_2024)

#co2stat_2021_hybr <- 
#inrate_hybr2_2021 %>% 
#full_join(co2stat_2021_hybr, by = "gics2bis") %>% 
#relocate(c(int_sc3up_EUR, int_sc3down_EUR), .after = int_sc12_EUR)

#suffix <- "_2021"

#co2stat_2021_hybr <- co2stat_2021_hybr %>%
#rename_at(vars(-all_of("gics2bis")), ~paste0(., suffix))

# Table unique pour l'extract 2024 à la maille GICS 4 (pour la DR)
co2stat_sc123_extract_2024 <-  co2stat_2023_hybr %>% 
  full_join(co2stat_2023_hybr, by="gics2bis") %>% 
  full_join(co2stat_2022_hybr, by="gics2bis") %>% 
  full_joint(co2stat_2022_hybr, by="gics2bis")%>% 
  full_join(gics_table, by = "gics2bis") %>% 
  select(-c(code_gics_1, gics_1, code_gics_3, gics_3)) %>% 
  mutate(
    indicator_gics = ifelse(gics2bis %in% gics_table$gics_4, 0, 1)
  ) %>% 
  relocate(c(indicator_gics, code_gics_2, gics_2, code_gics_4, gics_4), .after = gics2bis) 

co2stat_sc123_extract_2024 <- co2stat_sc123_extract_2024 %>%   #pour les effectifs prendre les effectis des années 2023,2022,2021
  sjlabelled::var_labels(
    gics2bis                = "secteur",
    n_2023                  = "effectif 2023",  
    n_2022                  = "effectif 2022",
    n_2021                  = "effectif 2021",
    int_sc12_EUR_2021       = "intensité scope 1+2, 2021 (tCO2/M€ CA)",
    int_sc12_EUR_2022       = "intensité scope 1+2, 2022 (tCO2/M€ CA)",
    int_sc12_EUR_2023       = "intensité scope 1+2, 2023 (tCO2/M€ CA)",
    indicator_gics          = "1 si le secteur viens de la maille gics 4, 0 sinon (gics 2)" 
  )


#A completer avec les données de l'inrate ==> en attente des données (voir Raphael Cottin)

co2stat_sc123_extract_2024 <- co2stat_sc123_extract_2024 %>% 
  sjlabelled::var_labels(
    gics2bis                = "secteur",
    n_2021                  = "effectif 2021",
    n_2022                  = "effectif 2022",
    int_sc12_EUR_2021       = "intensité scope 1+2, 2021 (tCO2/M€ CA)",
    int_sc3up_EUR_2021      = "intensité scope 3 amont, 2021 (tCO2/M€ CA)",
    int_sc3down_EUR_2021    = "intensité scope 3 aval, 2021 (tCO2/M€ CA)", 
    int_sc12_EUR_2022       = "intensité scope 1+2, 2022 (tCO2/M€ CA)",
    int_sc3up_EUR_2022      = "intensité scope 3 amont, 2022 (tCO2/M€ CA)",
    int_sc3down_EUR_2022    = "intensité scope 3 aval, 2022 (tCO2/M€ CA)", 
    indicator_gics          = "1 si le secteur viens de la maille gics 4, 0 sinon (gics 2)" 
  )




# Ajout de la table calculée et diffusée en 2021 (à partir de l'extract 2022)
co2stat_sc123_extract_2024 <- co2stat_sc123_2022 %>% 
  rename(
    int_sc12_EUR_2021 = int_sc12,
    int_sc3up_EUR_2021 = int_sc3up,
    int_sc3down_EUR_2021 = int_sc3down
  ) 

co2stat_sc123_extract_2022 <- co2stat_sc123_extract_2022 %>% 
  sjlabelled::var_labels(
    int_sc12_EUR_2021       = "intensité scope 1+2, 2021 (tCO2/M€ CA)",
    int_sc3up_EUR_2021      = "intensité scope 3 amont, 2021 (tCO2/M€ CA)",
    int_sc3down_EUR_2021    = "intensité scope 3 aval, 2021 (tCO2/M€ CA)",
  )

# Création du Excel

# exporter en format excel
dico_extract_2023 <- labelled::look_for(co2stat_sc123_extract_2023, details = "none")
dico_extract_2022 <- labelled::look_for(co2stat_sc123_extract_2022, details = "none")

l = 
  list(
    "intensités extract 2023"  = co2stat_sc123_extract_2023,
    "intensités extract 2022"  = co2stat_sc123_extract_2022,
    "descr. variables extract 2023" = dico_extract_2023, 
    "descr. variables extract 2022" = dico_extract_2022
  )






