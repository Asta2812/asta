# Empreinte carbone 2023
# Intensités
# import et exploration des données Reuters

#==============================================================================

# ___description___: 
# importe les données REUTERS/REFINITIV pour les années 2018 à 2022
# nettoie les données
# effectue un retraitement pour le scope 2
# fait différents lissages et traitements des valeurs extrêmes pour limiter la 
# volatilité lors de la construction de proxies sectoriels.

# ___inputs___: 
# MegaBaseTitre1906_original_added_mode_estimation_2021.csv       extract de reuters fourni par Paul Pally
# owid_elec_intens.csv                                            intensité de l'électricité fourni par OWID
# world_regions.csv                                               nomenclature Banque Mondiale des pays/régions (source OWID)


# __outputs__: 
# reuters_long                  données Reuters nettoyées et retraitées, au format "long" (.RDS et .csv)
# reuters_wide                  données Reuters nettoyées au format "wide" (.RDS)
# owid_elec_intens              intensités de l'électricité de OWID, nettoyée (.csv)
# world_regions                 nomenclature des regions, nettoyées (.csv)


#===============================================================================
getwd()
setwd("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta/data/")
# Setup                    
#here::i_am("./1-INTENSITES/script/import_reuters_2023.R")

library(readxl)
library(janitor)
library(skimr)
library(tidyverse)
library(tidylog)
library(here)
library(data.table)


# 1. Importer données et paramètres -----------------------------------------------

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


#save.image(here("./1-INTENSITES/temp/reuters.Rdata")) # à enlever 

# 2. Nettoyer données Reuters -----------------------------------------------------

#load(here("./1-INTENSITES/temp/reuters.Rdata"))  ==> # à elenver 

#Calcul du nombre de valeurs manquates dans la base Reuters 
library(skimr)
na <- skim(reuters) # données incomplète pour les émissions
hist(reuters$intensite_2023)
boxplot(reuters$intensite_2023)

reuters <- 
  # clean names
  clean_names(reuters)

reuters <- reuters %>%
  rename_with(~ str_replace(.x, "scope_", "scope"), contains('scope_'))

reuters <- 
  reuters %>%
  # clean gics_4 categories
  #rename(country = country_of_issuer) %>%
  mutate(gics_4 = str_to_lower(gics_4),
         gics_3 = str_to_lower(gics_3),
         gics_2 = str_to_lower(gics_2),
         gics_1 = str_to_lower(gics_1), 
         country = str_to_lower(country))

#reuters <- reuters %>% 
  # variable revenue_usd est au format au numérique (au lieu de caractère)
  #mutate(revenue_usd_11 = na_if(revenue_usd_11, "--"))  #remplacement de revenue_usd par revenue_usd_11

reuters <- reuters %>% 
  mutate_at(
    vars(contains('ca') | contains('double') | contains('total') | contains('scope') | contains("revenue")),
    as.double
  ) 

# Enlever total returns et sharpe 
reuters <- reuters %>%
  select(- contains('sharpe_'), -contains('returns')) #pas de changement

# Renommer les classifications Naics 

#reuters <- reuters %>% 
  #rename(
    #naics_industry_group = naics_idustry_group, 
    #naics_industry_group_code = naics_idustry_group_code,
    #trbc_business = trbc_buisness,
    #trbc_business_code = trbc_buisness_code
  #)

# Changer les chiffres d'affaire à 0 à valeur manquante
reuters <- reuters %>%
  mutate_at(
    vars(contains('ca'), revenue_usd_12),#remplacement de revenue_usd par revenue_usd_12
    ~ ifelse(. == 0, NaN, .)
  )

# Renomme le mode_estimation_2022
#reuters <- reuters %>% 
#  rename(
#    mode_estimation_2022 = report # la colonne report n'existe pas 
#  )

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


#save.image(here("./1-INTENSITES/temp/reuters_clean.Rdata")) ==> a remettre

# 3. Passage au format 'long' et nettoyage -------------------------------------

#load(here("./1-INTENSITES/temp/reuters_clean.Rdata"))


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

rm(temp)

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
           #naics_subsector_code, cette colonne n'existe pas 
           naics_industry_group,
           #naics_industry_group_code,cette colonne n'existe pas 
           naics_national_industry,
           #naics_national_industry_code, cette colonne n'exite pas 
           .after = code_gics_1)

reuters_long <- reuters_long %>% 
  relocate(trbc_economic_sector, 
           trbc_economic_sector_code,
           trbc_business,
           trbc_business_code,
           trbc_industry_group,
           trbc_industry_group_code
           #trbc_industry_34, cette colonne n'existe pas 
           #trbc_industry_35,
           #trbc_industry_code_39,
           #trbc_industry_code_40, 
           #.after = naics_national_industry_code
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

va <- skim(reuters) # nombre de valauers manquantes dans le colnnes numeriques 
rm(va) 

# Rectification de nomenclature GICS 4 dans Reuters
reuters_long <- reuters_long %>% 
  mutate(gics_2 = ifelse(gics_2 == "commercial  & professional services", 
                         "commercial & professional services", gics_2)) %>% 
  #select(-c(gics_4, gics_3, gics_2, gics_1, code_gics_3, code_gics_2, code_gics_1)) %>% 
  left_join(gics_table, by = "code_gics_4") %>% 
  relocate(
    #code_gics_1,
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

# Création d'une value de CA en MUSD
reuters_long <- reuters_long %>%
  mutate(
    ca_MUSD = ca/1e6
  )

# garder uniquement reporté
reuters_long <- reuters_long %>%
  filter(mode_estimation_2021 == "Reported" | mode_estimation_2022 == "Reported")


# Remplacement de l'intensite Reuters par l'intensité calculée                

reuters_long <- reuters_long %>%
  mutate(
    intensite = ifelse(
      !is.na(intensite_computed), 
      intensite_computed,
      intensite                    
    )
  )


save.image(here("./1-INTENSITES/temp/reuters_clean_long.Rdata"))


# 4. Nettoyage de données OWID  ------------------------------------------------

load(here("./1-INTENSITES/temp/reuters_clean_long.Rdata"))

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

mean_intens_dt <-  owid_dt[year %in% c(2019,2020,2021), 
                           .(intens_elec_mean = mean(intens_elec, na.rm = T)), 
                           by = country]

# ajouter labels
labelled::var_label(mean_intens_dt$intens_elec_mean) <-
  "intensité moyenne de l'électricité, 2019-2021 (gCO2/kWh)"

# ajouter code pays
mean_intens_dt <-  owid_elec_intens %>%  as.data.table() %>% 
  select(country, code, region) %>% distinct() %>% 
  merge(mean_intens_dt, all.y = TRUE, by = "country")


# Calculer le facteur de retraitement de l'électricité

intens_FR <- mean_intens_dt[country == "france", intens_elec_mean]
mean_intens_dt[,fact_retr := intens_elec_mean/intens_FR][]


save.image(here("./1-INTENSITES/temp/reuters_clean_long_retr.Rdata"))


# 7. Passage au format Long Est  -----------------------------------------------


load(here("./1-INTENSITES/temp/reuters_clean_long_retr.Rdata"))


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

save.image(here("./1-INTENSITES/temp/reuters_clean_long_winds.Rdata"))

# Retraitement du scope 2 à partir de données OWID  -----------------------------

load(here("./1-INTENSITES/temp/reuters_clean_long_winds.Rdata"))


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


#Possibilité d'aller jusqu'à la convergence : no changes : !check_changes(old_df, new_df) & iterate in a while pour aller au bout de la logique

save.image(here("./1-INTENSITES/temp/reuters_clean_long_winds_retr.Rdata"))

# conversion en euros  --------------------------------------------------

load(here("./1-INTENSITES/temp/reuters_clean_long_winds_retr.Rdata"))


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

taux_USD_EUR <- import_taux_USD_EUR(2017, taux_de_change) %>%             
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


# Exporter données FR -----------------------------------------------------

reuters_entreprises_FR <- reuters_long  %>% 
  filter(country == "france") %>% 
  select(name_or_code, ric, isin, year, code_gics_4, scope1, scope2, total, intensite, ca_MUSD, mode_estimation_2021, mode_estimation_2022)

write_csv2(reuters_entreprises_FR, here("./1-INTENSITES/output/reuters_entreprises_FR.csv"))

rm(reuters_entreprises_FR)

# sauvegarde données ---------------------------------------------

# données reuters au format long
saveRDS(reuters_long_est,    file = here("./1-INTENSITES/output/reuters_long.RDS"))

# données reuters nettoyées au format wide
saveRDS(reuters,    file = here("./1-INTENSITES/output/reuters_wide.RDS"))

# données OWID
saveRDS(owid_elec_intens, file = here("./1-INTENSITES/output/owid_elect_intens.RDS"))
saveRDS(world_regions, file = here("./1-INTENSITES/output/owid_wb_regions.RDS"))


unlink(here("./1-INTENSITES/temp/", "reuters*"))       # efface données temporaires