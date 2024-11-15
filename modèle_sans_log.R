setwd("C:/Users/M22527/Bpifrance/Raphael COTTIN - 2024-04_intensites_Asta")
getwd()

library(tidyverse)
library(tidylog)
library(dplyr)
library(readxl)
library(skimr)
library(glmnet)
library(caret)
library(recipes) 

# chargement dataframe reuters et intensités statistiques ----------------------

reuters24 <- read_xlsx("data/reuters_hybr_2024.xlsx")

load("temp/reuters_temp.Rdata")


# pré-traitement des données Reuters 2024  --------------------------------------------------
# sélection gics2bis et med_int_2023_from_2024(intens23)

reuters24 <- reuters24 %>% 
  select(gics2bis, intens23 = med_int_2023_from_2024)


# recréer la variable gics2bis
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


reuters_long_est <- reuters_long_est %>% 
  # création d'une nomenclature hybride aux niveaux gics 4 et gics 2
  mutate(
    gics2bis = case_when(
      gics_4 %in% sect_intens  ~  gics_4,
      TRUE                     ~  gics_2)
  )


# Sélection de léchantillon 2023 

reuters_2023 <- reuters_long_est %>%  
  filter(year == 2023)

# Vérification des doublons
reuters_2023 %>%  
  group_by(name_or_code) %>%  
  filter(n() > 1)



# Suppression des colonnes dans reuters 2023

reuters_2023 <- 
  reuters_2023 %>% 
  select(-c(isin, nace, nace_code, naics_industry_group, naics_international,
            naics_subsector, naics_sector, naics_national_industry, scope3, 
            intensite_computed, currency, ric, year)) 

reuters_2023 <- 
  reuters_2023 %>%
  select(-c(intens_sc1, intens_sc2, 
            intens_sc2_w, intens_sc1_w, scope2, scope1, ca))


reuters_2023 <-
  reuters_2023 %>% 
  select(-c(gics_1, code_gics_1,code_gics_2,code_gics_3,code_gics_4,
            trbc_business_code,trbc_industry_code_37,gics_2, gics_3,
            gics_4, trbc_economic_sector_code,trbc_industry_group_code,
            trbc_industry_code_38))




reuters_2023 <-
  reuters_2023 %>% 
  select(-c(trbc_sector, trbc_economic_sector, trbc_business, trbc_industry_group,
            trbc_industry_32, trbc_industry_33, name_or_code))



reuters_2023 <-
  reuters_2023 %>% 
  select(-c(code_gics_table))



# création d'une variable "intensité bis" cohérente avec les chiffres de revenu

reuters_2023 <- reuters_2023 %>% 
  mutate(revenue_Musd = revenue_usd /1e6, 
         intensite_bis = total / revenue_Musd) %>% 
  select(-revenue_usd, -intensite, -contains("code_gics"))

# # Création un vecteur de pays et leurs continents correspondants
continent <- c(
  "singapore" = "Asia", "thailand" = "Asia", "kenya" = "Africa", 
  "united kingdom" = "Europe", "united states of america" = "North America",
  "australia" = "Oceania", "canada" = "North America", "philippines" = "Asia",
  "hong kong" = "Asia", "oman" = "Asia", "italy" = "Europe", "brazil" = "South America",
  "saudi arabia" = "Asia", "lithuania" = "Europe", "china" = "Asia", 
  "india" = "Asia", "bahrain" = "Asia", "qatar" = "Asia", "germany" = "Europe", 
  "poland" = "Europe", "japan" = "Asia", "mexico" = "North America", 
  "norway" = "Europe", "south africa" = "Africa", "russia" = "Europe/Asia", 
  "egypt" = "Africa", "chile" = "South America", "finland" = "Europe", 
  "united arab emirates" = "Asia", "sweden" = "Europe", "switzerland" = "Europe", 
  "korea" = "Asia", "belgium" = "Europe", "indonesia" = "Asia", 
  "new zealand" = "Oceania", "malaysia" = "Asia", "argentina" = "South America", 
  "kuwait" = "Asia", "france" = "Europe", "austria" = "Europe", 
  "turkey" = "Europe/Asia", "netherlands" = "Europe", "denmark" = "Europe", 
  "romania" = "Europe", "colombia" = "South America", "slovenia" = "Europe", 
  "morocco" = "Africa", "cyprus" = "Europe", "taiwan" = "Asia", 
  "luxembourg" = "Europe", "israel" = "Asia", "spain" = "Europe", 
  "greece" = "Europe", "hungary" = "Europe", "portugal" = "Europe", 
  "ireland" = "Europe", "cambodia" = "Asia", "uruguay" = "South America", 
  "malta" = "Europe", "vietnam" = "Asia", "iceland" = "Europe", 
  "peru" = "South America", "ukraine" = "Europe", "kazakhstan" = "Asia", 
  "nigeria" = "Africa", "jordan" = "Asia", "palestine" = "Asia", 
  "czechia" = "Europe", "mauritius" = "Africa", "pakistan" = "Asia", 
  "panama" = "North America", "sri lanka" = "Asia", "mongolia" = "Asia"
)

# Création de la colonne "region" correspondant au continent des différents pays

reuters_2023 <- reuters_2023 %>%
  mutate(Region = continent[country])


# Suppression des valeurs manquantes dans reuters 2023

str(reuters_2023)
colnames(reuters_2023)

reuters_2023 <-
  reuters_2023 %>% 
  na.omit()


# partitionner la base reuters en test vs apprentissage  -----------------------

nb <- 10
set.seed(1234)
bloc <- sample(rep(1:nb,length=nrow(reuters_2023)))
table(bloc)
reuters_test <- reuters_2023[bloc == 1,]
reuters_appr <- reuters_2023[bloc != 1,]


# Estimation des médianes sectorielles

# Réestimation des médianes sectorielles dans la base d'apprentissage
intens_gics2bis_appr <- reuters_appr %>%
  group_by(gics2bis) %>%
  summarise(intens_med_gics2bis = median(intensite_bis))

# Réestimation des médianes sectorielles dans la base test
intens_gics2bis_test <- reuters_test %>%
  group_by(gics2bis) %>%
  summarise(intens_med_gics2bis = median(intensite_bis))

# Attribution des intensités médianes pour la base test tests

reuters_test_mediane <- reuters_test %>%
  left_join(intens_gics2bis_test, by = "gics2bis") %>%
  mutate(total_prev_mediane = intens_med_gics2bis * revenue_Musd)

# Attribution des intensités médiane pour la base d'apprentissage

reuters_appr_mediane <- reuters_appr %>% 
  left_join(intens_gics2bis_appr, by="gics2bis") %>% 
  mutate(total_prev_mediane = (intens_med_gics2bis * revenue_Musd))

# Dans la base reuters 2023

total_reuters <- reuters_2023 %>% 
  left_join(intens_gics2bis_appr, by="gics2bis") %>% 
  mutate(total_prev = intens_med_gics2bis * revenue_Musd)

# Calcul du RMSE du modèle par la médiane dans la base d'apprentissage

RMSE_appr_mediane <-  reuters_appr_mediane%>% 
  mutate(total_prev = intens_med_gics2bis * revenue_Musd)

# calcul du rmse pour la base d'apprentissage 

RMSE_appr <-
  RMSE_appr_mediane %>%  
  mutate(
    erreur_mediane = (total - total_prev)^2  # 15437310.
  ) %>%  
  summarise(RMSE_mediane = sqrt(mean(erreur_mediane, na.rm = TRUE)))

RMSE_test <-
  reuters_test_mediane %>% 
  mutate(
    erreur_mediane = (total - total_prev_mediane)^2  # 
  ) %>%  
  summarise(RMSE_mediane = sqrt(mean(erreur_mediane, na.rm = TRUE))) #3311545.


# ML sous le modèle de regression linéaire par validation croisée avec l'echantillon d'apprentissage
unique(reuters_2023$Region)

set.seed(1234)
# Conversion des variables categorielles en facteur dans la base d'apprentissage et dans la base de test

reuters_appr_ols <- reuters_appr %>% 
  mutate(Region = as.factor(Region),
         gics2bis = as.factor(gics2bis))

reuters_test_ols <- reuters_test %>% 
  mutate(Region = as.factor(Region),
         gics2bis = as.factor(gics2bis))

# Application de l'ols par la validation croisée
cv_model_ols <- train(
  total ~ revenue_Musd + Region + gics2bis ,
  data = reuters_appr_ols,
  method = "lm",
  trControl = trainControl(method = "cv", number = 10)
)

summary(cv_model_ols)

# Évaluation de la prédiction sous l'ensemble des données de test
prediction_ols <- predict(cv_model_ols, newdata = reuters_test_ols) 

# Calcul du RMSE du modèle ols (en log) 
rmse <- sqrt(mean((reuters_test_ols$total - prediction_ols)^2)) 
print(paste("RMSE: ", rmse)) #5427724.

# Calcul du MAE
mae_ols <- mean(abs(reuters_test_ols$total - prediction_ols)) #2623172

# ML sous des modèles de regressions pénalisées
# Spécification de la matrice des variables explicatives et la variable cible dans le BD d'apprentissage

X_train <- model.matrix(total ~ revenue_Musd + gics2bis + Region, reuters_appr)[, -1]
Y_train <- reuters_appr$total

# Spécification de la matruice des variable explicative et de la variable cible dans la BD de test

X_test <- model.matrix(total ~ revenue_Musd + gics2bis + Region, reuters_test)
Y_test <- reuters_test$total


# Application de la regression Ridge
ridge <- cv.glmnet(
  x = X_train,
  y = Y_train,
  alpha = 0
)



# 
ridge$lambda %>% head() # les termes de penalités associé à la regression Ridge

# la meilleur valeur de lambda
best_lambda_ridge <- ridge$lambda.min


# Faire des prédictions sur l'ensemble de test 
ridge_pred <- predict(ridge, s = best_lambda_ridge, newx = X_test) 


# Calcul le RMSE sur la base de test 

ridge_rmse <- sqrt(mean((Y_test - ridge_pred)^2)) 
print(paste("RMSE: ", ridge_rmse)) # 9842363760.

# Calcul du MAE de la regrssion Ridge
mae_ridge <- mean(abs(reuters_test$total - ridge_pred )) #3793229624


# Application de la regression Lasso  avec validation croisée

lasso <- cv.glmnet(X_train, Y_train, alpha = 1, family = "gaussian")

# la meilleure valeur de lambda
lambda_lasso <- lasso$lambda.min

# prédictions sur l'ensemble de test
lasso_pred <- predict(lasso, s = lambda_lasso, newx = X_test)

# Calcul du RMSE sur la BD de test
lasso_rmse <- sqrt(mean((Y_test - lasso_pred)^2)) #4827903849
print(paste("RMSE: ", lasso_rmse))

# Calcul du MAE de la regression lasso
mae_lasso <- mean(abs(reuters_test$total - lasso_pred )) # 1864119417


# Application de l'elastic net

elastic_net <- cv.glmnet(X_train, Y_train, alpha = 0.5, family = "gaussian")

# la meilleure valeur de lambda
lambda_en <- elastic_net$lambda.min

print(paste("Best lambda: ", lambda_en)) #0.006

# Faire des prédictions sur l'ensemble de test 
elastic_net_pred <- predict(elastic_net, s = lambda_en, newx = X_test) 

# Calcul du RMSE 
elastic_net_rmse <- sqrt(mean((Y_test - elastic_net_pred)^2)) 
print(paste("RMSE: ", elastic_net_rmse)) # 2926146249.3

# Calcul du MAE de la regression EN
mae_en <- mean(abs(reuters_test$total - elastic_net_pred )) # 1130760498

