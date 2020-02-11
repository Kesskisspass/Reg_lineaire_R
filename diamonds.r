# Import des librairies
library(tidyverse)
library(dplyr)
library(ggplot2)

# Import données, transformation en facteur non ordonnés
diamonds <- ggplot2::diamonds %>% mutate_if(is.factor, ~ factor(., ordered = FALSE))

# Description des variables
str(diamonds)

summary(diamonds)

# On vérifie s'il existe des valeurs manquantes
diamonds %>% filter(is.na(carat) | is.na(cut) | is.na(color) | is.na(clarity) | is.na(depth) | is.na(table) | is.na(price) | is.na(x) | is.na(y) | is.na(z))

# Pas de valeur manquante (NA) par contre on constate qu'il existe des valeurs à 0 dans les champs x, y et z
summary(diamonds %>% filter(x == 0 | y ==0 | z == 0))
summary(diamonds %>% filter(x == 0 & y ==0 & z == 0))

# On va chercher à expliquer la variable price en fonction 
summary(lm(data = diamonds, price ~ carat + cut + color + clarity + depth + x))

# Plot price en fontion du carat et couleur en fonction de clarity
diamonds %>% ggplot() +
  geom_point(aes(x = carat, y = price, color = clarity))

# Impact couleur par rapport au prix du carat
d2 <- diamonds %>% group_by(color) %>% summarise(moyPrix = mean(price), moyCarat = mean(carat)) %>% mutate(impactCouleur = moyPrix/moyCarat) %>% arrange(desc(impactCouleur)) 
d2 %>% ggplot() + geom_col(aes(x = d2$color, y = d2$impactCouleur)) + 
  labs(title="Prix carat en fonction de la couleur",
        x ="Couleur", y = "Prix carat")


# Creation d'un echantillon test et entrainment
coeff = 0.7
train <- sample(seq_len(nrow(diamonds)), size = coeff * nrow(diamonds))
diamonds_train <- diamonds[train, ]
diamonds_test <- diamonds[-train, ]

# Regression linéaire
diamonds_train.lm <- lm(formula = price ~ carat + cut + clarity + color + depth + table + x,
                        data = diamonds)
summary(diamonds_train.lm)

# On rajoute les colonnes predict et residuals
diamonds$predict <- predict(diamonds_train.lm)
diamonds$residuals <- residuals(diamonds_train.lm)


# Evaluation du modèle avec les données de test
diamonds_test$price_predict <- predict(diamonds_train.lm, newdata = diamonds_test)
diamonds_compare <- diamonds_test %>% select(price, price_predict)

# Validation croisée
train.control <- caret::trainControl(method = "cv", number = 10)
diamonds_cv  <- caret::train(price ~ carat + cut + clarity + color + depth + table + x,
                             data = diamonds, 
                             method = "lm",
                             trControl = train.control)
print(diamonds_cv)
