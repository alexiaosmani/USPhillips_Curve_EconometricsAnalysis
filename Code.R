library(texreg)
library(FinTS)
library(tseries)
library(xts)
library(stargazer)
library(urca)
library(zoo)
library(xts)
library(aTSA)
library(forecast)

### MODÉLISATION UNIVARIÉE ###

### Taux de chômage aux États-Unis (UNRATE) ###

### a ###

data_UNRATE <- data.frame(read.csv("UNRATE-2.csv", sep=";"))
head(data_UNRATE)
data_UNRATE$date <- as.Date(data_UNRATE$observation_date, format = "%Y-%m-%d")
UNRATE <- xts(data_UNRATE["UNRATE"], order.by=data_UNRATE$date)
head(UNRATE)
n <- length(UNRATE)
print(n) #120 observations

### b ###
### Représentation graphique et corrélogrammes ###

plot(UNRATE) #probablement pas stationnaire
acf(UNRATE) #fonction d'autocorrélation simple décroit doucement vers 0
pacf(UNRATE) #nous ne pouvons rien conclure, nous procédons aux tests pour vérifier

plot(diff(UNRATE))

### c ###
### Tests de racine unitaire ###

# Modèle 3 : tendance déterministe et constante.
summary(ur.df(UNRATE, type="trend", selectlags="BIC"))
# t value = -0,897 > VC = -3,43 au seuil de 5% on ne peut pas rejetter H0
# Il y a probablement une racine unitaire.
# Testons si le modèle est bien spécifié
# On teste la significativité du coefficient de la tendance b
# abs(t-value) = 0,640 < VC = 3,12, on ne peut pas rejetter H0 au seuil de 5%
# b n'est pas significatif 

# Modèle 2 : avec constante et sans tendance.
summary(ur.df(UNRATE, type="drift", selectlags="BIC"))
# t value = -1,547 > -2,88, on ne peut pas rejetter H0 au seuil de 5%.
# La présence d'une racine unitaire semble confirmée.
# abs(t value) = 0,085 < 2,84, on ne peut pas rejetter H0 au seuil de 5%, 
# c n'est pas significatif donc le modèle est mak spécifié.

# Modèle 1 : sans tendance et sans constante.
summary(ur.df(UNRATE, type="none", selectlags="BIC"))
# t value = -4,938 < -1,95 au seuil de 5% on rejette H0
# Cela signifie que l'absence d'une racine unitaire est confirmée
# et que notre série est stationnaire.

# Par précaution, on teste les résultats de notre test ADF à l'aide des tests KPSS et Phillips-Perron: 

# test KPSS
summary(ur.kpss(UNRATE, type='mu', lags = "short"))
# t value = 2,4617 > 0,463 au seuil de 5%, on rejette H0
# Présence d'une racine unitaire confirmée, la série n'est pas stationnaire.
# Les conclusions ne sont pas alignées avec le test ADF sur le modèle 1

# test Phillips-Perron
summary(ur.pp(UNRATE, type="Z-tau", model="constant", lags = "short"))
# Z-tau = -1,6997 > VC = -2,885699, on ne peut pas rejetter H0 au seuil de 5%
# La série n'est pas stationnaire, cela confirme le résultat précédent.

# En raison des résultats des différents tests, nous préférons rester prudents et conclure 
# que la série n'est pas stationnaire.
# En effet, les tests KPSS et Phillips-Perron vont dans le même sens, 
# ce qui renforce notre conclusion sur la non stationnarité de la série.
# Nous allons stationnariser la série.

# À partir d'ici, on travaille avec la série différenciée : 
diff_UNRATE <- diff(UNRATE)
diff_UNRATE <- diff_UNRATE[-1, ]
plot(diff_UNRATE)
# Graphiquement, la série différentiée semble stationnaire.

# On refait la stratégie séquentielle pour cette série pour s'assurer qu'elle soit stationnaire.

### Test ADF ###

# Modèle 3 : avec constante et tendance déterministe
summary(ur.df(diff_UNRATE, type="trend", selectlags="BIC"))
# t value = -9,819 < -3,43 au seuil de 5% on rejette H0
# L'absence de racine unitaire est confirmée.
# On teste la spécification de notre modèle.
# abs(t value) = 1,831 < 3,12 on ne peut pas rejeter H0 au seuil de 5%,
# le coefficient b n'est pas significatif, le modèle est mal spécifié.


# Modèle 2 : avec constante et sans tendance
summary(ur.df(diff_UNRATE, type="drift", selectlags="BIC"))
# t value = -9,553 < VC = -2,88, au seuil de 5% on rejette H0
# L'absence de racine unitaire est confirmée.
# On teste la spécification de notre modèle.
# abs(t value) = 4,956 > 2,84, on rejette H0 au seuil de 5%
# Le coefficient c est significatif ; le modèle est bien spécifié.
# I(0) + c

# Vérifions les résultats de notre test ADF à l'aide des tests KPSS et PP

# Test KPSS
summary(ur.kpss(diff_UNRATE, type='mu', lags = "short"))
# t value = 0,3791 < VC = 0,463, au seuil de 5%, on ne peut pas rejeter H0,
# la série est stationnaire.

# Test PP
summary(ur.pp(diff_UNRATE, type="Z-tau", model="constant", lags = "short"))
# Z-tau = -14,1886 < VC = -2,89 au seuil de 5% on rejette H0
# la série est stationnaire.

# On en conclut que notre série différenciée est stationnaire, I(0) + c


### Indice des prix à la consommation aux États-Unis, PCEPI ###

### a ###

data_PCEPI <- data.frame(read.csv("PCEPI-2.csv", sep=";"))
head(data_PCEPI)
data_PCEPI$date <- as.Date(data_PCEPI$observation_date, format = "%Y-%m-%d")
PCEPI <- xts(data_PCEPI["PCEPI"], order.by=data_PCEPI$date)
head(PCEPI)
m <- length(PCEPI)
print(m) #120 observations

### b ###
### Représentation graphique et corrélogrammes ###

plot(PCEPI) # semble non stationnaire
acf(PCEPI) # décroît doucement vers 0, pas de termes MA
pacf(PCEPI) # Nous ne pouvons rien conclure ; nous procédons aux tests pour vérifier.

plot(diff(PCEPI))

### c ####
### Tests de RU ###

# Modèle 3 : avec tendance déterministe et constante.
summary(ur.df(PCEPI, type="trend", selectlags="BIC"))
# t value = -1,778 > VC = -3,43 au seuil de 5% on ne peut pas rejetter H0
# Il y a probablement une racine unitaire.
# Testons si le modèle est bien spécifié
# On teste la significativité du coefficient de la tendance b
# abs(t-value) = 1,752 < VC = 3,12, on ne peut pas rejetter H0 au seuil de 5%
# b n'est pas significatif, le modèle est mal spécifié.

# Modèle 2 : avec constante et sans tendance.
summary(ur.df(PCEPI, type="drift", selectlags="BIC"))
# t value = -0,304 > -2,88 ; on ne peut pas rejetter H0 au seuil de 5%.
# La présence d'une racine unitaire semble confirmée.
# abs(t value) = 0,565 < 2,84 ; on ne peut pas rejeter H0 au seuil de 5%. 
# c n'est pas significatif, le modèle est mal spécifié.

# Modèle 1 : sans tendance et sans constante.
summary(ur.df(PCEPI, type="none", selectlags="BIC"))
# t value = 5,0635 > -1,95, on ne peut pas rejetter H0 au seuil de 5%, 
# La présence d'une racine unitaire est semble confirmée.
# Il s'agit donc d'un processus I(1).

# Par précaution, on teste les résultats de notre test ADF à l'aide des tests KPSS et Phillips-Perron.

# test KPSS
summary(ur.kpss(PCEPI, type='mu', lags = "short"))
# t value = 2,3976 > 0,463 au seuil de 5%, on rejette H0,
# La présence d'une RU est possible, la série n'est pas stationnaire.

# test PP
summary(ur.pp(PCEPI, type="Z-tau", model="constant", lags = "short"))
# Z-tau = -0,1777 > VC = -2,886, on ne peut pas rejetter H0 au seuil de 5%
# La série n'est pas stationnaire.
# Cela confirme le résultat précédent, PCEPI est un processus I(1)

# Les résultats des tests ADF, KPSS et PP vont tous dans le même sens, ce qui renforce notre conclusion sur la non-stationnarité de la série.

# À partir d'ici, on travaille avec la série différenciée : 
diff_PCEPI <- diff(PCEPI)
diff_PCEPI <- diff_PCEPI[-1, ]
plot(diff_PCEPI) #la série différenciée semble stationnaire.

# On refait la stratégie séquentielle pour cette série pour s'assurer qu'elle soit stationnaire.

### Test ADF ###

# Modèle 3 : avec constante et tendance déterministe
summary(ur.df(diff_PCEPI, type="trend", selectlags="BIC"))
# t value = -6,442 < -3,43, au seuil de 5% on rejette H0
# L'absence de racine unitaire semble confirmée.
# On teste la spécification de notre modèle 
# abs(t value) = 0,035 < 3,12, on ne peut pas rejetter H0 au seuil de 5%
# Le coefficient b n'est pas significatif, le modèle est mal spécifié.

# Modèle 2 : avec constante et sans tendance
summary(ur.df(diff_PCEPI, type="drift", selectlags="BIC"))
# t value = -6,472 < VC = -2,88, au seuil de 5% on rejette H0
# L'absence de racine unitaire est confirmée.
# On teste la spécification de notre modèle :
# abs(t value) = 4,846 > 2,84, on rejette H0 au seuil de 5%
# Le coefficient c est significatif, le modèle est bien spécifié.
# La série est stationnaire I(0) + c

# Vérifions les résultats de notre test ADF à l'aide des tests KPSS et PP

# Test KPSS
summary(ur.kpss(diff_PCEPI, type='mu', lags = "short"))
# t value = 0,1362 < VC = 0,463, au seuil de 5%, on ne peut pas rejetter H0
# la série est stationnaire.

# Test PP
summary(ur.pp(diff_PCEPI, type="Z-tau", model="constant", lags = "short"))
# Z-tau = -7,335 < VC = -2,89 au seuil de 5% on rejette H0
# la série est stationnaire.

# On en conclut que notre série différenciée est stationnaire.

### d ###

# Représentation des ACF et PACF

acf(diff_PCEPI) #décroit rapidement vers 0
pacf(diff_PCEPI) # 1 retard significatif
 
# On ne peut pas conclure, donc on teste différents modèles pour voir lequel est le meilleur.

# On enlève les trois dernières obervations de PCEPI : 
PCEPI_petit <- PCEPI[1:(nrow(PCEPI)-3)]

# On teste si la série différentiée est un ARMA(1,1)
mod1 <- arima(PCEPI_petit, order = c(1,1,1))
mod1

# On teste si la série est un MA(1)
mod2 <- arima(PCEPI_petit, order = c(0,1,1))
mod2

# On teste si la série est un AR(1)
mod3 <- arima(PCEPI_petit, order = c(1,1,0))
mod3

# On teste si la série est un AR(2)
mod4 <- arima(PCEPI_petit, order = c(2,1,0))
mod4

# On teste si la série est un MA(2)
mod5 <- arima(PCEPI_petit, order = c(0,1,2))
mod5

# On teste si la série est un ARMA(2,1)
mod6 <-arima(PCEPI_petit, order = c(2,1,1))
mod6

# On teste si la série un ARMA(1,2)
mod7 <- arima(PCEPI_petit, order = c(1,1,2))
mod7


# On calcule les critères d'information.

# On calcule des critères AIC pour tous les modèles
AIC <- AIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
print(AIC)
# Le modèle 6 minimise le critère AIC

# On calcule des critères BIC pour tous les modèles
BIC <- BIC(mod1, mod2, mod3, mod4, mod5, mod6, mod7)
print(BIC)
# Le modèle 6 minimise le critère BIC

# Au vu des résultats des critères, on sélectionne le mod6 car il minimise les critères AIC et BIC
meilleur_modele <- mod6

# On teste la validité de notre modèle à travers plusieurs tests

### Test sur les paramètres ###
screenreg(meilleur_modele)
# pour ar1, t stat = 1,36/0,09 = 15,11 
# abs(t value) > 1,96 le coefficient est significatif
# pour ar2, t stat = -0,36/0,09 = -4, 
# Donc abs(t value)= 4 > 1,96 le coefficient est significatif
# pour ma1, t stat = -0,99/0,02 = -49,5
# abs(t value) > 1,96 le coefficient est significatif 

### Tests sur les résidus ###

checkresiduals(meilleur_modele)

res <- residuals(meilleur_modele) #on récupère les résidus
plot(res) 

# On regarde l'ACF et la PACF : 
acf(res)
pacf(res)
# corrélogrammes rassurants, proche de 0 

# test de Ljung-Box sur l'autocorrélation des résidus
Box.test(res, lag=10, type="Ljung-Box")
# p-value = 0,9411 > 5%, on ne peut pas rejetter H0
# pas d'autocorrélation des résidus

# test des effets d'ARCH
ArchTest(res, lag=10)
# p value = 0,5598 > 5%
# On ne rejette pas H0, absence d'effets d'ARCH (homoscédasticité)

#test de Jarque-Bera sur la normalité des résidus.
jarque.bera.test(res)
# p value = 0,004927 < 5%, les résidus ne suivent pas une loi normale
# C'est une limite de notre modèle qu'il faudra prendre en compte dans nos résultats

# Le modèle capture correctement la dynamique de la série et satisfait les principales hypothèses de validité statistique. 
# La non-normalité des résidus n’invalide pas nécessairement le modèle, dans la mesure où les estimateurs
# des paramètres restent convergents et sans biais sous des conditions plus générales.

# On vérifie que notre modèle est bien spécifié avec la constante
mod_drift <- Arima(PCEPI, order = c(2,1,1), include.constant = TRUE)
summary(mod_drift)
# On teste la significativité de la constante 
# t value = 0,1192/0,0163 = 7,31 > 1,96 la constante est significative.
# Notre modèle est bien spécifié.

### e ###
### Prévision ###

#on enlève les trois dernières dates
Date <- data.frame(data_PCEPI$date)
Date <- Date[1: (nrow(PCEPI)-3), ]

# Prévisions in-sample statiques (à un pas)

PCEPI_fitted_static <- xts(fitted(meilleur_modele), order.by=Date)
plot(PCEPI_fitted_static,  type='l', col="blue", ylab="Indice des prix", xlab="temps", main="prédictions in-sample statiques")
lines(PCEPI, col="red")
legend("bottomright", legend=c("prédictions", "observé"), col=c("blue", "red"), lty=1, xpd=TRUE)

#Prévisions out-of-sample : 

prev <- forecast::forecast(meilleur_modele, h=12, level= 90) #12 mois, à 90%
prev$mean #prevision 
prev$lower #borne basse de l'intervalle de prévision à 90%
prev$upper #borne haute de l'intervalle de prévision à 90%

#Pas besoin de recolorer le processus car on a fait nos prévisions sur 
#la série PCEPI non différenciée
plot(prev, main="Prévision out-of-sample ARIMA pour PCEPI")

### MODELISATION MULTIVARIEE : VAR ####

library(vars)
library(tseries)
library(urca)
library(lmtest)
library(ggplot2)
library(dplyr)

# On construit le vecteur de séries stationnarisées
data_var <- cbind(diff_PCEPI, diff_UNRATE)
colnames(data_var) <- c("diff_PCEPI", "diff_UNRATE")

# Visualisation
ts.plot(data_var, col = c("blue", "red"), lty = 1:2,
        main = "Séries stationnarisées : diff_PCEPI et diff_UNRATE")
legend("topright", legend = c("diff_PCEPI", "diff_UNRATE"),
       col = c("blue", "red"), lty = 1:2, cex = 0.8)

# Les deux séries oscillent autour de zéro, sans tendance nette, ce qui traduit une stabilité à long terme. 
# Économiquement, cela indique que les chocs sur l’inflation et le chômage sont temporaires et se corrigent avec le temps.

### a ###

#Sélection du nombre de retards 

# On recherche le lag optimal pour le modèle VAR
lagselect <- VARselect(data_var, lag.max = 10, type = "const")
lagselect$criteria

# Sélection du lag optimal selon les critères AIC
p_opt <- lagselect$selection[["AIC(n)"]]
cat("Lag optimal retenu :", p_opt, "\n")
# On choisit p = 1 car c'est le nombre de retard qui minimise le critère AIC.

# Estimation du modèle VAR avec p optimal

# Estimation du modèle VAR
var_model <- VAR(data_var, p = p_opt, type = "const")
summary(var_model)
# Le modèle inclut une constante et une seule période de retard pour les deux variables.

# Résultats pour l’équation de l’inflation
# Le coefficient de diff_PCEPI.l1 est positif et significatif, montrant que l'inflation dépend fortement de ses valeurs passées.
# L'inflation met du temps à se stabiliser après un choc, elle persiste dans le temps avant de revenir à son équilibre
# Le coefficient de diff_UNRATE.l1 n’est pas significatif, indiquant que le chômage n’a pas d’effet direct sur l’inflation à court terme.
# La constante positive traduit une légère tendance haussière de l’inflation en moyenne.

# Résultats pour l’équation du chômage
# Le coefficient de diff_UNRATE.l1 est négatif et significatif, suggérant un retour progressif du chômage vers sa moyenne.
# Le coefficient de diff_PCEPI.l1 est non significatif, ce qui montre une absence d’impact direct de l’inflation sur le chômage.
# La constante négative révèle une tendance moyenne à la baisse du chômage sur la période étudiée.

# Interprétation économique globale
# Le modèle est stable, les racines étant inférieures à 1 en module.
# Les résidus sont quasi indépendants, confirmant la bonne spécification du modèle.
# Les interactions entre inflation et chômage sont faibles à court terme, 
# indiquant que la courbe de Phillips n’est pas clairement observée sur cette période.

# Vérification de la stabilité du VAR
stability <- roots(var_model, modulus = TRUE)
if(all(stability < 1)){
  cat("Le modèle VAR est stable : toutes les racines sont dans le cercle unité.\n")
} else {
  cat("Attention : certaines racines sont en dehors du cercle unité !\n")
}

# Diagnostic des résidus

# Test d'autocorrélation
serial_test <- serial.test(var_model, lags.pt = 12, type = "PT.asymptotic")
serial_test
# p-value = 0,3224 > 0,05, on ne peut pas rejetter H0 au seuil de 5%, 
# il n'y a pas d'autocorrélation des résidus.

# Test d'hétéroscédasticité
arch_test <- arch.test(var_model, lags.multi = 12, multivariate.only = TRUE)
arch_test
# p-value = 0,7559 > 0,05, au seuil de 5% on ne peut pas rejetter H0.
# Les résidus ne présentent aucun effet d'ARCH significatif, la variance des résidus est stable dans le temps.

# Test de normalité de Jarque Bera 
norm_test <- normality.test(var_model, multivariate.only = TRUE)
norm_test
# Skewness : p-value = 0,1132 > 0,05
# Kurtosis : p-value = 0,07271 > 0,05
# Au seuil de 5%, on ne peut pas rejetter H0
# Les résidus suivent une distribution proche de la normale.

### b ###
# Test de causalité de Granger 

# H0 : la variable exclue ne cause pas la variable testée
# Causalité de l'inflation vers le chômage : 
granger_PCEPI <- causality(var_model, cause = "diff_PCEPI")
cat("Causalité (PCEPI -> UNRATE) :\n")
print(granger_PCEPI$Granger)
# p-value = 0,3186 > 0,05, on ne rejette pas H0 au seuil de 5%
# Pas de causalité de l'inflation vers le chômage

# Causalité du chômage vers l'inflation : 
granger_UNRATE <- causality(var_model, cause = "diff_UNRATE")
cat("\nCausalité (UNRATE -> PCEPI) :\n")
print(granger_UNRATE$Granger)
# p-value = 0,199 > 0,05, on ne rejette pas H0 au seuil de 5%
# Pas de causalité du chômage vers l'inflation.

# Conclusion
# Aucune des deux relations n’est significative au sens de Granger.
# Les deux séries n’ont pas de relation causale directe à court terme.
# Cette conclusion remet en question la courbe de Phillips traditionnelle.

### c ###

# Analyse Impulsion-Réponse des chocs 

# On place les variables de la plus exogène à la endogène
# Ici : le chômage (diff_UNRATE) réagit lentement aux prix et reflète avant tout les conditions réelles. 
# Les ajustements d’emploi étant inertiels, un choc d’inflation n’a pas d’effet immédiat sur le chômage. 
# À l’inverse, l’inflation est plus endogène : les prix s’ajustent rapidement aux tensions sur le marché du travail.
# On testera également d’autres ordres pour vérifier la robustesse des résultats.

# Modèle avec ordre cohérent (UNRATE -> PCEPI)
var_model_ordre <- VAR(data_var[, c("diff_UNRATE", "diff_PCEPI")], p = 1, type = "const")

# Modèles inversés pour tester la sensibilité à l’ordre des variables
var_model_desordre1 <- VAR(data_var[, c("diff_PCEPI", "diff_UNRATE")], p = 1, type = "const")

# Représentation des IRF

# Réponse de l’inflation à un choc de chômage
irf_unrate_to_pcepi <- irf(var_model_ordre, impulse = "diff_UNRATE",
                           response = "diff_PCEPI",
                           n.ahead = 10, boot = TRUE)
plot(irf_unrate_to_pcepi)
# La réponse de l’inflation est très faible et non significative.
# L’inflation ne réagit quasiment pas à un choc sur le chômage.
# Ce résultat confirme l’absence de causalité de Granger du chômage vers l’inflation.


# Réponse du chômage à un choc d’inflation
irf_pcepi_to_unrate <- irf(var_model_ordre, impulse = "diff_PCEPI",
                           response = "diff_UNRATE",
                           n.ahead = 10, boot = TRUE)
plot(irf_pcepi_to_unrate)
# Le chômage ne réagit pas de manière significative.
# L’effet est proche de zéro et se dissipe rapidement, cohérent avec le test de Granger
# montrant que l’inflation ne cause pas le chômage.

#Interprétation économique
# L’absence de réaction croisée confirme que la courbe de Phillips n’est pas observée à court terme :
# pas de relation inverse inflation-chômage significative sur les données différenciées.

### d ###

# Relation de cointégration entre le niveau des prix et le taux de chômage.

# Étape 1 du test Engle–Granger : relation de long terme et résidus

reg_LT <- lm(PCEPI ~ UNRATE, data = data_PCEPI)  
print(summary(reg_LT))

resid_EG <- reg_LT$residuals

ggplot(data_PCEPI, aes(x = date, y = resid_EG)) +
  geom_line(color = "darkblue") +
  labs(title = "Résidus de la relation long terme (PCEPI ~ UNRATE)",
       y = "Résidu", x = "Date") +
  theme_minimal()

# Les résidus de la relation ne sont pas stationnaires : la série dérive nettement après 2015 
# et ne revient pas autour de zéro. 
# Cela indique l’absence de cointégration entre les deux variables sur la période étudiée.


# Étape 2 : test de stationnarité des résidus

print(po.test(cbind(PCEPI, UNRATE)))   
# p value = 0,15 > 0,05 il n'existe pas de relation de cointégration entre les deux variables.

# Conclusion : 
# Le test Phillips–Ouliaris ne valide pas l’hypothèse de cointégration 
# entre l’inflation (PCEPI) et le chômage (UNRATE).
# En conséquence, il n’est pas pertinent d’estimer un modèle ECM, 
# puisqu’il n’existe pas de relation de long terme à corriger.
# Économiquement, cela signifie qu'il n'existe pas de relation de long terme stable
# entre l'inflation (PCEPI) et le chômage (UNRATE) sur la période étudiée.
# Les deux variables évoluent indépendamment à long terme : absence de cointégration.