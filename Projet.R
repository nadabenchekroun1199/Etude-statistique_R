---
  title: "Projet de R�gression Lin�aire "
author: "Nada BENCHEKROUN"
 
output: html_document
---
  
  
  ## Charger les packages
  
  Dans ce projet, nous allons explorer les donn�es en utilisant le package `dplyr` et visualiser cela en utilisant le package `ggplot2` pour visualiser les donn�es. Les donn�es peuvent �tre trouv�es dans la package `statsr`.

```{r load-packages, message=FALSE, warning=FALSE}
library(dplyr)
library(ggplot2)
library(statsr)
```

## Les donn�es

Nous consid�rons les donn�es de The World FactBook de 2015 produite par la CIA annuellement et contient des informations sur chaque pays du monde sur l'aspect g�ographique, d�mographique, politique, �conomique, communication ou militaire. L'ensemble de donn�es comprend 279 pays (et entit�s) et 75 variables. Nous voulons effectuer une �tude observationnelle et �tablir des corr�lations via la r�gression lin�aire. Chargeons les donn�es :
  
  ```{r load-data}
WorldFactBook <- read.csv(file="WorldFactBook.csv")
dim(WorldFactBook)
```

## R�gression lin�aire

L'�quation de la droite de r�gression est obtenue par la m�thode des moindres carr�s qui calcule les valeurs $\hat{a}$ et $\hat{b}$ de a et b tel que : 
  \[\hat{a} = \frac{Cov(x,y)}{Var(x)} = \frac{\sum_i (x_i-\bar{x})(y_i-\bar{y})}{\sum_i (x_i-\bar{x})^2}\] et
\[\hat{b} = \bar{y}-\hat{a}\bar{x} = \frac{1}{n}\sum_i y_i - \hat{a}\sum_i x_i\]
Gr�ce � la droite de r�gression lin�aire, il est possible de pr�voir une tendance pour une valeur donn�e x. De plus, l'outil calcule le coefficient de corr�lation selon :
  \[Corr(x,y) = \frac{Cov(x,y)}{\sigma_x \sigma_y}\]
\[r = (Corr(x,y))^2\]
o� $\sigma$ est l'�cart type.

Le coefficient de corr�lation que l'on note r varie entre -1 et +1. Plus sa valeur absolue est proche de 1, plus le mod�le lin�aire est fiable et d�crit correctement la r�alit�.

## 1�re �tude : Le taux de naissance est-il en corr�lation avec le taux de croissance de la population ?

```{r Taux_naissance-Taux_croissance_population}
plot(WorldFactBook$Birth.rate, WorldFactBook$Population.growth.rate, main="Taux de naissance en fonction du taux de croissance de la population", xlab="Taux de naissance", ylab="Taux de croissance de la population")
```

On peut voir que le nuage de point suit assez bien une droite.
On va donc calculer les coefficients a et b.

```{r Taux_naissance-Taux_croissance_population-coefficients_a_b}
plot(WorldFactBook$Birth.rate, WorldFactBook$Population.growth.rate, main="Corr�lation entre le taux de naissance et le taux de croissance de la population", xlab="Taux de naissance", ylab="Taux de croissance de la population")

n = 279 # Nombres de variables trouv� avec dim(World_FactBook).

# Calcul des coefficients a et b.
a = cov(WorldFactBook$Birth.rate, WorldFactBook$Population.growth.rate, use="na.or.complete")/var(WorldFactBook$Birth.rate, na.rm=TRUE)
b = (1/n)*sum(WorldFactBook$Population.growth.rate, na.rm=TRUE) - a*(1/n)*sum(WorldFactBook$Birth.rate, na.rm=TRUE)
paste("Coefficient a : ", a)
paste("Coefficient b : ", b)

# Equation de la droite.
y = a*WorldFactBook$Birth.rate+b

# Ajout de la droite de r�gression au graphique.
lines(y~WorldFactBook$Birth.rate, type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(5.2,4.2,leg,col='red', lty=1,cex = 0.8)
```

```{r Taux_naissance-Taux_croissance_population-coefficient_lin�aire}
# Calcul de la corr�lation.
correlation = cov(WorldFactBook$Birth.rate, WorldFactBook$Population.growth.rate, use="na.or.complete")/(sd(WorldFactBook$Birth.rate, na.rm=TRUE)*sd(WorldFactBook$Population.growth.rate, na.rm=TRUE))
# Calcul du coefficient de corr�lation lin�aire.
coefficient = correlation^2
paste("Corr�lation :", correlation)
paste("Coefficient de corr�lation lin�aire : ", coefficient)
```

**Nous v�rifions nos caluls avec les formules pr�d�finies de R :**

```{r Taux_naissance-Taux_croissance_population-formules_R}
summary(lm(WorldFactBook$Population.growth.rate ~ WorldFactBook$Birth.rate, data = WorldFactBook))
```

Nous observons que nos valeurs calculatoires sont assez proches des valeurs calcul�es par la fonction R.

Maintenant, nous calculons l'intervalle de confiance gr�ce � :
  \[\bar{x} \pm z^\star \frac{\sigma}{\sqrt{n}}\] o� $z^\star$ est la valeur critique � droite, $\sigma$ est l'�cart type et $\bar{x}$ est la moyenne.

```{r Taux_naissance-Taux_croissance_population-intervalle_confiance}
plot(WorldFactBook$Birth.rate, WorldFactBook$Population.growth.rate, main="Corr�lation entre le taux de naissance et le taux de croissance de la population", xlab="Taux de naissance", ylab="Taux de croissance de la population")
lines(y~WorldFactBook$Birth.rate, type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(5.2,4.2,leg,col='red', lty=1,cex = 0.8)

# On calcule la valeur critique pour un intervalle de confiance de 95%.
z_star_95 <- qnorm(0.975)
paste("Valeur critique pour un intervalle de confiance de 95% : ", z_star_95)

# Calcul des intervalles :
inf = mean(WorldFactBook$Birth.rate, na.rm=TRUE) - z_star_95 * (sd(WorldFactBook$Birth.rate, na.rm=TRUE) / sqrt(n))
sup = mean(WorldFactBook$Birth.rate, na.rm=TRUE) + z_star_95 * (sd(WorldFactBook$Birth.rate, na.rm=TRUE) / sqrt(n))
paste("Valeur minimale : ", inf)
paste("Valeur maximale : ", sup)

# L'erreur :
  erreur = (sup-inf)/2
paste("Erreur : ", erreur)

# Nos 2 droites d'intervalle de confiance.
y_sup = y+erreur
y_inf = y-erreur
# Ajout des droites d'intervalle de confiance au graphique.
lines(y_sup~WorldFactBook$Birth.rate, type='l',col='green')
lines(y_inf~WorldFactBook$Birth.rate, type='l',col='green')
```

**Conclusion de la 1�re �tude**
  Nous avons un coefficient pour la droite de r�gression qui est d'environ 0.6. Cela nous permet de dire que la qualit� de la regression est assez bonne. Nous pouvons �galement dire, gr�ce au graphe de la droite que le taux de naissance est assez corr�l� avec le taux de croissance de la population (pente non nulle de la droite).

## 2�me �tude : Le PIB par habitant est-il en corr�lation avec le taux de naissance ?

```{r PIB_par_habitant-Taux_naissance}
plot(WorldFactBook$GDP...per.capita..PPP., WorldFactBook$Birth.rate, main="PIB par habitant en fonction du taux de naissance", xlab="PIB par habitant", ylab="Taux de naissance")
```

On peut voir que le nuage de point suit assez bien une droite.
On va donc calculer les coefficients a et b.

```{r PIB_par_habitant-Taux_naissance-coefficients_a_b}
plot(WorldFactBook$GDP...per.capita..PPP., WorldFactBook$Birth.rate, main="Corr�lation entre le PIB par habitant et le taux de naissance", xlab="PIB par habitant", ylab="Taux de naissance")

n = 279 # Nombres de variables trouv� avec dim(World_FactBook).

# Calcul des coefficients a et b.
a = cov(WorldFactBook$GDP...per.capita..PPP., WorldFactBook$Birth.rate, use="na.or.complete")/var(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE)
b = (1/n)*sum(WorldFactBook$Birth.rate, na.rm=TRUE) - a*(1/n)*sum(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE)
paste("Coefficient a : ", a)
paste("Coefficient b : ", b)

# Equation de la droite.
y = a*WorldFactBook$GDP...per.capita..PPP.+b

# Ajout de la droite de r�gression au graphique.
lines(y~WorldFactBook$GDP...per.capita..PPP., type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(0,46.7,leg,col='red', lty=1,cex = 0.8)
```

```{r PIB_par_habitant-Taux_naissance-coefficient_lin�aire}
# Calcul de la corr�lation.
correlation = cov(WorldFactBook$GDP...per.capita..PPP., WorldFactBook$Birth.rate, use="na.or.complete")/(sd(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE)*sd(WorldFactBook$Birth.rate, na.rm=TRUE))
# Calcul du coefficient de corr�lation lin�aire.
coefficient = correlation^2
paste("Corr�lation :", correlation)
paste("Coefficient de corr�lation lin�aire : ", coefficient)
```

**Nous v�rifions nos caluls avec les formules pr�d�finies de R :**
  
  ```{r PIB_par_habitant-Taux_naissance-formules_R}
summary(lm(WorldFactBook$Birth.rate ~ WorldFactBook$GDP...per.capita..PPP., data = WorldFactBook))
```

Nous observons que nos valeurs calculatoires sont assez proches des valeurs calcul�es par la fonction R.

Maintenant, nous calculons l'intervalle de confiance gr�ce � :
\[\bar{x} \pm z^\star \frac{\sigma}{\sqrt{n}}\] o� $z^\star$ est la valeur critique � droite, $\sigma$ est l'�cart type et $\bar{x}$ est la moyenne.

```{r PIB_par_habitant-Taux_naissance-intervalle_confiance}
plot(WorldFactBook$GDP...per.capita..PPP., WorldFactBook$Birth.rate, main="Corr�lation entre le PIB par habitant et le taux de naissance", xlab="PIB par habitant", ylab="Taux de naissance")
lines(y~WorldFactBook$GDP...per.capita..PPP., type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(0,46.7,leg,col='red', lty=1,cex = 0.8)

# On calcule la valeur critique pour un intervalle de confiance de 95%.
z_star_95 <- qnorm(0.975)
paste("Valeur critique pour un intervalle de confiance de 95% : ", z_star_95)

# Calcul des intervalles :
inf = mean(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE) - z_star_95 * (sd(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE) / sqrt(n))
sup = mean(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE) + z_star_95 * (sd(WorldFactBook$GDP...per.capita..PPP., na.rm=TRUE) / sqrt(n))
paste("Valeur minimale : ", inf)
paste("Valeur maximale : ", sup)

# L'erreur :
erreur = (sup-inf)/2
paste("Erreur : ", erreur)

# Nos 2 droites d'intervalle de confiance.
y_sup = y+erreur
y_inf = y-erreur
# Ajout des droites d'intervalle de confiance au graphique.
lines(y_sup~WorldFactBook$GDP...per.capita..PPP., type='l',col='green')
lines(y_inf~WorldFactBook$GDP...per.capita..PPP., type='l',col='green')
```

**Conclusion de la 2�me �tude**
  Nous avons un coefficient pour la droite de r�gression qui est d'environ 0.3. Cela nous permet de dire que la qualit� de la regression n'est pas bonne. Nous pouvons donc dire que le PIB par habitant n'est pas corr�l� avec le taux de naissance.

## 3�me �tude : Les d�penses dans l'�ducation sont-elle en corr�lation avec les d�penses dans la sant� ?

```{r Depenses_education_Depense_sante}
plot(WorldFactBook$Education.expenditures, WorldFactBook$Health.expenditures, main="D�penses dans l'�ducation en fonction des d�penses dans la sant�", xlab="D�penses dans l'�ducation", ylab="D�penses dans la sant�")
```

On peut voir que le nuage de point suit assez bien une droite.
On va donc calculer les coefficients a et b.

```{r Depenses_education_Depense_sante-coefficients_a_b}
plot(WorldFactBook$Education.expenditures, WorldFactBook$Health.expenditures, main="Corr�lation entre les d�penses dans l'�ducation et les d�penses dans la sant�", xlab="D�penses dans l'�ducation", ylab="D�penses dans la sant�")

n = 279 # Nombres de variables trouv� avec dim(World_FactBook).

# Calcul des coefficients a et b.
a = cov(WorldFactBook$Education.expenditures, WorldFactBook$Health.expenditures, use="na.or.complete")/var(WorldFactBook$Education.expenditures, na.rm=TRUE)
b = (1/n)*sum(WorldFactBook$Health.expenditures, na.rm=TRUE) - a*(1/n)*sum(WorldFactBook$Education.expenditures, na.rm=TRUE)
paste("Coefficient a : ", a)
paste("Coefficient b : ", b)

# Equation de la droite.
y = a*WorldFactBook$Education.expenditures+b

# Ajout de la droite de r�gression au graphique.
lines(y~WorldFactBook$Education.expenditures, type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(0.2,18.3,leg,col='red', lty=1,cex = 0.8)
```

```{r Depenses_education_Depense_sante-coefficient_lin�aire}
# Calcul de la corr�lation.
correlation = cov(WorldFactBook$Education.expenditures, WorldFactBook$Health.expenditures, use="na.or.complete")/(sd(WorldFactBook$Education.expenditures, na.rm=TRUE)*sd(WorldFactBook$Health.expenditures, na.rm=TRUE))
# Calcul du coefficient de corr�lation lin�aire.
coefficient = correlation^2
paste("Corr�lation :", correlation)
paste("Coefficient de corr�lation lin�aire : ", coefficient)
```

**Nous v�rifions nos caluls avec les formules pr�d�finies de R :**
  
  ```{r Depenses_education_Depense_sante-formules_R}
summary(lm(WorldFactBook$Health.expenditures ~ WorldFactBook$Education.expenditures, data = WorldFactBook))
```

Nous observons que nos valeurs calculatoires sont assez proches des valeurs calcul�es par la fonction R.

Maintenant, nous calculons l'intervalle de confiance gr�ce � :
\[\bar{x} \pm z^\star \frac{\sigma}{\sqrt{n}}\] o� $z^\star$ est la valeur critique � droite, $\sigma$ est l'�cart type et $\bar{x}$ est la moyenne.

```{r Depenses_education_Depense_sante-intervalle_confiance}
plot(WorldFactBook$Education.expenditures, WorldFactBook$Health.expenditures, main="Corr�lation entre les d�penses dans l'�ducation et les d�penses dans la sant�", xlab="D�penses dans l'�ducation", ylab="D�penses dans la sant�")
lines(y~WorldFactBook$Education.expenditures, type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(0.2,18.3,leg,col='red', lty=1,cex = 0.8)

# On calcule la valeur critique pour un intervalle de confiance de 95%.
z_star_95 <- qnorm(0.975)
paste("Valeur critique pour un intervalle de confiance de 95% : ", z_star_95)

# Calcul des intervalles :
inf = mean(WorldFactBook$Education.expenditures, na.rm=TRUE) - z_star_95 * (sd(WorldFactBook$Education.expenditures, na.rm=TRUE) / sqrt(n))
sup = mean(WorldFactBook$Education.expenditures, na.rm=TRUE) + z_star_95 * (sd(WorldFactBook$Education.expenditures, na.rm=TRUE) / sqrt(n))
paste("Valeur minimale : ", inf)
paste("Valeur maximale : ", sup)

# L'erreur :
erreur = (sup-inf)/2
paste("Erreur : ", erreur)

# Nos 2 droites d'intervalle de confiance.
y_sup = y+erreur
y_inf = y-erreur
# Ajout des droites d'intervalle de confiance au graphique.
lines(y_sup~WorldFactBook$Education.expenditures, type='l',col='green')
lines(y_inf~WorldFactBook$Education.expenditures, type='l',col='green')
```

**Conclusion de la 3�me �tude**
  Nous avons un coefficient pour la droite de r�gression qui est d'environ 0.2. Cela nous permet de dire que la qualit� de la regression n'est pas bonne. Nous pouvons donc dire que les d�penses dans l'�ducation ne sont pas corr�l�es avec les d�penses dans la sant�.

## 4�me �tude : La consommation d'�lectricit� est-elle en corr�lation avec le PIB ?

```{r Electricite_consommation-PIB}
plot(WorldFactBook$Electricity...consumption, WorldFactBook$GDP..purchasing.power.parity., main="Consommation d'�lectricit� en fonction du PIB", xlab="Consommation d'�lectricit�", ylab="PIB")
```

On peut voir que le nuage de point suit assez bien une droite.
On va donc calculer les coefficients a et b.

```{r Electricite_consommation-PIB-coefficients_a_b}
plot(WorldFactBook$Electricity...consumption, WorldFactBook$GDP..purchasing.power.parity., main="Corr�lation entre la consommation d'�lectricit� et le PIB", xlab="Consommation d'�lectricit�", ylab="PIB")

n = 279 # Nombres de variables trouv� avec dim(World_FactBook).

# Calcul des coefficients a et b.
a = cov(WorldFactBook$Electricity...consumption, WorldFactBook$GDP..purchasing.power.parity., use="na.or.complete")/var(WorldFactBook$Electricity...consumption, na.rm=TRUE)
b = (1/n)*sum(WorldFactBook$GDP..purchasing.power.parity., na.rm=TRUE) - a*(1/n)*sum(WorldFactBook$Electricity...consumption, na.rm=TRUE)
paste("Coefficient a : ", a)
paste("Coefficient b : ", b)

# Equation de la droite.
y = a*WorldFactBook$Electricity...consumption+b

# Ajout de la droite de r�gression au graphique.
lines(y~WorldFactBook$Electricity...consumption, type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(0,1.8e+13,leg,col='red', lty=1,cex = 0.8)
```


```{r Electricite_consommation-PIB-coefficient_lin�aire}
# Calcul de la corr�lation.
correlation = cov(WorldFactBook$Electricity...consumption, WorldFactBook$GDP..purchasing.power.parity., use="na.or.complete")/(sd(WorldFactBook$Electricity...consumption, na.rm=TRUE)*sd(WorldFactBook$GDP..purchasing.power.parity., na.rm=TRUE))
# Calcul du coefficient de corr�lation lin�aire.
coefficient = correlation^2
paste("Corr�lation :", correlation)
paste("Coefficient de corr�lation lin�aire : ", coefficient)
```

**Nous v�rifions nos caluls avec les formules pr�d�finies de R :**
  
  ```{r Electricite_consommation-PIB-formules_R}
summary(lm(WorldFactBook$GDP..purchasing.power.parity. ~ WorldFactBook$Electricity...consumption, data = WorldFactBook))
```

Nous observons que nos valeurs calculatoires sont assez proches des valeurs calcul�es par la fonction R.

Maintenant, nous calculons l'intervalle de confiance gr�ce � :
\[\bar{x} \pm z^\star \frac{\sigma}{\sqrt{n}}\] o� $z^\star$ est la valeur critique � droite, $\sigma$ est l'�cart type et $\bar{x}$ est la moyenne.

```{r Electricite_consommation-PIB-intervalle_confiance}
plot(WorldFactBook$Electricity...consumption, WorldFactBook$GDP..purchasing.power.parity., main="Corr�lation entre la consommation d'�lectricit� et le PIB", xlab="Consommation d'�lectricit�", ylab="PIB")
lines(y~WorldFactBook$Electricity...consumption, type='l',col='red', lwd=3)
leg = paste("y = ", a, "x + ", b)
legend(0,1.8e+13,leg,col='red', lty=1,cex = 0.8)

# On calcule la valeur critique pour un intervalle de confiance de 95%.
z_star_95 <- qnorm(0.975)
paste("Valeur critique pour un intervalle de confiance de 95% : ", z_star_95)

# Calcul des intervalles :
inf = mean(WorldFactBook$Electricity...consumption, na.rm=TRUE) - z_star_95 * (sd(WorldFactBook$Electricity...consumption, na.rm=TRUE) / sqrt(n))
sup = mean(WorldFactBook$Electricity...consumption, na.rm=TRUE) + z_star_95 * (sd(WorldFactBook$Electricity...consumption, na.rm=TRUE) / sqrt(n))
paste("Valeur minimale : ", inf)
paste("Valeur maximale : ", sup)

# L'erreur :
erreur = (sup-inf)/2
paste("Erreur : ", erreur)

# Nos 2 droites d'intervalle de confiance.
y_sup = y+erreur
y_inf = y-erreur
# Ajout des droites d'intervalle de confiance au graphique.
lines(y_sup~WorldFactBook$Electricity...consumption, type='l',col='green')
lines(y_inf~WorldFactBook$Electricity...consumption, type='l',col='green')
```

**Conclusion de la 4�me �tude**
  Nous avons un coefficient pour la droite de r�gression qui est d'environ 0.99. Cela nous permet de dire que la qualit� de la regression est tr�s bonne. Nous pouvons �galement dire, gr�ce au graphe de la droite que la consommation d'�lectricit� est tr�s corr�l�e avec le PIB (pente non nulle de la droite).

## Test d'hypoth�se

#H0 : la moiti� des gens utilisent internet

#```{r test}
#pop = mean(World_FactBook$Population, na.rm=TRUE)
#pop/2
#mean(World_FactBook$Internet.users, na.rm=TRUE)
#inference(y = World_FactBook$Population, x = World_FactBook$Internet.users, data = World_FactBook, statistic = #"mean", type = "ht", null = 0, alternative = "greater", method = "theoretical")
#```

