

#On commence par charger les packages

library(statsr)
library(dplyr)
library(ggplot2)

#On a créé une fonction permettant d'enlever les "," et les "$" de nos valeurs

enleverdollar<- function(x){
  gsub("\\$|,","",x)
}

#On récupère tous les fichiers 
getwd()
#setwd(dir="./Stat/Projet")
donneescode=read.csv(file = "codes.csv", na.string='NA')
donneescategories=read.csv(file = "categories.csv",na.string='NA')

#On récupère les noms des pays, les catégories et leurs références
nomPays<- data.frame(Name= donneescode[,'Name'])
nomCategories<- data.frame(Name= donneescategories[,'Name'],Ref= donneescategories[,'Num'])

#On commence à remplir notre WorldFactBook avec seulement le nom des pays pour l'instant
WorldFactBook<- data.frame(Name= nomPays$Name)

for (i in 1:dim(nomCategories)[1]){
  
  #On récupère les références des catégories pour récupérer le c*.csv correspondant
  r= nomCategories$Ref[i]
  #On crée la chaine de caractères représentant le nom du fichier à récupérer
  str=paste("./data/c",r,".csv",sep="")
  #On récupère le csv correspondant
  csvCorrespondant=read.csv(file= str, na.strings = 'NA' )
  #On ne garde que les colonnes Name et Value
  el<- data.frame(Pays= csvCorrespondant[,'Name'],Valeur=csvCorrespondant[,'Value'])
  #Grâce à setdiff on recupère les pays qui sont dans WorldFactBook mais qui ne sont pas dans les données récupérées du csv
  #On veut les rajouter avec une valeur NA
  manque=setdiff(WorldFactBook$Name,el$Pays)
  PaysNA <-data.frame(Pays=manque, Valeur= 'NA')
  #On ajoute les pays manquants à nos données récupérées pour la catégories qu'on traite
  cvs <- rbind(el,PaysNA)
  cvs[] <- lapply(cvs, as.character)
  cvs<- cvs[order(cvs$Pays),]
  #on prend soin de trier les données selon l'ordre alhphabetique des pays
  tmp<-data.frame(Name=cvs$Pays,Valeur=cvs$Valeur)
  #Il peut arriver que l'on récupère des données dans les csv correspondant à des pays qui ne nous intéressent pas
  #Ces pays ne sont pas contenus dans la liste des pays que l'on souhaite étudier
  test<- data.frame(Pays=setdiff(tmp$Name,WorldFactBook$Name))
  #on récupère le nom d'éventuels pays qui se trouveraient dans nos données récupérées mais pas dans WorldFactBook
  
  if(dim(test)[1]!=0){
    #On teste si il y a effectivement des pays dans nos données qu'on ne veut pas étudier 
    for (i in 1:dim(test)[1]){
      #Pour chacun de ces pays on recupère l'indice de sa position dans notre tableau de données recupérées
      ind=which(cvs$Pays==test[i,1])
      cvs<-cvs[-ind,]
      #On enlève ensuite la ligne de données correspondant à ce pays
    }
  }
  
  #On récupère les valeurs du c*.csv et on enleve les "," et les "dollars"$" si besoin
  result=cvs$Valeur
  result=enleverdollar(result)
  
  #On le rentre dans notre WorldFactBook
  WorldFactBook<- data.frame(WorldFactBook, result)
  
}
#Enfin, on souhaite renommer les colonnes de notre WorldFactBook
WorldFactBook_bis<-WorldFactBook[,-1]
colnames(WorldFactBook_bis)<-nomCategories$Name
c1<- data.frame(Name= nomPays$Name)
WorldFactBook<-cbind(c1,WorldFactBook_bis)

write.csv(WorldFactBook,'WorldFactBook.csv')
