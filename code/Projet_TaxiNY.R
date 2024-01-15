##########################################################################################################
##                        Methodes d'ensemble                                                           ##
##                          Base sur les données "Donnee_Principale"                                          ##
##########################################################################################################


###############################################################################################
# 1. Préparer l'environnement
###############################################################################################
## Vérifier si les packages sont présent et sinon les installer
# Pour plus de package, ajouter des noms dans le vecteur c()
list.of.packages <- c("adabag",
                      "randomForest",
                      "pROC",
                      "rpart.plot",
                      "geosphere",
                      "dplyr",
                      "pacman",
                      "pscl",
                      "ROCR",
                      "lightgbm",
                      "methods",
                      "Matrix",
                      "caret",
                      "rattle","vip","gbm")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Lance l'ensemble des packages dans la liste "list.of.packages"
lapply(list.of.packages, require, character.only = TRUE)

#**********************************************************************************************
# Créer un repertoire de travail                                                              #
#**********************************************************************************************
## Créer Repertoire de travail 
setwd("Ajouter Repertoire")

#**************************************************
# Importer les données                            #
#**************************************************
Donnee_Principale= read.csv("Projet_30636_A22_Donnee_Principale.csv",header= TRUE, sep=",")

##************************************************************
# Exploration des données
##************************************************************

## On constate une données aberrante dans la colonne "passenger_count", soit 999.
## On constate aussi des données manquantes dans les colonnes "passenger_count", "fare_amount" et "pickup_datetime".
## Le traitement se fera après avoir séparé nos données "apprentissage" et "validation". 
## La seule exception étant "fare_amount" que nous allons traiter avant la séparation.

summary(Donnee_Principale)

na_strings <- c("")
missing_values <- is.na(Donnee_Principale$pickup_datetime) | Donnee_Principale$pickup_datetime %in% na_strings

## Montrer les files qui ont données manquantes dans "pickup_datetime"
rows_with_missing <- Donnee_Principale[missing_values, ]

### Le traitement de notre variable cible "fare_amount" se fait avant la séparation des données.
## Supprimer les lignes où il manque la variable cible "fare_amount"

Donnee_Principale<-Donnee_Principale[!is.na(Donnee_Principale$fare_amount), ]


##************************************************************
# Création des fichiers entrainement et validation           #
##***********************************************************

# On a 114 000 observations dans notre table de données.
# On voudrais avoir une échatillon de "apprentissage" avec 91 200 (80%) observations et 
# une échantillon de test avec 22 800 (20%) observations

## Établir seed
set.seed(Sys.time())
set.seed(4)

## Couper l'échantillon
indtrain=sample(1:NROW(Donnee_Principale),91200,replace=FALSE)

df_train=Donnee_Principale[indtrain,]
df_valid=Donnee_Principale[-indtrain,]

##*************************************************
# Nettoyage des données                           #
#**************************************************

## Transformer la variable cible (fare_amount) dans une variable catégorialle et binaire (fare_amount_num)
## 1 si la valeur est inférieure à 10. Cela correspont à un tarif juste.
## 0 si la valeur est suppérieure à 10. Cela correspont à un tarif injuste.

df_train$fare_amount_num=ifelse(df_train$fare_amount<10,1,0)
df_valid$fare_amount_num=ifelse(df_valid$fare_amount<10,1,0)

## Remplacement des données aberrantes et des données manquantes dans "passenger_count" 
## par la médiane de df_train dans nos deux fichiers.
## La médiane nous évite d'avoir une donnée aberrante avec décimales.
Median_passanger<-median(df_train$passenger_count,na.rm=TRUE)
df_train$passenger_count[is.na(df_train$passenger_count)]<-Median_passanger
df_train$passenger_count<-ifelse(df_train$passenger_count>7,Median_passanger,df_train$passenger_count)

df_valid$passenger_count[is.na(df_valid$passenger_count)]<-Median_passanger
df_valid$passenger_count<-ifelse(df_valid$passenger_count>7,Median_passanger,df_train$passenger_count)


##*************************************************
# Création de variables explicatives              #
#**************************************************
## Créer une colonne avec l'heure isolée (supprimer mm:ss) à partir de la colonne pickup_datetime, 
## on aura donc un crenau horaire d'une heure dans une nouvelle colonne (Hour_Num). 
## Par exemple, toutes les heures entre 11h00 et 11h59 seront assignées à 11.
## Ensuite on la transforme en numeric

df_train$Hour_Num=substring(df_train$pickup_datetime, first=12, last=13)
df_train$Hour_Num <- as.numeric(df_train$Hour_Num)
df_valid$Hour_Num=substring(df_valid$pickup_datetime, first=12, last=13)
df_valid$Hour_Num <- as.numeric(df_valid$Hour_Num)

## Conversion de la colonne "pickup_datetime" en objet datetime

df_train$pickup_datetime<- as.Date(df_train$pickup_datetime)
df_valid$pickup_datetime<- as.Date(df_valid$pickup_datetime)

## Création de la colonne du nombre du jour de la semaine.
## De 1 to 7. 1 étant lundi et 7 étant dimanche.
df_train$date_num <- as.numeric(format(df_train$pickup_datetime, "%u"))
df_valid$date_num <- as.numeric(format(df_valid$pickup_datetime, "%u"))

## On a crée une fonction pour calculer la mode

Mode_estimate <- function(x) {
  distinct_values <- unique(x)
  distinct_values[which.max(tabulate(match(x, distinct_values)))]
}


## Finalement on remplace la mode dans la nouvelle colonne "date_num" et "Hour_Num"
Mode_date<- Mode_estimate(df_train$date_num)
df_train$date_num[is.na(df_train$date_num)]<- Mode_date
df_valid$date_num[is.na(df_valid$date_num)]<- Mode_date

Mode_Hour<-Mode_estimate (df_train$Hour_Num)

df_train$Hour_Num[is.na(df_train$Hour_Num)]<-Mode_Hour
df_valid$Hour_Num[is.na(df_valid$Hour_Num)]<-Mode_Hour

## Nouvelle variable "AM_PM" afin de différencier 
## 0 si AM et 1 si PM.
## Remplacement des NA par la mode de train.
df_train$AM_PM<-ifelse(df_train$Hour_Num<=12,0,1)
df_valid$AM_PM<-ifelse(df_valid$Hour_Num<=12,0,1)

Mode_AM_PM<-Mode_estimate (df_train$AM_PM)

df_train$Hour_Num[is.na(df_train$Hour_Num)]<-Mode_AM_PM
df_valid$Hour_Num[is.na(df_valid$Hour_Num)]<-Mode_AM_PM

## Calculer la distance (m) parcourue à partir de la Latitud-Longitud dans une nouvelle variable "Distance" 
##On utilise le package geospher, concretement la fonction distHaversine.

df_train$Distance<- distHaversine(cbind(df_train$pickup_longitude,df_train$pickup_latitude),
                                 cbind(df_train$dropoff_longitude,df_train$dropoff_latitude), r=6378137)
df_valid$Distance<- distHaversine(cbind(df_valid$pickup_longitude,df_valid$pickup_latitude),
                                  cbind(df_valid$dropoff_longitude,df_valid$dropoff_latitude), r=6378137)


##On va tronquer les colonnes de latitud et longitud en laissant seulement deux chiffres décimales

df_train$pickup_longitude=trunc(df_train$pickup_longitude*100)/100
df_train$pickup_latitude=trunc(df_train$pickup_latitude*100)/100
df_train$dropoff_longitude=trunc(df_train$dropoff_longitude*100)/100
df_train$dropoff_latitude=trunc(df_train$dropoff_latitude*100)/100

df_valid$pickup_longitude=trunc(df_valid$pickup_longitude*100)/100
df_valid$pickup_latitude=trunc(df_valid$pickup_latitude*100)/100
df_valid$dropoff_longitude=trunc(df_valid$dropoff_longitude*100)/100
df_valid$dropoff_latitude=trunc(df_valid$dropoff_latitude*100)/100


## On elimine les variables nuisibles : ID et pickup_datetime,
## ainsi que la variable à prédire : fare_amount


df_train=df_train[,-c(1,2,8)]
df_valid=df_valid[,-c(1,2,8)]
df_train$fare_amount_num <-
  as.factor(df_train$fare_amount_num)
df_valid$fare_amount_num <-
  as.factor(df_valid$fare_amount_num)

####################################################
# 2. Méthode arbre de classification 
####################################################
set.seed(4)

## Appliquer le modele d'arbre de classification 

Modele_Arbre=rpart(fare_amount_num~.,data=df_train,
                   method="class",
                   control= rpart.control(maxdepth=4,minsplit=600, cp=0))

fancyRpartPlot(Modele_Arbre, caption = NULL)
rpart.plot(Modele_Arbre,type=5,extra=1)


#*************************************************************************************
# Évaluer la performace du modèle
#*************************************************************************************

## Predire la valeur de la variable cible sur l'échantillon de validation
Pred_Arbre=predict(Modele_Arbre,
                        newdata =df_valid, 
                        type="class" )

## Évaluer la performance du modèle sur l'echantillon validation
## Matrice de confusion:
MC0=table(Pred_Arbre,df_valid$fare_amount_num)

##  Accuracy:
mean(Pred_Arbre==df_valid$fare_amount_num)

sum(diag(MC0))/sum(MC0)

## AUC ROC: type= prob
Prob_Arbre=predict(Modele_Arbre,
                        df_valid,
                        type='prob')


auc_Arbre=auc(df_valid$fare_amount_num,
              Prob_Arbre[,2])

roc(df_valid$fare_amount_num,
    Prob_Arbre[,2],
    plot=TRUE,
    col="#4daf4a",
    lwd=1,
    print.auc=TRUE)


## Importance des variables explicatives dans le model.
var_importance <- vip::vip(Modele_Arbre, num_features = 9)
print(var_importance)

#########################################################
# 3.- Modèle Bagging
#########################################################
set.seed(4)

### Création du modèle

Modele_Bagging <-
  bagging(fare_amount_num~.,
          data =df_train,
          mfinal = 100,
          control=rpart.control(maxdepth=4, minsplit=600, cp=0))


##Extractions de sorties

head(Modele_Bagging$class)

#*********************************************************************************
# Évaluer la performace du modèle Bagging                                        #
#*********************************************************************************

## Taux de bonne classification:

prediction_bagging <-
  predict(Modele_Bagging,
          newdata = df_valid,
          type = "class")



## Matrice de confusion Bagging et Taux bonne classification

Accur_Bag<-prediction_bagging$confusion
TBC_Bagging <- 1-prediction_bagging$error

#### AUC ROC
Prob_Bagging=predict(Modele_Bagging,
                     newdata =df_valid,
                     type="prob")

auc_Bagging=auc(df_valid$fare_amount_num,
                Prob_Bagging$prob[,2])

## AUC Graphique

roc(response = df_valid$fare_amount_num,
    Prob_Bagging$prob[,2],
    plot = TRUE,
    col = "#FF0000",
    lwd = 3,##largeur de la curve
    print.auc = T)

## Tableau d'importance Bagging

importanceplot(Modele_Bagging,
               horiz = T,
               cex.names = 0.8)

Bagging_importance=Modele_Bagging$importance


#########################################################
# 4.- Modèle Boosting
#########################################################
set.seed(4)

### Création du modèle

Modele_Boosting=boosting(fare_amount_num~.,
                         data = df_train,
                         mfinal=100,
                         control=rpart.control(maxdepth=4, minsplit=600, cp=0))



#*************************************************************************************
# Évaluer la performace du modèle
#*************************************************************************************

Pred_Boosting=predict(Modele_Boosting,
                      newdata =df_valid, 
                      type="class" )

## TBC Boosting et onfusionMatrix
TBC_Boosting=1-Pred_Boosting$error

Pred_Boosting$confusion

## AUC:

Prob_Boosting=predict(Modele_Boosting,
                      newdata =df_valid,
                      type="prob")

auc_Boosting=auc(df_valid$fare_amount_num,Prob_Boosting$prob[,2])
auc_Boosting
## AUC Graphique de Boosting
roc(response = df_valid$fare_amount_num,
    Prob_Boosting$prob[,2],
    plot = TRUE,
    col = "#660066",
    lwd = 1,
    print.auc = T)

## Les variables importantes du Modèle de Boosting

importanceplot(Modele_Boosting,
               horiz=T,
               cex.names=0.8)

Boosting_importance=Modele_Boosting$importance
Boosting_importance


##############################################
# 5. Modèle Forêt Aléatoire
##############################################
set.seed(4)

### Création du modèle
Modele_RF=randomForest(fare_amount_num~.,
                       data = df_train,
                       ntree=100,
                       mtry=4
                       )


#*************************************************************************************
# Évaluer la performace du modèle
#*************************************************************************************

Pred_RF=predict(Modele_RF,
                newdata =df_valid,
                type="class" )


## Matrice de confusion Forêt aléatoire
Confusion_RF <-
  table(Pred_RF,
        df_valid$fare_amount_num)

Confusion_RF

## Taux de bonne classification Forêt aléatoire:
TBC_RF <-
  sum(diag(Confusion_RF))/sum(Confusion_RF)
TBC_RF

## AUC ROC Forêt aléatoire:

Prob_RF=predict(Modele_RF,
                newdata =df_valid,
                type="prob")

auc_RF=auc(df_valid$fare_amount_num,Prob_RF[,2])
auc_RF


roc(response = df_valid$fare_amount_num,
    Prob_RF[,2],
    plot = TRUE,
    col = "#0000CC",
    lwd = 1,
    print.auc = T)

### Les variables importantes du Modèle de Foret Alatoire

varImpPlot(Modele_RF, sort=TRUE, n.var=10)
RF_importance=Modele_RF$importance


###############################################################
# 6.- Gradient Boosting
###############################################################

df_train$fare_amount_num<-as.numeric(df_train$fare_amount_num)-1

set.seed(4)

Modele_gbm = gbm(fare_amount_num ~.,
              data = df_train,
              shrinkage=0.1,
              distribution="bernoulli",
              interaction.depth = 4,
              cv.folds=5,
              n.trees = 200,
              verbose=T)

pred_gbm = predict.gbm(object = Modele_gbm,
                   newdata = df_valid,
                   n.trees = 200,
                   type = "response")

#*************************************************************************************
# Évaluer la performace du modèle
#*************************************************************************************

## Matrice de Confusion

Pred_Bin<-as.factor(ifelse(pred_gbm > 0.5, 1,0))

df_valid$fare_amount_num<- as.factor(df_valid$fare_amount_num)


confusionMatrix(Pred_Bin,df_valid$fare_amount_num)



## AUC Graphique de Gradient Boosting model
roc(df_valid$fare_amount_num, pred_gbm,
                 plot = TRUE,
                 col = "orange",
                 lwd = 1.5,
                 print.auc = T)

##best.iter<- gbm.perf(Modele_gbm, method="OOB")
## Variables Importance
summary(Modele_gbm)



###############################################################
# 7.- Modèle LightGBM
###############################################################
set.seed(4)

## On separe la variable cible des données de train et validation

X_train<-df_train[,-c(6)]
y_train <- df_train$fare_amount_num  

X_valid<-df_valid[,-c(6)]
y_valid <- df_valid$fare_amount_num  

# Convert data to LightGBM dataset format
train_data <- lgb.Dataset(data = as.matrix(X_train), label = as.vector(y_train),
                          free_raw_data = FALSE)

params <- list(
  objective = "binary",  # Binary classification
  metric = "binary_logloss",  # Evaluation metric
  boosting_type = "gbdt",  # Gradient boosting type
  num_leaves = 200,  # Number of leaves in one tree
  max_depth = 6,
  learning_rate = 0.05,  # Learning rate
  feature_fraction = 0.9,  # Fraction of features used for tree building
  verbose = -1  # Suppress LightGBM's output
)

num_round <- 100 
model <- lgb.train(params, train_data, num_round)



test_pred <- predict(model, as.matrix(X_valid))
# Create a confusion matrix
confusion_matrix <- confusionMatrix(as.factor(ifelse(test_pred > 0.5,1,0)),
                                    as.factor(y_valid))
## Variables importance
tree_imp <- lgb.importance(model, percentage = TRUE)
lgb.plot.importance(tree_imp, top_n = 9L, measure = "Frequency")


## AUC ROc acabarlo

roc_lgbm <- roc(y_valid, test_pred)

# Calculate ROC AUC
roc_auc <- auc(roc_lgbm)

# Print ROC AUC
cat("ROC AUC:", roc_auc, "\n")

# Plot ROC curve
plot(roc_lgbm, main = "ROC Curve", print.auc = TRUE)


#######################################################################
# 8.- Comparaison des modèles
########################################################################


roc(df_valid$fare_amount_num,
    Prob_Arbre[,2],
    plot=TRUE,
    col="#4daf4a",
    lwd=1.5,
    print.auc=TRUE)
plot.roc(df_valid$fare_amount_num,Prob_Bagging$prob[,2],col="#FF0000",lwd=1.5,add=T,print.auc=TRUE,print.auc.y=0.45)
plot.roc(df_valid$fare_amount_num,Prob_Boosting$prob[,2],col="#CCCC00",lwd=1.5,add=T,print.auc=TRUE,print.auc.y=0.4)
plot.roc(df_valid$fare_amount_num,Prob_RF[,2],col="#0000CC",lwd=1.5,add=T,print.auc=TRUE,print.auc.y=0.35)
plot.roc(df_valid$fare_amount_num, pred_gbm,
         col = "orange",
         lwd = 1.5,
         add=T,
         print.auc = T,
         print.auc.y=0.30)
plot.roc(y_valid, test_pred,
         col = "cyan",
         lwd = 1.5,
         add=T,
         print.auc = T,
         print.auc.y=0.25)
legend("bottomright",legend=c("Arbre","Bagging","Boosting",
                              "Foret aleatoire","Gradient Boosting", "LightGBM"),
                               col=c("#4daf4a","#FF0000","#CCCC00","#0000CC","orange","black"),
                               lwd=4)




