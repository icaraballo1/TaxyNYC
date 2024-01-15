CONTEXTE DU PROJET :

Vous êtes mandaté de prédire si le montant du tarif (y compris les péages) pour un trajet en taxi à
New York (en fonction des lieux de prise en charge et de dépose) est équitable.
Si le frais est inférieur à 10 $ est équitable sinon, non.
Le dossier de données contient un ensembles de données :
- Projet_30636_A22_Donnee_Principale.csv : 114000 lignes et 8 colonnes

DESCRIPTION DES VARIABLES :
# IDENTIFICATION :
• ID : un identifiant unique chaque ligne dans les ensembles d'apprentissages et de Score.
Il doit simplement être utilisé comme un champ d'ID unique. Requis dans votre CSV de
soumission.
# LES AUTRES COLONNES :
• pickup_datetime : valeur d'horodatage indiquant quand le trajet en taxi a commencé.
• pickup_longitude : la coordonnée de longitude de l'endroit où le trajet en taxi a
commencé.
• pickup_latitude : la coordonnée de latitude de l'endroit où le trajet en taxi a commencé.
• dropoff_longitude : la coordonnée de longitude de l'endroit où le trajet en taxi s'est
terminé.
• dropoff_latitude : la coordonnée de latitude de l'endroit où le trajet en taxi s'est terminé.
• nombre_passagers : nombre entier indiquant le nombre de passagers dans le trajet en
taxi.
# VARIABLE CIBLE :

• fare_amount (y) : montant en dollars du coût du trajet en taxi. Cette valeur est
uniquement dans l'ensemble d'apprentissage.
• Note : étant donné que le problème principal est un problème de classification, il faut
transférer cette variable avec la condition suivante :
- Si fare_amount <10 : 1
- Sinon : 0

RECONNAISSANCE
Les données sont inspirées d’une compétition Kaggle hébergé en partenariat avec Google Cloud et
Coursera.
