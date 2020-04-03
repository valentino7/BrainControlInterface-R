library("LiblineaR")
library("e1071")
library("ggplot2")
library("tidyr")


source("split_data.R")
source("data_preprocessing.R")
source("remove_signals.R")
source("data_normalization.R")
source("chart.R")
source("choosing_classifier.R")
source("choosing_parameters.R")
source("linear_SVM.R")
source("gaussian_SVM.R")
source("polynomial_SVM.R")
source("prediction_to_character.R")
source("training_bestParameters.R")
source("training.R")
source("test.R")


# ------------------------- IMPORT DATASET ------------------------- #
X <- read.table("dataset/X.txt", quote="\"", comment.char="")

plot_dir <- paste(getwd(), "grafici/", sep="/" )
dir.create(plot_dir)
N_signals<-204
colnames(X) <- c(
  paste("Fz_",1:N_signals,sep=""),
  paste("Cz_",1:N_signals,sep=""),
  paste("Pz_",1:N_signals,sep=""),
  paste("Oz_",1:N_signals,sep=""),
  paste("P3_",1:N_signals,sep=""),
  paste("P4_",1:N_signals,sep=""),
  paste("P7_",1:N_signals,sep=""),
  paste("P8_",1:N_signals,sep="")
)
Y <- read.table("dataset/Y.txt", quote="\"", comment.char="")
C <- read.table("dataset/C.txt", quote="\"", comment.char="")
X["C"] = C
X["Y"] = Y


# ------------------------- PRE PROCESSING ------------------------- #
# Si effettua la media sulle 10 iterazioni per ogni carattere
words_number<-6
df<- data_preprocessing(X,words_number)

# ------------------------- CHART ------------------------- #
# grafici dei campioni  dei segnali per ogni carattere e per ogni elettrodo
#chart(df,plot_dir)

# dopo aver analizzato gli stimoli di ogni elettrodo per ogni classificazione, 
# si è deciso di rimuovere gli ultimi 38 valori ( dei 204 iniziali) per ogni elettrodo
# Infatti si ritiene che i primi 664 ms bastino per individuare la p300.
df <- remove_signals(df)


# ------------------------- DATA SPLIT ------------------------- #
# Si effettua uno split random dei dati nel seguente modo: 
# 5 parole vengono usate per training e validation, 1 parola per test
split <- split_data(df)

# ------------------------- DATA NORMALIZATION ------------------------- #
# si normalizzano i dati rimuovendo la media e dividendo per la varianza
normalized_data <- data_normalization(split)

# ------------------------- DATA TRAINING & VALIDATION------------------------- #
# viene effettuato il k-fold validation per individuare prima il classificatore migliore tra SVM con kernel lineare, gaussiano o polinomiale
cross_validation_output <- choosing_classifier(normalized_data)
View(cross_validation_output)
# viene poi effettuato il k-fold validation per individuare l'iperparametro migliore per il classificatore migliore prima individuato
cross_validation_parameter_output <- choosing_parameters(normalized_data)
View(cross_validation_parameter_output)

# ------------------------- TRAINING WITH BEST CLASSIFIER AND HYPERPARAMETER ------------------------- #
# si addestra l'SVM con 5 parole e si usa la parola non usiata come test
out<- training_bestParameters(df)
View(out)
# ------------------------- TRAINING WITH ALL DATA------------------------- #
model <- training(df)
# decommentare le seguenti righe per inserire la parola che si intende testare:
# il formato dei file per la parola da testare deve essere uguale al formato dei file del dataset
# data <- read.table("dataset/X_test.txt", quote="\"", comment.char="")
#Y_T <- read.table("dataset/Y_test.txt", quote="\"", comment.char="")
#C_T <- read.table("dataset/C_test.txt", quote="\"", comment.char="")
#colnames(data) <- c(
#   paste("Fz_",1:N_signals,sep=""),
#   paste("Cz_",1:N_signals,sep=""),
#   paste("Pz_",1:N_signals,sep=""),
#   paste("Oz_",1:N_signals,sep=""),
#   paste("P3_",1:N_signals,sep=""),
#   paste("P4_",1:N_signals,sep=""),
#   paste("P7_",1:N_signals,sep=""),
#   paste("P8_",1:N_signals,sep="")
# )
#data["C"] = C_T
#data["Y"] = Y_T
#test_result <- test( model,data)

