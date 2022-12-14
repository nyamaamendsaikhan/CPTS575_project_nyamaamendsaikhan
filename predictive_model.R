# Project 

rm(list = ls())
library(dplyr)

# ------------------------------------------------------------------------------------
# Layer row column 16506*100*3
layrowcol <- read.csv("Layrowcol.csv", header = TRUE)
names(layrowcol) <- c('layer', 'row', 'column')

# ------------------------------------------------------------------------------------
# Import  river cells
river_cells <- read.csv("river.csv", header = TRUE)
river_cells1 <- layrowcol %>%
  left_join(river_cells)
#river_cells1 <- na.omit(river_cells1)
write.csv(river_cells1, "river_stp1.csv")

# Import drain cells
drain_cells <- read.csv("drain.csv", header = TRUE)
drain_cells1 <- layrowcol %>%
  left_join(drain_cells)
#drain_cells1 <- na.omit(drain_cells1)
write.csv(drain_cells1, "drain_stp1.csv")

# ------------------------------------------------------------------------------------
# No need to change layer, row, columns (from text file)
river_matlab <- read.csv('river_matlab.csv', header = TRUE)
names(river_matlab) <- c('layer', 'row', 'column', 'stp1','stp2','stp3','stp4','stp5','stp6','stp7','stp8','stp9','stp10','stp11','stp12','stp13','stp14',
                         'stp15','stp16','stp17','stp18','stp19','stp20','stp21','stp22','stp23','stp24','stp25','stp26','stp27',
                         'stp28','stp29','stp30','stp31','stp32','stp33','stp34','stp35','stp36','stp37','stp38','stp39','stp40',
                         'stp41','stp42','stp43','stp44','stp45','stp46','stp47','stp48','stp49','stp50','stp51','stp52','stp53',
                         'stp54','stp55','stp56','stp57','stp58','stp59','stp60','stp61','stp62','stp63','stp64','stp65','stp66',
                         'stp67','stp68','stp69','stp70','stp71','stp72','stp73','stp74','stp75','stp76','stp77','stp78','stp79',
                         'stp80','stp81','stp82','stp83','stp84','stp85','stp86','stp87','stp88','stp89','stp90')
river_matlab2 <- layrowcol %>% left_join(river_matlab)
river_matlab2[is.na(river_matlab2)] <- -99
write.csv(river_matlab2, 'river_matlab_all.csv')

drain_matlab <- read.csv('drain_matlab.csv', header = TRUE)
names(drain_matlab) <- c('layer', 'row', 'column', 'stp1','stp2','stp3','stp4','stp5','stp6','stp7','stp8','stp9','stp10','stp11','stp12','stp13','stp14',
                         'stp15','stp16','stp17','stp18','stp19','stp20','stp21','stp22','stp23','stp24','stp25','stp26','stp27',
                         'stp28','stp29','stp30','stp31','stp32','stp33','stp34','stp35','stp36','stp37','stp38','stp39','stp40',
                         'stp41','stp42','stp43','stp44','stp45','stp46','stp47','stp48','stp49','stp50','stp51','stp52','stp53',
                         'stp54','stp55','stp56','stp57','stp58','stp59','stp60','stp61','stp62','stp63','stp64','stp65','stp66',
                         'stp67','stp68','stp69','stp70','stp71','stp72','stp73','stp74','stp75','stp76','stp77','stp78','stp79',
                         'stp80','stp81','stp82','stp83','stp84','stp85','stp86','stp87','stp88','stp89','stp90')
drain_matlab2 <- layrowcol %>% left_join(drain_matlab)
drain_matlab2[is.na(drain_matlab2)] <- -99
write.csv(drain_matlab2, 'drain_matlab_all.csv')

# ------------------------------------------------------------------------------------
# Pumping rates
pumping <- read.csv("pumping_all.csv", header = TRUE)
names(pumping) <- c('stp','layer', 'row', 'column', 'wellid','qdes')
pumping$layer <- pumping$layer +1
pumping$row <- pumping$row + 1
pumping$column <- pumping$column +1

# Find cumulative pumping for each horizontal cell
pumping_cum <- pumping %>% select(stp,layer,row,column,wellid,qdes) %>% group_by(stp,layer,row,column) %>%summarise(qdes_total = sum(qdes))

# Exporting pumping rates for stp 71-90
stp_all <- read.csv('stp_all.csv')
names(stp_all) <- c('stp','layer', 'row', 'column')
pumping_71_90 <- stp_all %>% left_join(pumping_cum)
pumping_71_90[is.na(pumping_71_90)] <- -99
write.csv(pumping_71_90,'pumping_71_90.csv')

# 
rm(list = ls())
input <- read.csv('project_input.csv', header = T)
input <- input%>% select(-input7, -input8, -input9,-input10)
names(input) <- c('stp','layer','row','column','sim_hds','intl_hds','pump','rch','riv','drn','thick','hk','vka')
input <- input%>%select(sim_hds, intl_hds, rch, thick, hk,vka)
input1 <- input
input1[input1 == -99] <- NA
#write.csv(input1, 'proj_final_input.csv')

input2 <- input1 %>% select(-stp,-layer,-row,-column)

install.packages('tidyr')
library(tidyr)
input3 <- input2[complete.cases(input2),]
##############################################################################################
# Validation Set Approach

# The dataset is divided into two sets: training and testing data set
# Model is trained on the training set, and the model is tested on test set

install.packages("caret")
library(caret)
set.seed(915)
# Training dataset - 80%
vsa_train <- createDataPartition(input3$sim_hds, p=0.8, list = FALSE)
vsa_train_set <- input3[vsa_train,]
vsa_test_set <- input3[-vsa_train,]

# VSA model
vsa_model <- lm(sim_hds ~., data=vsa_train_set)
vsa_predict <- predict(vsa_model, vsa_test_set)

data.frame( R2 = R2(vsa_predict, vsa_test_set $ sim_hds ),
            RMSE = RMSE(vsa_predict, vsa_test_set $ sim_hds ),
            MAE = MAE(vsa_predict, vsa_test_set $ sim_hds ))

# VSA model (pump+rch+drn+riv)
vsa_model <- lm(sim_hds ~ pump+rch+drn+riv, data=vsa_train_set)
vsa_predict <- predict(vsa_model, vsa_test_set)

data.frame( R2 = R2(vsa_predict, vsa_test_set $ sim_hds ),
            RMSE = RMSE(vsa_predict, vsa_test_set $ sim_hds ),
            MAE = MAE(vsa_predict, vsa_test_set $ sim_hds ))


# VSA model (ppump+rch+drn+riv+intl_hds)
vsa_model <- lm(sim_hds ~ pump+rch+drn+riv+intl_hds, data=vsa_train_set)
vsa_predict <- predict(vsa_model, vsa_test_set)

data.frame( R2 = R2(vsa_predict, vsa_test_set $ sim_hds ),
            RMSE = RMSE(vsa_predict, vsa_test_set $ sim_hds ),
            MAE = MAE(vsa_predict, vsa_test_set $ sim_hds ))

# VSA model (pump+rch+drn+riv+intl_hds+thick+hk+vka)
vsa_model <- lm(sim_hds ~ pump+rch+drn+riv+intl_hds+thick+hk+vka, data=vsa_train_set)
vsa_predict <- predict(vsa_model, vsa_test_set)

data.frame( R2 = R2(vsa_predict, vsa_test_set $ sim_hds ),
            RMSE = RMSE(vsa_predict, vsa_test_set $ sim_hds ),
            MAE = MAE(vsa_predict, vsa_test_set $ sim_hds ))


##############################################################################################
# Leave one out cross-validation (LOOCV)
loocv_train <- trainControl(method = "LOOCV")

loocv_model <- train(sim_hds ~pump+rch+drn+riv, data = input3, method = "lm", trControl = loocv_train)
print(loocv_model)

##############################################################################################
# k fold cross-validation
set.seed(159)
kcv_control <- trainControl(method ="cv", number = 50)

kcv_model <- train(sim_hds ~pump+rch+drn+riv, data=input3, method = "lm", trControl = kcv_control)
print(kcv_model)

kcv_model <- train(sim_hds ~pump+rch+drn+riv+intl_hds, data=input3, method = "lm", trControl = kcv_control)
print(kcv_model)

kcv_model <- train(sim_hds ~pump+rch+drn+riv+intl_hds+thick+hk+vka, data=input3, method = "lm", trControl = kcv_control)
print(kcv_model)

# Repeated k fold cross-validation
set.seed(815)
kcv_rep_control <- trainControl(method ="repeatedcv", number = 50, repeats = 10)

kcv_rep_model <- train(sim_hds ~pump+rch+drn+riv, data=input3, method = "lm", trControl = kcv_rep_control)
print(kcv_rep_model)


kcv_rep_model <- train(sim_hds ~pump+rch+drn+riv+intl_hds, data=input3, method = "lm", trControl = kcv_rep_control)
print(kcv_rep_model)

kcv_rep_model <- train(sim_hds ~pump+rch+drn+riv+intl_hds+thick+hk+vka, data=input3, method = "lm", trControl = kcv_rep_control)
print(kcv_rep_model)

#########################################################
# K fold cross validation in R
library(modelr)
cv <- crossv_kfold(input3, k = 100)


model1 <- map(cv$train, ~lm(sim_hds ~ pump+rch+drn+riv, data=input3))
model2 <- map(cv$train, ~lm(sim_hds ~ pump+rch+drn+riv+intl_hds, data=input3))
model3 <- map(cv$train, ~lm(sim_hds ~ pump+rch+drn+riv+intl_hds+thick+hk+vka, data=input3))
errs <- map2_dbl(model1,cv$test,rmse)


