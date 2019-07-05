##file open.##
##############

setwd("~/0_제주_EVI_BN파일준비/0_2_데이터테이블준비_보강분석/6_BN분석/11_블랙리스트만 설정/로데이터 모음")
folder <- "~/0_제주_EVI_BN파일준비/0_2_데이터테이블준비_보강분석/6_BN분석/11_블랙리스트만 설정/로데이터 모음/"  
# path to folder that holds multiple .csv files
file_list <- list.files(path=folder, pattern="*.csv") 
# create list of all .csv files in folder
# read in each .csv file in file_list and create a data frame with the same name as the .csv file
for (i in 1:length(file_list)){
  assign(file_list[i], 
      read.csv(paste(folder, file_list[i], sep=''))
         )}

a_chim_test <- chim_test_npn.csv[3:12] 
a_chim_train <- chim_train_npn.csv[3:12] 
a_hon_test <- hon_test_npn.csv[3:12] 
a_hon_train <- hon_train_npn.csv[3:12] 
a_hwal_test <- hwal_test_npn_2.csv[3:12] 
a_hwal_train <- hwal_train_npn.csv[3:12] 

#3. 베이지안 네트워크 Blacklist 설정 
Blacklist_re <- matrix(c(
  "EVIavg","FD",
  "EVIavg","GSL",
  "EVIavg","HR",
  "EVIavg","HW",
  "EVIavg","RX5",
  "EVIavg","SDII",
  "EVIavg","SPEI12",
  "EVIavg","SU",
  "EVIavg","LC") ,,2,byrow=TRUE)
colnames(Blacklist_re)<-c("from","to")
Blacklist_re

Whitelist_re <- matrix(c(
  "LC","EVIavg") ,,2,byrow=TRUE)
colnames(Whitelist_re)<-c("from","to")
Whitelist_re

#4. mmhc algorithm.
##re-do the mmhc algorithm.
mmhc_chim_train <- mmhc(a_chim_train, whitelist = Whitelist_re, blacklist = Blacklist_re)
bnlearn:::print.bn(mmhc_chim_train)
plot(mmhc_chim_train)
Labels <- c("EVIavg","FD","GSL","HR","HW","RX5","SDII","SPEI12","SU","LC2")
graph_chim_train <- qgraph(mmhc_chim_train, nodeNames=Labels, legend.cex=0.5)
fit_chim_train <- bn.fit(mmhc_chim_train, a_chim_train)
fit_chim_train$EVIavg
fit_chim_train

mmhc_hon_train <- mmhc(a_hon_train, whitelist = Whitelist_re, blacklist = Blacklist_re)
bnlearn:::print.bn(mmhc_hon_train)
plot(mmhc_hon_train)
graph_hon_train <- qgraph(mmhc_hon_train, nodeNames=Labels, legend.cex=0.5)
fit_hon_train <- bn.fit(mmhc_hon_train, a_hon_train)
fit_hon_train$EVIavg
fit_hon_train

mmhc_hwal_train <- mmhc(a_hwal_train, whitelist = Whitelist_re, blacklist = Blacklist_re)
bnlearn:::print.bn(mmhc_hwal_train)
plot(mmhc_hwal_train)
graph_hwal_train <- qgraph(mmhc_hwal_train, nodeNames=Labels, legend.cex=0.5)
fit_hwal_train <- bn.fit(mmhc_hwal_train, a_hwal_train)
fit_hwal_train$EVIavg
fit_hwal_train


