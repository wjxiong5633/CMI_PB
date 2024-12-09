#===softimpute iteration 100 times====
library(softImpute)
library(dbplyr)

filepath<-"./imputed_data/time_needimpute"

timelist_id<-readRDS("./imputed_data/timelist_id.rds")

time0_data<-readRDS(paste0(filepath,"/time0_count.rds")) %>% as.data.frame()

filepath_save<-"./imputed_data/R_softImpute/time0_count"
data_need_impute<-t(time0_data)%>% as.matrix()
for(se in 1:100){
  # se<-1
  print(se)
  seed1<-se*24
  start_time <- Sys.time()
  Impute_1_softImpute<- softImpute(data_need_impute,rank.max =115, lambda = 30, type = c("als", "svd"), thresh = 1e-05,
                                   maxit = 100, trace.it = FALSE, warm.start = NULL, final.svd = TRUE)
  ximp=t(complete(data_need_impute,Impute_1_softImpute))
  # c<-t(Impute_1_missRanger[["u"]])
  end_time <- Sys.time()
  print(end_time - start_time)
  save(ximp,file=paste(filepath_save,"/SoftImpute_",se,".RData",sep=""))
}


