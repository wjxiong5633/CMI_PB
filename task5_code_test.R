
##------------------------------------------task5  prediction   softimpute + PCA + rf
set.seed(123)
timelist_id = readRDS("./imputed_data/timelist_id.rds")
combine_df <- readRDS("./weijia/CMI_PB/task_data/combine_df.rds")
ids = combine_df %>% dplyr::select(dataset,type,subject_id,specimen_id)
baseline_df = combine_df[,1:15] %>% filter(timepoint == 0) %>% 
  mutate(age = round(as.numeric(difftime(   as.Date(date_of_boost),  as.Date(year_of_birth), units = "days"))/365)) %>% 
  dplyr::select(-type,-specimen_id,-date_of_boost,-year_of_birth,-visit,-timepoint,-specimen_type,-dataset,-planned_day_relative_to_boost)

## train, test, test subject id
train_id = ids[ids$dataset %in% c("2020_dataset","2021_dataset"),]
valid_id = ids[ids$dataset == "2022_dataset",]
test_id = ids[ids$dataset == "2023_dataset",]


time0 <- readRDS("./imputed_data/R_softimpute/time0_mean_count.rds")
task5_train_y_CCL3 <- readRDS("./weijia/CMI_PB/task_data/task5/task5_train_y_CCL3.rds")
task5_y_id = task5_train_y_CCL3$subject_id

y_data = task5_train_y_CCL3 %>% dplyr::select(-type,-timepoint,-specimen_id) %>% 
  mutate(CCL3_count_time3 = countENSG00000277632.1) %>% 
  dplyr::select(subject_id,CCL3_count_time3)

x_data = data.frame(subject_id = timelist_id[["0"]][,c("subject_id")], time0)
y_data_train = y_data %>% filter(subject_id %in% train_id$subject_id) %>% filter(subject_id %in% task5_y_id) 

traindata_raw = left_join(y_data_train,x_data, by = c("subject_id")) 
names(traindata_raw) = make.names(names(traindata_raw))
dim(traindata_raw)

testdata_raw = x_data %>% filter(subject_id %in% test_id$subject_id)
names(testdata_raw) = make.names(names(testdata_raw))
dim(testdata_raw)





#######################################################################################################
############################# PCA and RF model ########################################################
#################################################################################################### ###
set.seed(123)
X = as.matrix(traindata_raw[,-c(1:2)])
y = traindata_raw$CCL3_count_time3
data <- X
target <- y

# Perform PCA
pca_result <- prcomp(data, scale. = TRUE)
train_features <- as.data.frame(pca_result$x[, 1:10])


train_features$subject_id = traindata_raw$subject_id
train_all = left_join(train_features,baseline_df,by = "subject_id")


test_pca <- predict(pca_result, newdata = as.matrix(testdata_raw[,-c(1)]))
test_features <- as.data.frame(test_pca[, 1:10])
test_features$subject_id = testdata_raw$subject_id
test_all = left_join(test_features,baseline_df,by = "subject_id")


ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-testation
model <- train(y = target, x = train_all %>% dplyr::select(-subject_id), method = "rf", trControl = ctrl)


# Extract feature importance
importance <- varImp(model)
# Print the feature importance
print(importance)

# Extract feature importance
importance <- varImp(model)$importance
# Create a data frame for plotting
importance_df <- data.frame(
  Feature = rownames(importance),
  Importance = importance[, "Overall"]
) %>% arrange(desc(importance)) %>% head(10)
# Sort the features by importance
importance_df <- importance_df[order(importance_df$Importance, decreasing = TRUE), ]
# Create a bar plot for feature importance
ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Feature Importance Plot",
       x = "Feature",
       y = "Importance") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


pred_train_y <- predict(model, newdata = train_all)
pred_train = cbind(subject_id = traindata_raw$subject_id, pred_train_y) %>% as.data.frame %>% 
  mutate(rank = rank(-pred_train_y, ties.method = "min")) %>% 
  arrange(subject_id) 

pred_train_order = pred_train$rank

actual_train = traindata_raw %>% 
  mutate(rank = rank(-CCL3_count_time3, ties.method = "min")) %>% 
  arrange(subject_id) %>% 
  dplyr::select(subject_id,CCL3_count_time3,rank)

actual_train_order = actual_train$rank

cor(pred_train_order,actual_train_order,method = "spearman")
cor.test(pred_train_order,actual_train_order,method  = "spearman")



# Make predictions on the test set
pred_test_y <- predict(model, newdata = test_all)
pred_test = cbind(subject_id = testdata_raw$subject_id, pred_test_y) %>% as.data.frame %>% 
  mutate(rank = rank(-pred_test_y, ties.method = "min")) %>% 
  arrange(subject_id) 
pred_test_order = pred_test$rank


## read tsv
result <- read.table("./weijia/CMI_PB/final_version/result_WJ.tsv", header = TRUE, sep = "\t")
result$X3.1..CCL3.D3.Rank = pred_test_order
write.table(result, "./weijia/CMI_PB/final_version/result_WJ.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
