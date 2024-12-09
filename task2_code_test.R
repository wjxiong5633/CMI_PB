
##------------------------------------------task2  prediction   missMDA + lasso + rf
set.seed(123)
timelist_id = readRDS("./imputed_data/timelist_id.rds")
combine_df <- readRDS("./task_data/combine_df.rds")
ids = combine_df %>% dplyr::select(dataset,type,subject_id,specimen_id)
baseline_df = combine_df[,1:15] %>% filter(timepoint == 0) %>% 
  mutate(age = round(as.numeric(difftime(   as.Date(date_of_boost),  as.Date(year_of_birth), units = "days"))/365)) %>% 
  dplyr::select(-type,-specimen_id,-date_of_boost,-year_of_birth,-visit,-timepoint,-specimen_type,-dataset,-planned_day_relative_to_boost)

## train, test, test subject id
train_id = ids[ids$dataset %in% c("2020_dataset","2021_dataset"),]
valid_id = ids[ids$dataset == "2022_dataset",]
test_id = ids[ids$dataset == "2023_dataset",]



time0 <- readRDS("./imputed_data/R_missMDA/time0_mean_count.rds")
task2_train_y_IgG_fc<- readRDS("./task_data/task2/task2_train_y_IgG_fc.rds") %>% dplyr::select(subject_id,foldchange)
task2_y_id = task2_train_y_IgG_fc$subject_id

y_data = task2_train_y_IgG_fc 
x_data = data.frame(subject_id = timelist_id[["0"]][,c("subject_id")], time0)

y_data_train = y_data %>% filter(subject_id %in% c(train_id$subject_id,test_id)) %>% filter(subject_id %in% task2_y_id)


traindata_raw = left_join(y_data_train,x_data, by = c("subject_id")) 
names(traindata_raw) = make.names(names(traindata_raw))
dim(traindata_raw)

testdata_raw = x_data %>% filter(subject_id %in% test_id$subject_id)
names(testdata_raw) = make.names(names(testdata_raw))
dim(testdata_raw)

# Fit a Lasso model with cross-testation to select features
library(glmnet)
# Convert the categorical variable to dummy variables
baseline_d = baseline_df   ## %>% filter(subject_id%in%traindata_raw$subject_id)
data_dummies = model.matrix(~.-1, data = baseline_d[,-1]) %>% as.data.frame()
data_dummies$subject_id = baseline_df$subject_id
train_bind = left_join(traindata_raw,data_dummies,by = c("subject_id"))
test_bind = left_join(testdata_raw,data_dummies,by = c("subject_id"))

X = as.matrix(train_bind[,-c(1:2)]) ## as.matrix(traindata_raw[,-c(1:3)])
y = traindata_raw$foldchange
lasso_cv <- cv.glmnet(X,y, alpha = 1)
best_lambda <- lasso_cv$lambda.min
best_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
best_coef <- coef(best_model) 
select_features_coef = best_coef[which(best_coef!= 0),]
select_features= names(best_coef[which(best_coef!= 0),])[-1]

traindata = train_bind[,c("foldchange",select_features)]
testdata = test_bind[,c("subject_id",select_features)]

library(caret)
library(randomForest)
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-testation


#######################################################################################################
############################# Train the Random Forest model using caret ###############################
#######################################################################################################
model <- train(foldchange ~ ., data = traindata, method = "rf", trControl = ctrl)

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


pred_train_y <- predict(model, newdata = traindata)
pred_train = cbind(subject_id = traindata_raw$subject_id, pred_train_y) %>% as.data.frame %>% 
  mutate(rank = rank(-pred_train_y, ties.method = "min")) %>% 
  arrange(subject_id)
pred_train_order = pred_train$rank


actual_train = traindata_raw %>% 
  mutate(rank = rank(-foldchange, ties.method = "min")) %>% 
  arrange(subject_id) %>% 
  dplyr::select(subject_id,foldchange,rank)

actual_train_order = actual_train$rank
cor(pred_train_order,actual_train_order,method = "spearman")
cor.test(pred_train_order,actual_train_order,method  = "spearman")



# Make predictions on the test set
pred_test_y <- predict(model, newdata = testdata)
pred_test = cbind(subject_id = testdata_raw$subject_id, pred_test_y) %>% as.data.frame %>% 
  mutate(rank = rank(-pred_test_y, ties.method = "min")) %>% 
  arrange(subject_id)
pred_test_order = pred_test$rank



## read tsv
result <- read.table("./final_version/result_WJ.tsv", header = TRUE, sep = "\t")
result$X1.2..IgG.PT.D14.FC.Rank = pred_test_order
write.table(result, "./final_version/result_WJ.tsv", sep = "\t", quote = FALSE, row.names = FALSE)
