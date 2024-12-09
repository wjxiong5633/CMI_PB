##################################################################
library(h2o)
library(tidyverse)
h2o_multiple_imputation <- function(data, n_imputations = 100,i) {
  # List to store all imputed datasets
  all_imputations <- list()
  tryCatch({
    # Initialize H2O once
    h2o.init(max_mem_size = "32g", nthreads = -1)
    # Store original data and NA positions
    original_data <- data
    na_mask <- is.na(original_data)
    # Convert data to H2O frame
    if (is.data.frame(data)) {
      data_h2o <- as.h2o(data)
    } else {
      data_h2o <- as.h2o(as.data.frame(data))
    }
    
    # Fit autoencoder once with dropout
    autoencoder_model <- h2o.deeplearning(
      training_frame = data_h2o,
      x = colnames(data_h2o),
      autoencoder = TRUE,
      hidden = c(round(ncol(data)/2), 
                 round(ncol(data)/4), 
                 round(ncol(data)/2)),
      activation = "TanhWithDropout",  # Changed to TanhWithDropout
      hidden_dropout_ratios = c(0.2, 0.2, 0.2),
      input_dropout_ratio = 0.1,
      epochs = 100,
      seed = 1234,
      reproducible = TRUE,
      force_load_balance = FALSE  # For reproducibility
    )
    saveRDS(autoencoder_model,paste0("./imputed_data/R_autoencoder/model_time",timept[i],".rds"))
    # Generate multiple imputations
    for(ii in 1:n_imputations) {
      message(sprintf("Generating imputation %d of %d", ii, n_imputations))
      result <- original_data[,colSums(!is.na(original_data))>0]
      # Set different seed for each imputation
      set.seed(1234 + ii)
      # Get predictions with dropout enabled
      pred <- as.data.frame(h2o.predict(autoencoder_model, data_h2o))
      # Only replace missing values for each column
      pred[!na_mask] <- result[!na_mask]
      saveRDS(pred,paste0(".imputed_data/R_autoencoder/time",timept[i],"/","time",timept[i],"_impute",ii,".rds"))
      all_imputations[[ii]] <- pred
    }
    h2o.rm(autoencoder_model)
    
    # Clean up H2O
    h2o.shutdown(prompt = FALSE)
  }, error = function(e) {
    message(sprintf("Error: %s", e$message))
    h2o.shutdown(prompt = FALSE)
    return(NULL)
  })
  # Return list of all imputations
  return(all_imputations)
}



###############################################################
B <- readRDS("./imputed_data/time_needimpute/time0_count.rds")
B2 <- B #[,1:3]

out<- h2o_multiple_imputation(B2,100,1)

