library(missMDA)

add_columnwise_noise <- function(data, noise_factor = 0.1, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # Initialize a copy of the data to perturb
  data_perturbed <- data
  
  # Iterate over each column
  for (col in names(data)) {
    # Identify observed (non-missing) indices
    observed_idx <- which(!is.na(data[[col]]))
    if (length(observed_idx) > 0){
      # Calculate the standard deviation of observed data
      col_sd <- sd(data[[col]][observed_idx], na.rm = TRUE)
      
      # Define the standard deviation of the noise to add
      noise_sd <- noise_factor * col_sd  # Adjust noise_factor as needed
      
      # Generate Gaussian noise
      noise <- rnorm(length(observed_idx), mean = 0, sd = noise_sd)
      
      # Add noise to observed data
      data_perturbed[[col]][observed_idx] <- data[[col]][observed_idx] + noise
    }
  }
  
  return(data_perturbed)
}


n.impute <- 100

################################################################################################  
# here change the timept
i <- 1
print(i)

################################################################################################  

B <- readRDS("./imputed_data/time_needimpute/time0_count.rds")

df2 <- B
for (j in 1:n.impute){
  print(j)
  df_perturbed <- add_columnwise_noise(df2, noise_factor = 0.1, seed = 123+j)
  # Estimate the number of principal components (ncp)
  # Adjust ncp.max based on your data; with 7000 columns, consider a reasonable upper limit
  #ncp_estimation <- estim_ncpPCA(df_perturbed, ncp.max = 50)  # Example: ncp.max = 20
  ncp <- 20 #ncp_estimation$ncp
  imputed_data <- imputePCA(df_perturbed, ncp = ncp)$completeObs
  imputed_data[!is.na(df2)] <- df2[!is.na(df2)]
  saveRDS(imputed_data,file = paste0("./imputed_data/R_missMDA/time",timept[i],"/missMDA_imputed_",j,".rds"))
}




