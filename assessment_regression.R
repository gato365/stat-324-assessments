summarize_reg_model <- function(model,model_description) {
  # Get number of observations and predictors
  n <- length(residuals(model))
  p <- length(coefficients(model)) - 1
  
  # 0) RSS 
  RSS <- round(sum(residuals(model)^2),2)
  
  # 1) RSE
  RSE <- round(sqrt(RSS / (n - p - 1)),2)
  
  # 2) R-Squared and Adjusted R-Squared
  R2 <- round(summary(model)$r.squared,2)
  adj_R2 <- round(summary(model)$adj.r.squared,2)
  
  # 3) AIC
  aic_val <- round(AIC(model),2)
  
  # 4) BIC
  bic_val <- round(BIC(model),2)
  
  # Create summary dataframe
  mlr_metrics <- data.frame(
    type = model_description,
    RSS = RSS,
    RSE = RSE,
    R2 = R2,
    Adj_R2 = adj_R2,
    AIC = aic_val,
    BIC = bic_val
  )
  
  return(mlr_metrics)
}