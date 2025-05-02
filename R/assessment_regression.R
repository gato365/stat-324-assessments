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
  aic_val <- round( n * log(RSS / n) + 2 * p,2)
  
  # 4) BIC
  bic_val <-  round(n * log(RSS / n) + log(n) * p,2)
  
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


plot_scatter_resid <- function(mod, data, title_prefix = NULL) {
  
  # 0.  Dependencies -------------------------------------------------
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("The ggplot2 package must be installed to use this function.")
  
  # 1.  Extract response & first predictor names --------------------
  vars <- all.vars(formula(mod))        # includes response first
  if (length(vars) < 2)
    stop("Model must have at least one explanatory variable.")
  
  y_name <- vars[1]
  x_name <- vars[2]                     # first explanatory variable
  
  # guard: do the requested columns exist in 'data'?
  if (!all(c(y_name, x_name) %in% names(data)))
    stop("Columns '", y_name, "' and/or '", x_name, "' not found in 'data'.")
  
  # 2a. Scatterplot with OLS line -----------------------------------
  
  
  p_scatter <- ggplot2::ggplot(
    data, ggplot2::aes_string(x = x_name, y = y_name)) +
    ggplot2::geom_point(color = "steelblue", size = 2) +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      x = x_name,
      y = y_name,
      title = paste0(
        if (!is.null(title_prefix)) paste0(title_prefix, ": \n ") else "",
        "Scatter Plot")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10))
    )
  
  # 2b. Residual‑vs‑fitted plot -------------------------------------
  resid_df <- data.frame(
    fitted    = fitted(mod),
    std_resid = rstandard(mod)
  )
  
  p_resid <- ggplot2::ggplot(
    resid_df, ggplot2::aes(x = fitted, y = std_resid)) +
    ggplot2::geom_point(color = "red", size = 2) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(labels = scales::comma) +
    ggplot2::labs(
      x = "Fitted values",
      y = "Standardized residuals",
      title = paste0(
        if (!is.null(title_prefix)) paste0(title_prefix, ": \n") else "",
        "Residuals vs. Fitted Plot")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title  = ggplot2::element_text(hjust = 0.5),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 10)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 10))
    )
  
  # 3.  Side‑by‑side layout -----------------------------------------
  if (requireNamespace("patchwork", quietly = TRUE)) {
    p_scatter + p_resid + patchwork::plot_layout(guides = "collect")
  } else if (requireNamespace("gridExtra", quietly = TRUE)) {
    gridExtra::grid.arrange(p_scatter, p_resid, ncol = 2)
  } else {
    message(
      "Install the 'patchwork' (recommended) or 'gridExtra' package ",
      "for automatic side‑by‑side display.\n",
      "Returning a list with the two ggplot objects instead.")
    list(scatter = p_scatter, residuals = p_resid)
  }
}