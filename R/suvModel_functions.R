#' function to build model
#'@export
build_surv_model <- function(x,
                          variables = x[['descriptive_columns']],
                          strata_variables = NULL,
                  model_type = c('survfit', 'logrank', 'coxph'),
                  group_by = NULL,
                          ...){
  library(survival)

  model_type <- match.arg(model_type)
  model_function <- switch(model_type,
         survfit = survminer::surv_fit,
         logrank = survival::survdiff,
         coxph = survival::coxph)

  surv_formula <- create_surv_formula(x, variables, strata_variables)
  if(is.null(group_by)){
    model_function(formula = surv_formula, data = x$data)

  }else{
    data_split <- split(x$data, x$data[, group_by], drop = TRUE)

    names(data_split) <- sapply(lapply(strsplit(names(data_split), '\\.'), function(x) paste(group_by, x, sep = ':')),
           function(y) paste0(y, collapse = '+'))
    lapply(data_split, function(data_i) model_function(formula = surv_formula, data = data_i))
  }

}


#' function to build model
#'@export
extract_signif_vars_cox <- function(x,
                             variables = x[['descriptive_columns']],
                             strata_variables = NULL,
                             group_by = NULL,
                             thershold = 0.05,
                             ...){
  univariate_results_pvalue <- list()
  univariate_results_coeff <- list()
  for(var in variables){
    uni_cox_model <- build_surv_model(x, variables = var,
                     strata_variables = strata_variables,
                     group_by = group_by,
                     model_type = 'coxph')
    #dfl <- c(list(x), list(...))
    if(inherits(uni_cox_model, 'coxph')){
      uni_cox_model <- list(all = uni_cox_model)
    }

    univariate_results_pvalue[[var]] <- lapply(uni_cox_model,
                                         function(model) pvalue = summary(model)$logtest['pvalue'])
    univariate_results_coeff[[var]] <- lapply(uni_cox_model, coefficients)


  }

  univariate_results_pvalue_bind <- as.data.frame(do.call(rbind, univariate_results_pvalue))

  significant_vars_list <- list()
  for(group in colnames(univariate_results_pvalue_bind)){
    significant_vars_list[[group]] <- rownames(univariate_results_pvalue_bind)[univariate_results_pvalue_bind[,group] <= thershold]
  }

 return(list(pvalue = univariate_results_pvalue_bind,
             coefficients = univariate_results_coeff,
             significant_vars_list = significant_vars_list))


}

#' function to compare logrank pairwise
#'@export
build_pairwise_logrank <- function(x,
                             variables = x[['descriptive_columns']],
                             strata_variables = NULL,
                             group_by = NULL,
                             pivot_wider = FALSE,
                             ...){


  surv_formula <- create_surv_formula(x, variables, strata_variables)
  if(is.null(group_by)){
    pairwise_comparison <- survminer::pairwise_survdiff(formula = surv_formula, data = x$data, ...)
    pairwise_comparison_df <- broom::tidy(pairwise_comparison)
    pairwise_comparison_df$subset <- 'ALL'

  }else{
    data_split <- split(x$data, x$data[, group_by], drop = TRUE)

    names(data_split) <- sapply(lapply(strsplit(names(data_split), '\\.'), function(x) paste(group_by, x, sep = ':')),
                                function(y) paste0(y, collapse = '+'))
    pairwise_comparison_list <- lapply(data_split, function(data_i) survminer::pairwise_survdiff(formula = surv_formula, data = data_i, ...))
    pairwise_comparison_df <- do.call( rbind, lapply(pairwise_comparison_list, broom::tidy))
    pairwise_comparison_df$subset <- gsub(x = rownames(pairwise_comparison_df), pattern = '\\..+', '', perl = TRUE)
  }

  pairwise_comparison_df <- pairwise_comparison_df[, c('subset', 'group1', 'group2', 'p.value')]
  if(pivot_wider){
    pairwise_comparison_df <- tidyr::pivot_wider(pairwise_comparison_df,
                                                 id_cols = c('subset', 'group1'),
                                                 names_from = 'group2',
                                                 values_from = 'p.value')
  }
return(pairwise_comparison_df)

}

