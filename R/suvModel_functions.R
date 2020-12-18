#' function to build model
#'@export
logrank <- function(formula, data, weights = NULL){
   # browser()
  if(is.null(weights)){

    logrank_object <- tryCatch({
      logrank_object_r <- survival::survdiff(formula, data)
       list(chisq = logrank_object_r$chisq,
            p.value = pchisq(logrank_object_r$chisq, length(logrank_object_r$n) - 1, lower.tail = FALSE))

      },
      error = function(e) list(chisq = NA,
                               p.value = NA))


  }else{

    logrank_object <- tryCatch({
      data_cbind <- cbind(data, 'PSM_WEIGHTS' = weights)
      survey_design <- survey::svydesign(id=~1,variables=NULL, weights = ~PSM_WEIGHTS , data=data_cbind)
      logrank_object_r <- survey::svylogrank(formula, survey_design)
      list(chisq = logrank_object_r[[2]]$chisq,
                             p.value = logrank_object_r[[2]]$p)
    },
    error = function(e) list(chisq = NA,
                             p.value = NA))


  }
  logrank_object
}


#' function to build model
#'@export
build_surv_model_single_ <- function(x,
                             variables = x[['descriptive_columns']],
                             strata_variables = NULL,
                             model_type = c('survfit', 'logrank', 'coxph'),
                             ...){
    # browser()

  model_type <- match.arg(model_type)
  model_function <- switch(model_type,
                           survfit = survminer::surv_fit,
                           logrank = logrank,
                           coxph = survival::coxph)

  surv_formula <- create_surv_formula(x, variables, strata_variables)


  if(!is.null(x$weights)){

    model_function(formula = surv_formula, data = x$data, weights = x$data[[x$weights]], ...)
  }else{
    model_function(formula = surv_formula, data = x$data, ...)
  }


}



#' function to build model
#'@export
build_surv_model <- function(x,
                          variables = x[['descriptive_columns']],
                          strata_variables = NULL,
                  model_type = c('survfit', 'logrank', 'coxph'),
                  group_by = NULL,
                          ...){
  library(survival)
  # browser()

  if(is.null(group_by)){
    build_surv_model_single_ (x,
                              variables = variables,
                              strata_variables = strata_variables,
                              model_type = model_type,
                              ...)

  }else{
    data_split <- split(x, x$data[, group_by], drop = TRUE)

    names(data_split) <- sapply(lapply(strsplit(names(data_split), '\\.'), function(x) paste(group_by, x, sep = ':')),
           function(y) paste0(y, collapse = '+'))

    lapply(data_split, function(data_i)     build_surv_model_single_ (data_i,
                                                                      variables = variables,
                                                                      strata_variables = strata_variables,
                                                                      model_type = model_type,
                                                                      ...))

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

#' function to build model
#' @export
compare_levels <- function(x,
                           variables = x[['descriptive_columns']],
                           strata_variables = NULL,
                           p.adjust.method = "BH"){

  # DNAME <- paste(deparse(substitute(data)), "and", .collapse(group_var, sep = " + " ))
  DNAME <- 'df'
  METHOD <- "Log-Rank"


  var <- x$data[, variables[1]]
  var <- as.factor(var)
  surv_formula <- create_surv_formula(x, variables, strata_variables)

  compare_levels_pair <- function(i, j) {
     # browser()
    # .subset = (var %in% (levels(var))[c(i,j)]) & (x$data[, x$time_column]>0)
    .subset = (var %in% (levels(var))[c(i,j)])
    x_subset <- subset(x, .subset)
    sdif <- build_surv_model(x_subset,
                               variables = variables,
                               strata_variables = NULL,
                               model_type = 'logrank',
                               group_by = NULL)
    sdif$p.value


  }
  # browser()
  PVAL <- stats::pairwise.table(compare_levels_pair, levels(var), p.adjust.method)
  # PVAL
  res <- list(method = METHOD, data.name = DNAME, p.value = PVAL,
              p.adjust.method = p.adjust.method)

  class(res) <- "pairwise.htest"
  res
}







#' function to compare logrank pairwise
#'@export
build_pairwise_logrank <- function(x,
                             variables = x[['descriptive_columns']],
                             strata_variables = NULL,
                             group_by = NULL,
                             pivot_wider = FALSE,
                             p.adjust.method = "BH",
                             ...){


  surv_formula <- create_surv_formula(x, variables, strata_variables)
  if(length(unique(x$data[,variables]))< 3){
    p.adjust.method <- 'none'
  }
  # browser()

  if(is.null(group_by)){
    # browser()
    # pairwise_comparison <- survminer::pairwise_survdiff(formula = surv_formula, data = x$data, ...)
    pairwise_comparison <- compare_levels(x,
                                          variables = variables,
                                          strata_variables = strata_variables,
                                          p.adjust.method = p.adjust.method)
    pairwise_comparison_df <- broom::tidy(pairwise_comparison)
    pairwise_comparison_df$subset <- 'ALL'

  }else{
    data_split <- split(x, x$data[, group_by])

    names(data_split) <- sapply(lapply(strsplit(names(data_split), '\\.'), function(x) paste(group_by, x, sep = ':')),
                                function(y) paste0(y, collapse = '+'))

    pairwise_comparison_list <- lapply(data_split, function(data_i)
      compare_levels(data_i,
                     variables = variables,
                     strata_variables = strata_variables,
                     p.adjust.method = p.adjust.method))
    pairwise_comparison_df <- do.call( rbind, lapply(pairwise_comparison_list, broom::tidy))
    pairwise_comparison_df$subset <- gsub(x = rownames(pairwise_comparison_df), pattern = '\\..+', '', perl = TRUE)
  }

  pairwise_comparison_df <- pairwise_comparison_df[, c('subset', 'group1', 'group2', 'p.value')]
  pairwise_comparison_df$p.value <- pval_format_txt(pairwise_comparison_df$p.value )
  if(pivot_wider){
    pairwise_comparison_df <- tidyr::pivot_wider(pairwise_comparison_df,
                                                 id_cols = c('subset', 'group1'),
                                                 names_from = 'group2',
                                                 values_from = 'p.value')


  }
return(pairwise_comparison_df)

}

