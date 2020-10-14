#' function to create surv formula
#'@export
create_surv_formula <- function(x,
                                variables = NULL,
                                strata_variables = NULL){

  if(!is.null(strata_variables)){
    variables <- setdiff(variables, strata_variables)
  }
  descriptive_formula <- paste0(variables, collapse = '+')
  # strata_formula <- switch(is.null(strata_variables) + 1, paste0('+', paste0(paste0('survival::strata(', strata_variables, ')'), collapse = '+')),  NULL)
  strata_formula <- switch(is.null(strata_variables) + 1, paste0('+', paste0(paste0('strata(', strata_variables, ')'), collapse = '+')),  NULL)


  as.formula(paste0('survival::Surv(', x$time_column, ',', x$event_column,') ~ ', descriptive_formula, strata_formula))

}

#' function to create table1 formula
#'@export
create_table_formula <- function(x,
                                 variables = x[['descriptive_columns']],
                                 strata_variables = NULL){


  if(!is.null(strata_variables)){
    variables <- setdiff(variables, strata_variables)
  }
  row_formula <- paste0(variables, collapse = '+')
  column_formula <- switch(is.null(strata_variables) + 1, paste0('|', paste0(strata_variables, collapse = '*')),  NULL)

  formula_to_table1 <- as.formula(paste0('~', row_formula, column_formula))
  formula_to_table1


}


#' function to extract formula from model
#'@export
extract_formula <- function(surv_model){
  ## if single model
  if (inherits(surv_model, 'survfit') | inherits(surv_model, 'coxph') ){
    ## if model comes from survminer::surv_fit
    if(inherits(surv_model$call$formula, 'formula')){
      surv_formula <- surv_model$call$formula
    }else{
      ### if model comes from survival
      surv_formula <- get(deparse(surv_model$call$formula))
    }
  }

  # if list of surv model from survminer
  if (inherits(surv_model, 'list') & all(sapply(surv_model, function(x) inherits(x, 'survfit')))){
    surv_formula <- lapply(surv_model, function(x) x$call$formula)

  }
  return(surv_formula)
}


#' function to create table1 formula
#'@export
get_variables_surv_formula <- function(surv_formula){
  unlist(strsplit(as.character(surv_formula)[3], '\\s+\\+\\s+'))
}

#' function to create table1 formula
#'@export
get_variables_surv_model <- function(surv_model){


  surv_formula <- extract_formula(surv_model)

  if(!is.list(surv_formula)){
    get_variables_surv_formula(surv_formula)
  }else{
    unname(lapply(surv_formula, get_variables_surv_formula))
  }

}
