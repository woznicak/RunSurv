#' function to propensity score matching
#' @export
#'

propensity_matching_single_ <- function(data,
                                       vars_to_matching,
                                       var_treat,
                                       control_indicator = 0,
                                       ps_method = 'matching',
                                       ...){


  formula_to_pms <- as.formula(paste0(var_treat,'~', paste0(vars_to_matching, collapse = '+')))

  data_sel <- data[, c(var_treat, vars_to_matching)]
  rownames(data_sel) <- 1:nrow(data_sel)

  if(!is.logical(data_sel[, var_treat])){
    data_sel[, var_treat] <- as.logical(data_sel[, var_treat] == control_indicator)
  }
  if(ps_method == 'matching'){
    match.it <- MatchIt::matchit(formula_to_pms,
                                 data = data_sel,
                                 ...)
    sel_cases <- as.numeric(unique(c(match.it$match.matrix, rownames(match.it$match.matrix))))

    df_psm <- data[sel_cases,]
  }else{
    # match_model <- mgcv::gam(formula = formula_to_pms,
    #           family = binomial("logit"),
    #           data = data_sel,
    #           ...)

    match_model <- glm(formula = formula_to_pms,
                             family = 'binomial',
                             data = data_sel)
    proba_pms <- predict(match_model, data_sel, type = "response")

    df_psm <- cbind(data, weight_pms = as.numeric(data_sel[, var_treat])/proba_pms + (1 - as.numeric(data_sel[, var_treat]))/(1-proba_pms))

  }


  return(df_psm)
}


#' function to propensity score matching
#' @export
#'
propensity_matching <- function(x,
                                vars_to_matching,
                                var_treat,
                                var_group = NULL,
                                control_indicator = 0,
                                ps_method = 'matching',
                                ...){

  if(!is.null(var_group)){
    uniq_groups <- unique(x$data[, var_group])
    data_split <- split(x$data, x$data[, var_group])

    psm_df_list <- lapply(data_split, function(one_group_df) propensity_matching_single_(data = one_group_df,
                                                               vars_to_matching = vars_to_matching,
                                                               var_treat = var_treat,
                                                               control_indicator = control_indicator,
                                                               ps_method = ps_method,
                                                               ...))
    # psm_df <- psm_df_list

    psm_df <- do.call(rbind, psm_df_list)


  }else{
    psm_df <- propensity_matching_single_(data = x$data,
                                vars_to_matching = vars_to_matching,
                                var_treat = var_treat,
                                control_indicator = control_indicator,
                                ps_method = ps_method,
                                ...)


  }


  return(psm_df)


}








