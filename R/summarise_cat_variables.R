pval_format_txt <- function(pvalue, threshold = 1e-04){
  pval.txt <- ifelse(pvalue < threshold, paste("p < ",format(threshold, scientific = FALSE)),
                     paste("p =", signif(pvalue, 2)))
  pval.txt
}




extract_OS_rate_logrank_ <- function(model,
                                     timepoints,
                                     digits = 2,
                                     OS_name = paste(as.character(timepoints), 'days')){
  # browser()

  OS_rate_list <- tryCatch({lapply(timepoints,
                                   function(timepoint_i) round(summary(model,
                                                                       times = c(timepoint_i),
                                                                       extend = TRUE)$surv,
                                                               digits))},
                           error = function(e) return(as.list(rep(NA, length.out = length(timepoints)))))
  OS_rate_df <-  data.frame(OS_rate_list)
  colnames(OS_rate_df) <- OS_name

  if(is.null(model$strata)){
    #extract variable from formula
    variable <- as.character(model$call$formula[[3]])

    OS_rate_df[,'variable'] <- paste0(variable, '=', unique(model$call$data[, variable]))
    OS_rate_df[,'cnt'] <- nrow(model$call$data)
  }else{
    OS_rate_df[,'variable'] <- names(model$strata)
    OS_rate_df[,'cnt'] <- model$n
  }


   OS_rate_df <- OS_rate_df[, c('variable', 'cnt', OS_name)]
  return(OS_rate_df)
}



#' @export
extract_OS_rate <- function(x,
                            variable,
                            times,
                            group_by = NULL,
                            OS_name = paste(as.character(times), 'days'),
                            common_legend = TRUE,
                            simplify_label = FALSE){

  stopifnot(is.factor(x$data[, variable]))
  stopifnot(length(variable) == 1)

  #
  KM_model <- build_surv_model(x, variables = variable, group_by = group_by,model_type = 'survfit')
  no_levels_var <- nlevels(x$data[,variable])
  KM_palette = RColorBrewer::brewer.pal(max(no_levels_var, 3),'Set1')
  names(KM_palette) <- levels(x$data[, variable])

  ## simplify label
  if(simplify_label){
    new_label <- gsub(paste0(paste0('(', group_by, ':)'), collapse = '|'), '', names(KM_model), perl = TRUE)
    names(KM_model) <- new_label
  }



  ## extract OS_rate
  ## logrank_model- can be one model or list of models
  if(inherits(KM_model, 'survfit') ){
    # browser()
    KM_palette_single_model <- unname(KM_palette[levels(droplevels(x$data[, variable]))])

    OS_rate <- extract_OS_rate_logrank_(KM_model, times, digits = 2)
    # if(is.null(x$weights)){
    #   p_value_KM_curve <-  survminer::surv_pvalue(KM_model, x$data)$pval
    # }else{
    #   p_value <- build_surv_model(x, variables = variable, model_type = 'logrank')$p.value
    #   p_value_KM_curve <-ifelse(is.na(p_value), FALSE, p_value)
    # }

    p_value <- build_surv_model(x, variables = variable, model_type = 'logrank')$p.value
    p_value_KM_curve <- pval_format_txt(p_value)
    p_value_KM_curve <-ifelse(is.na(p_value_KM_curve), FALSE, p_value_KM_curve)



    KM_plots <- survminer::ggsurvplot(KM_model, x$data,
                                           pval = p_value_KM_curve,
                                           surv.scale = 'percent',
                                           xscale = 'd_y',
                                           xlab="Time in years",
                                      conf.int = TRUE,
                                      palette = KM_palette_single_model
                                      )$plot+
      ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1))
    legend_object <- ggpubr::get_legend(KM_plots)

  }else if (inherits(KM_model, 'list')){

       # browser()
    OS_rate_list <- lapply(KM_model, function(model) extract_OS_rate_logrank_(model, times, digits = 2))
    OS_rate <- do.call(rbind, OS_rate_list)
    group_column_vec <- gsub(x = rownames(OS_rate), pattern = '\\..+', '', perl = TRUE)
    rownames(OS_rate) <- NULL
    OS_rate <- cbind(group = group_column_vec, OS_rate)


    OS_rate <- tidyr::pivot_wider(OS_rate, names_from = 'variable', values_from = c(OS_name, 'cnt'), values_fill = list(cnt = 0))
    OS_rate <- tidyr::unite(OS_rate, 'cnt',starts_with('cnt'), sep = '-')
    colnames(OS_rate) <- gsub('variable_', '' ,colnames(OS_rate))


    df_split <- lapply(KM_model, function(x) x$call$data)
    # p_value_KM_curve <- do.call(rbind, survminer::surv_pvalue(KM_model, df_split))
     # browser()
      logrank_to_pvalue <- build_surv_model(x,
                       variables = variable,
                       strata_variables = NULL,
                       model_type =  'logrank',
                       group_by = group_by)
      ## simplify label
      if(simplify_label){
        # new_label <- gsub(paste0(paste0('(', group_by, ':)'), collapse = '|'), '', names(KM_model), perl = TRUE)
        names(logrank_to_pvalue) <- new_label
      }
      logrank_to_pvalue <- lapply(logrank_to_pvalue, data.frame)

      p_value_KM_curve <- do.call(rbind, logrank_to_pvalue)
      p_value_KM_curve <- subset(p_value_KM_curve, select = -chisq)
      p_value_KM_curve <- cbind(group = rownames(p_value_KM_curve), p_value_KM_curve)
      p_value_KM_curve$p.value <- pval_format_txt(p_value_KM_curve$p.value )

      rownames(p_value_KM_curve) <- NULL
      p_value_list <- lapply(logrank_to_pvalue, function(logrank_to_pvalue_i) if(is.na(logrank_to_pvalue_i$p.value)) FALSE
                                                                                 else pval_format_txt(logrank_to_pvalue_i$p.value))


    OS_rate <- merge(OS_rate, p_value_KM_curve[, c('group', 'p.value')], by = 'group')
      # browser()

    KM_palette_list <- lapply(df_split, function(x) unname(KM_palette[levels(droplevels(x[, variable]))]))


    # apply
    KM_plots <- mapply(FUN = function(fit, data, palette, title, p.value, ...)
        survminer::ggsurvplot(fit = fit, data = data, palette = palette, title = title, pval = p.value,...)$plot+
          ggplot2::guides(colour = ggplot2::guide_legend(ncol = 1)),
                       KM_model, df_split, KM_palette_list, names(KM_model), p_value_list,
                       MoreArgs = list(surv.scale = 'percent',
                                       xscale = 'd_y',
                                       xlab="Time in years",
                                       conf.int = TRUE),
                       SIMPLIFY = FALSE)

    if(common_legend){
      KM_model_all_data <- build_surv_model(x, variables = variable, group_by = NULL,model_type = 'survfit')
      KM_curve_all_data <- survminer::ggsurvplot(KM_model_all_data, x$data, palette = unname(KM_palette))
      legend_object <- ggpubr::get_legend(KM_curve_all_data)
    }


  }
  return(list(OS_table = OS_rate,
              logrank_pvalue_KM_curve = p_value_KM_curve,
              plots = KM_plots,
              legend_object = legend_object,
              KM_curves = KM_model
              # models = logrank_model
              ))

}





#' @export

summary_surv_variables <- function(x,
                                   variables = list(),
                                   times = 5* 365.25){


  flat_variables <- unique(unlist(variables))

  surv_fm <- create_surv_formula(x, variables = flat_variables)
  # surv_model <- survminer::surv_fit(surv_fm, data = x$data)
  surv_model <- survfit(surv_fm, data = x$data)

  summary_table <- extract_surv_table(surv_model, times = times)
  return(summary_table)
  # if(length(variables[['rows']]) > 0){
  #
  # }

}
