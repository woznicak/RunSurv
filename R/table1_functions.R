#' function to create surv formula
#'@export
create_table1 <- function(x,
                          variables = x[['descriptive_columns']],
                          strata_variables = NULL,
                          add_pvalue = TRUE,
                          overall = "Overall",
                          digits=2,
                          ...){
  formula_table1 <- create_table_formula(x, variables, strata_variables)
  weights_var <- x$weights
  df <- x$data



  # rndr.continous <- table1::render.continuous.default

  if(!is.null(strata_variables)){


    no_cat <- length(unique(df[, strata_variables]))
    uniq_cat <- sort(paste(unique(df[, strata_variables])))



    if(add_pvalue){
      df[, strata_variables] <- factor(df[, strata_variables], levels = c(uniq_cat, 'pvalue'))
      # function to add p-value
      # https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html#a-column-of-p-values
      render_pvalue <- function(x, name, render.continuous = render.continuous.default, ...) {
        # browser()
        if (length(x) == 0) {
          y <- df[[name]]
          s <- rep("", length(table1::render.default(x=y, name=name, ...)))
          if(no_cat > 1){
            if (is.numeric(y)) {
              if(no_cat >= 2){
                # p <- t.test(y ~ df[, strata_variables] )$p.value
                if(no_cat == 2){
                  p <- wilcox.test(y ~ df[, strata_variables] )$p.value
                }else{
                  p <- kruskal.test(y ~ df[, strata_variables] )$p.value
                }
              }

            } else if(length(unique( df[, strata_variables])) > 1){
              # browser()
              p <- tryCatch({chisq.test(table(y, droplevels(df[, strata_variables] )))$p.value},
                            error = function(e) return(NA))
            }
            s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
          }
          s
        } else {
          table1::render.default(x=x, name=name,
                                 #render.continuous = table1::render.continuous.default,
                                 ...)
        }
      }



      rndr <- render_pvalue



      rndr.strat <- function(label, n, ...) {
        ifelse(n==0, label, table1::render.strat.default(label, n, ...))
      }

      list_args <- list(x = formula_table1,
                        data=df, droplevels=FALSE, render=rndr, render.strat=rndr.strat, overall=overall, ...)

    }else{
      list_args <- list(x = formula_table1,
                        data=df, droplevels=FALSE, overall=overall,
                        # render.continuous = rndr.continous,
                        ...)
    }

  }else{
    list_args <- list(x = formula_table1,
                      data=x$data,
                      # render.continuous = rndr.continous,
                      ...)
  }

  do.call(table1::table1, list_args)


}
