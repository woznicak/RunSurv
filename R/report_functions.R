#' generate report rmd
#' @export
#'
makeSurvReport <- function(x,
                           output_file,
                           single_variables,
                           main_variable = NULL,
                           group_variables = NULL,
                           strata_variables = NULL,
                           OS_rate_timepoint = 5*365.25,
                           path_to_script = NULL,
                           ignored_variables = NULL,
                           xlsx_path = NULL){

  file <- system.file("autoReport_template.Rmd", package = 'RunSurv')
  rmarkdown::render(input = file,
                    output_file = output_file,
                    params = list(data_surv = x,
                                  main_variable = main_variable,
                                  important_single_variables = single_variables,
                                  group_variables = group_variables,
                                  strata_variables = strata_variables,
                                  path_to_script = path_to_script,
                                  OS_rate_timepoint = OS_rate_timepoint,
                                  ignored_variables = ignored_variables,
                                  xlsx_path = xlsx_path))
}
