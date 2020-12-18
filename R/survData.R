
#' new class
#'
#' @param data A number.
#' @param time_column A number.
#' @param event_column event
new_survData <- function(data,
                         time_column,
                         event_column,
                         descriptive_columns,
                         factor_columns,
                         numeric_columns,
                         weights){



  structure(list(data = data,
                 time_column = time_column,
                 event_column = event_column,
                 descriptive_columns = descriptive_columns,
                 factor_columns = factor_columns,
                 numeric_columns = numeric_columns,
                 weights = weights),
            class = 'survData')

}

#' new class
#'
#' @param x A number.

validate_survData <- function(x){
  validate_time(x$data, x$time_column)
  validate_event(x$data, x$event_column)

}



#' new class
#'
#' @param data A number.
#' @param time_column A number.
#' @param event_column event
#' @param descriptive_columns variables to describe
#' @export
survData <- function(data,
                     time_column,
                     event_column,
                     descriptive_columns = NULL,
                     weights = NULL){
  # browser()
  stopifnot(is.data.frame(data))
  stopifnot(is.character(time_column))
  stopifnot(is.character(event_column))

  if(is.null(descriptive_columns)){
    descriptive_columns <- setdiff(colnames(data), c(time_column, event_column))
  }else{
    stopifnot(is.character(descriptive_columns))
  }
  type_of_columns <- unlist(lapply(data[,descriptive_columns], class))

  factor_columns <- descriptive_columns[type_of_columns == 'factor']
  numeric_columns <- descriptive_columns[type_of_columns %in% c('numeric', 'integer')]

  x <- new_survData(data,
                    time_column,
                    event_column,
                    descriptive_columns,
                    factor_columns,
                    numeric_columns,
                    weights)

  validate_survData(x)
  x

}

#' @export
split.survData <- function(x, f, ...){
  df_split <- split(x$data, f, ...)
  lapply(df_split, function(df_split_i) survData(data = df_split_i,
                                time_column = x$time_column,
                                event_column = x$event_column,
                                descriptive_columns = x$descriptive_columns,
                                weights = x$weights))


}

#' @export
subset.survData <- function(x, subset, ...){
  df_subset <- subset(x$data, subset = subset, ...)
 survData(data = df_subset,
          time_column = x$time_column,
          event_column = x$event_column,
          descriptive_columns = x$descriptive_columns,
          weights = x$weights)


}
