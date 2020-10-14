validate_time <- function(df, time_column){
  stopifnot(length(time_column) == 1)
  stopifnot(time_column %in% colnames(df))
  stopifnot(is.numeric(df[,time_column]))

}


validate_event <- function(df, event_column){
  stopifnot(length(event_column) == 1)
  stopifnot(event_column %in% colnames(df))
  stopifnot(is.logical(df[,event_column]))

}

validate_descriptive <- function(df, descriptive_columns){
  stopifnot(all(descriptive_columns %in% colnames(df)))




}
