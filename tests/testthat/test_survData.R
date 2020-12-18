context("Check survData() function")

source("helper_objects.R")

test_that("Type of survData", {
  lung_survData <- survData(data = lung,
                            time_column = 'time',
                            event_column = 'status')



  expect_is(lung_survData, "survData")
  expect_is(lung_survData$data, "data.frame")
  expect_is(lung_survData$time_column, "character")
  expect_is(lung_survData$event_column, "character")
  expect_is(lung_survData$descriptive_columns, "character")
})


test_that("Error in survData", {
  expect_error(survData(data = lung_error,
                            time_column = 'time',
                            event_column = 'status'))


})


test_that("Split function", {
  lung_survData <- survData(data = lung,
                            time_column = 'time',
                            event_column = 'status')

  lung_survData_split <- split(lung_survData, lung_survData$data$sex)

  expect_is(lung_survData_split, "list")
  expect_true(all(sapply(lung_survData_split, class) == "survData"))
  expect_length(lung_survData_split, 2)

})
