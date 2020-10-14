context("Check table1 functions")

source("helper_objects.R")
test_that("Check type of table1",{


  expect_is(create_table_formula(x = lung_survData), 'formula')
  expect_is(create_table1(x = lung_survData), 'table1')

  ## table1 with strata
  expect_is(create_table_formula(x = lung_survData, strata_variables = 'sex'), 'formula')
  expect_is(create_table1(x = lung_survData, strata_variables = 'sex'), 'table1')

})
