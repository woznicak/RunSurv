context("Check OS table")

source("helper_objects.R")
test_that('check OS type',{

  OS_object_one_var <- extract_OS_rate(lung_survData,
                  variable = 'ph.ecog',
                  times = c(200))

  # OS_object_one_var_group <- extract_OS_rate(lung_survData,
  #                                     variable = 'ph.ecog',
  #                                     group = 'sex',
  #                                     times = c(200))

  expect_is(OS_object_one_var$OS_table, 'data.frame')
  # expect_is(OS_object_one_var_group$OS_table, 'data.frame')

})
