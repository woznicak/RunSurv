context("Check function to build models")

source("helper_objects.R")

test_that("Check type of models",{
  model_surv_one_var <- build_surv_model(x = lung_survData,
                                      variables = 'sex',
                               model_type = 'survfit')

  model_cox_one_var <- build_surv_model(x = lung_survData,
                                         variables = 'age',
                                         model_type = 'coxph')

  model_cox_more_var <- build_surv_model(x = lung_survData,
                                        variables = c('age', 'sex'),
                                        model_type = 'coxph')

  expect_is(model_surv_one_var, 'survfit')
  expect_is(model_cox_one_var, 'coxph')
  expect_is(model_cox_more_var, 'coxph')


})

test_that("Check type of strata models",{

  model_cox_one_var_strata <- build_surv_model(x = lung_survData,
                                        variables = 'age',
                                        strata = 'sex',
                                        model_type = 'coxph')

  # model_cox_more_var <- build_surv_model(x = lung_survData,
  #                                        variables = c('age', 'sex'),
  #                                        model_type = 'coxph')

  expect_is(model_cox_one_var_strata, 'coxph')
  # expect_is(model_cox_more_var, 'coxph')


})

test_that("Check type of list of group models",{

  model_logrank_one_var_list <- build_surv_model(x = lung_survData,
                                             variables = 'lump_var',
                                             group_by = 'sex',
                                             model_type = 'survfit')

  model_logrank_one_var_list_strata <- build_surv_model(x = lung_survData,
                                                    variables = 'lump_var',
                                                    strata_variables = 'ph.ecog',
                                                    group_by = 'sex',
                                                    model_type = 'survfit')

  model_cox_one_var_list <- build_surv_model(x = lung_survData,
                                               variables = 'age',
                                               group_by = 'sex',
                                               model_type = 'coxph')

  model_cox_one_var_list_strata <- build_surv_model(x = lung_survData,
                                             variables = 'age',
                                             strata_variables = 'ph.ecog',
                                             group_by = 'sex',
                                             model_type = 'coxph')

  expect_is(model_logrank_one_var_list, 'list')
  expect_true(all(sapply(model_logrank_one_var_list, class) == 'survfit' ))
  expect_length(model_logrank_one_var_list, 2)

  expect_is(model_logrank_one_var_list_strata, 'list')
  expect_true(all(sapply(model_logrank_one_var_list_strata, class) == 'survfit' ))
  expect_length(model_logrank_one_var_list_strata, 2)


  expect_is(model_cox_one_var_list, 'list')
  expect_true(all(sapply(model_cox_one_var_list, class) == 'coxph' ))
  expect_length(model_cox_one_var_list, 2)

  expect_is(model_cox_one_var_list_strata, 'list')
  expect_true(all(sapply(model_cox_one_var_list_strata, class) == 'coxph' ))
  expect_length(model_cox_one_var_list_strata, 2)





})


test_that("Check type of list of group models",{

  model_logrank_one_var_list <- build_surv_model(x = lung_survData,
                                                 variables = 'lump_var',
                                                 group_by = c('sex','ph.ecog'),
                                                 model_type = 'survfit')


  expect_is(model_logrank_one_var_list, 'list')
  expect_true(all(sapply(model_logrank_one_var_list, class) == 'survfit' ))

  ## sex:2 ph.ecog:3 => nrow=0
  expect_length(model_logrank_one_var_list, 7)
  expect_equal(names(model_logrank_one_var_list), c("sex:1+ph.ecog:0",
                       "sex:2+ph.ecog:0",
                       "sex:1+ph.ecog:1",
                       "sex:2+ph.ecog:1",
                       "sex:1+ph.ecog:2",
                       "sex:2+ph.ecog:2",
                       "sex:1+ph.ecog:3"))


})

test_that("Check univariate coxph",{
  signif_vars <- extract_signif_vars_cox (x = lung_survData,
                           variables = c('sex','ph.ecog','lump_var'))

  expect_is(signif_vars, 'list')
  expect_length(signif_vars, 3)

  expect_is(signif_vars$pvalue, 'data.frame')
  expect_is(signif_vars$coefficients, 'list')
  expect_is(signif_vars$significant_vars_list, 'list')


})
