context("Check create formula and extraction functions")

source("helper_objects.R")
# library(survival)

test_that("Check formula type", {

  lung_survData <- survData(data = lung,
                            time_column = 'time',
                            event_column = 'status')
  surv_fm <- create_surv_formula(lung_survData, variables = c('ph.ecog', 'sex'))
  expect_is(surv_fm, 'formula')
  expect_equal(surv_fm, as.formula('survival::Surv(time, status)~ph.ecog + sex'))

  surv_fm_strata <- create_surv_formula(lung_survData, variables = c('ph.ecog'), strata_variables = 'sex')
  expect_is(surv_fm_strata, 'formula')
  expect_equal(surv_fm_strata, as.formula('survival::Surv(time, status)~ph.ecog+survival::strata(sex)'))

})


test_that("Extraction formula from survival models", {

  surv_fm_2 <- create_surv_formula(lung_survData, variables = c('ph.ecog', 'sex', 'ph.karno'))
  surv_fit_model <- survminer::surv_fit(surv_fm, data = lung_survData$data)
  surv_fit_model_list <- survminer::surv_fit(list(surv_fm, surv_fm_2),data = lung_survData$data)
  survfit_model <- survival::survfit(surv_fm, data = lung_survData$data)

  coxph_model <- survival::coxph(surv_fm, data = lung_survData$data)


  expect_is(extract_formula(coxph_model), 'formula')
  expect_is(extract_formula(surv_fit_model), 'formula')
  expect_is(extract_formula(survfit_model), 'formula')

})

test_that("Extraction formula from list of models", {
  surv_fm_2 <- create_surv_formula(lung_survData, variables = c('ph.ecog', 'sex', 'ph.karno'))
  surv_fit_model <- survminer::surv_fit(surv_fm, data = lung_survData$data)
  surv_fit_model_list <- survminer::surv_fit(list(surv_fm, surv_fm_2),data = lung_survData$data)
  survfit_model <- survival::survfit(surv_fm, data = lung_survData$data)

  expect_is(extract_formula(surv_fit_model_list), 'list')
  expect_length(extract_formula(surv_fit_model_list), 2)
})


test_that("Extraction variables from formulas", {

  surv_fm_2 <- create_surv_formula(lung_survData, variables = c('ph.ecog', 'sex', 'ph.karno'))
  surv_fit_model <- survminer::surv_fit(surv_fm, data = lung_survData$data)
  surv_fit_model_list <- survminer::surv_fit(list(surv_fm, surv_fm_2),data = lung_survData$data)
  survfit_model <- survival::survfit(surv_fm, data = lung_survData$data)
  expect_equal(get_variables_surv_model(surv_fit_model), c('ph.ecog', 'sex'))

  expect_equal(get_variables_surv_model(surv_fit_model_list), list(c('ph.ecog', 'sex'),
                                                                  c('ph.ecog', 'sex', 'ph.karno')))

})
