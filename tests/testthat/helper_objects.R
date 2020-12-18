
lung_error <- survival::lung

lung <- na.omit(survival::lung)
lung[,'status'] <- lung[,'status']==2
 lung[,'lump_var'] <- sample(c('a', 'b'), nrow(lung), replace = TRUE, prob = c(0.5, 0.5))
lung$ph.ecog <- as.factor(lung$ph.ecog)
lung$sex <- as.factor(lung$sex)
lung$lump_var <- as.factor(lung$lump_var)


lung_survData <- survData(data = lung,
                          time_column = 'time',
                          event_column = 'status')


surv_fm <- create_surv_formula(lung_survData, variables = c('ph.ecog', 'sex'))


## weighted
lung_weighted <- lung
lung_weighted$weights_psm <- predict(glm(lump_var~age+sex, lung, family = 'binomial'), type = 'response')
lung_weighted_survData <- survData(data = lung_weighted,
                          time_column = 'time',
                          event_column = 'status',
                          weights = 'weights_psm')
