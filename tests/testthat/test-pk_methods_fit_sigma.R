test_that("fit_sigma.pk works", {
  this_test_pk <- pk(subset(cvt, analyzed_chem_dtxsid %in% "DTXSID2021781" & species %in% "mouse"))
  this_test_pk <- this_test_pk +
    settings_optimx("bobyqa") +
    settings_preprocess(suppress.messages = TRUE)
  this_test_pk <- do_fit.pk(this_test_pk)
  this_test_fits <- get_data.pk(this_test_pk)
  this_test_fits$Predictions <- this_test_fits$Conc * 2 / this_test_fits$Time
  expect_no_error(this_sigma <- fit_sigma.pk(this_test_pk, this_test_fits, pred_col = "Predictions"))
  expect_false(
    identical(
      this_sigma$ext_fits.Predictions$sigma_fits$hyperparam_value,
      mean(unlist(this_sigma$fit[grepl("sigma_", this_sigma$fit$param_name), ][["estimate"]]))
    )
  )


})
