defpt <- define_param_table(paramEst, paramKey)
gpn <- getPanelName(defpt)

test_that("getPanelName appropriately classifies residual variance panel [MPT-GPN-001]", {

  # Residual variance panel
  expect_equal(gpn$type[gpn$panel == "RV"], "Residual variance")
  expect_equal(gpn$type_f[gpn$panel == "RV"], 6)
})

test_that("getPanelName appropriately classifies interindividual covariance parameters panel [MPT-GPN-002]", {

  # Interindividual covariance parameters
  expect_equal(unique(gpn$type[gpn$OM == TRUE & gpn$diag == FALSE]), "Interindividual covariance parameters")
  expect_equal(unique(gpn$type_f[gpn$OM == TRUE & gpn$diag == FALSE]), 5)
})

test_that("getPanelName appropriately classifies interindividual variance parameters panel [MPT-GPN-003]", {

  # Interindividual variance parameters
  expect_equal(unique(gpn$type[gpn$OM == TRUE & gpn$diag == TRUE & gpn$panel=="IIV"]),
               "Interindividual variance parameters")
  expect_equal(unique(gpn$type_f[gpn$OM == TRUE & gpn$diag == TRUE & gpn$panel=="IIV"]), 3)
})

test_that("getPanelName appropriately classifies interoccasion variance parameters panel [MPT-GPN-004]", {

  # Interoccasion variance parameters
  defpt2 <- defpt
  defpt2$OM[1] <- TRUE
  defpt2$diag[1] <- TRUE
  defpt2$panel[1] <- "IOV"
  gpn2 <- getPanelName(defpt2)

  expect_equal(unique(gpn2$type[gpn2$OM == TRUE & gpn2$diag == TRUE & gpn2$panel=="IOV"]),
               "Interoccasion variance parameters")
})

test_that("getPanelName appropriately classifies covariate effect parameters panel [MPT-GPN-005]", {

  # Covariate effect parameters
  defpt3 <- defpt
  defpt3$panel[1] <- "cov"
  gpn3 <- getPanelName(defpt3)

  expect_equal(unique(gpn3$type[gpn3$panel=="cov"]), "Covariate effect parameters")
})

test_that("getPanelName appropriately classifies structural model parameters panel [MPT-GPN-006]", {

  # Structural model parameters
  defpt4 <- defpt
  defpt4$panel[1] <- "struct"
  gpn4 <- getPanelName(defpt4)

  expect_equal(unique(gpn4$type[gpn4$panel=="struct"]), "Structural model parameters")
})

