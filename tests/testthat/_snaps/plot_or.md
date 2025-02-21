# table_or() produces {gt} tables equivalent to a snapshot

    Code
      lr <- readRDS(file = testthat::test_path("test_data", "lr_titanic.Rds"))
      plotor::table_or(lr, output = "gt")

---

    Code
      lr <- readRDS(file = testthat::test_path("test_data", "lr_diabetes.Rds"))
      plotor::table_or(lr, output = "gt")

---

    Code
      lr <- readRDS(file = testthat::test_path("test_data", "lr_infert.Rds"))
      plotor::table_or(lr, output = "gt")

