#' -----------------------------------------------------------------------------
#' MAKE TEST DATA
#'
#' This script is used to make the files used as part of the test suite.
#' -----------------------------------------------------------------------------

# Data functions ---------------------------------------------------------------
get_df_titanic <- function() {
  df <- datasets::Titanic |>
    dplyr::as_tibble() |>
    # convert counts to observations
    dplyr::filter(n > 0) |>
    tidyr::uncount(weights = n) |>
    # convert categorical variables to factors.
    # we specify an order for levels in Class and Survival, otherwise ordering
    # in descending order of frequency
    dplyr::mutate(
      Class = Class |>
        forcats::fct(levels = c('1st', '2nd', '3rd', 'Crew')),
      Sex = Sex |>
        forcats::fct_infreq(),
      Age = Age |>
        forcats::fct_infreq(),
      Survived = Survived |>
        forcats::fct(levels = c('No', 'Yes'))
    )

  # create a list of variable = labels
  var_labels <- list(
    Class = 'Passenger class',
    Sex = 'Gender',
    Age = 'Age at death',
    Survived = 'Survived'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  return(df)
}

get_df_infert <- function() {
  df <-
    datasets::infert |>
    dplyr::mutate(
      case = case |>
        dplyr::case_match(1 ~ 'Case', 0 ~ 'Control') |>
        forcats::fct(levels = c('Control', 'Case')),
      induced = induced |>
        dplyr::case_match(0 ~ '0', 1 ~ '1', 2 ~ '2 or more') |>
        forcats::fct(levels = c('0', '1', '2 or more')),
      spontaneous = spontaneous |>
        dplyr::case_match(0 ~ '0', 1 ~ '1', 2 ~ '2 or more') |>
        forcats::fct(levels = c('0', '1', '2 or more'))
    )

  # create a list of variable = labels
  var_labels <- list(
    education = 'Education (years)',
    age = 'Age (years)',
    parity = 'Count',
    induced = 'Number of prior induced abortions',
    case = 'Case status',
    spontaneous = 'Number of prior spontaneous abortions'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  return(df)
}

get_df_streptb <- function() {
  df <-
    medicaldata::strep_tb |>
    dplyr::mutate(
      arm = arm |> forcats::fct(levels = c('Control', 'Streptomycin')),
      dose_strep_g = dose_strep_g |> as.numeric(),
      dose_PAS_g = dose_PAS_g |> as.numeric(),
      gender = gender |> forcats::fct_infreq(),
      baseline_condition = baseline_condition |>
        forcats::fct(levels = c('1_Good', '2_Fair', '3_Poor')),
      baseline_temp = baseline_temp |>
        dplyr::case_match(
          '1_<=98.9F/37.2C' ~ '<= 37.2C',
          '2_99-99.9F/37.3-37.7C' ~ '37.3 - 37.7C',
          '2_99-99.9F/37.3-37.7C/37.3-37.7C' ~ '37.3 - 37.7C',
          '3_100-100.9F/37.8-38.2C' ~ '37.8 - 38.2C',
          '3_100-100.9F/37.8-38.2C/37.8-38.2C' ~ '37.8 - 38.2C',
          '4_>=101F/38.3C' ~ '>= 38.3C'
        ) |>
        forcats::fct(levels = c('<= 37.2C', '37.3 - 37.7C', '37.8 - 38.2C', '>= 38.3C')) |>
        forcats::fct_inorder(ordered = TRUE),
      baseline_esr = baseline_esr |>
        dplyr::case_match(
          '2_11-20' ~ '11-20',
          '3_21-50' ~ '21-50',
          '4_51+' ~ '51+',
          '<NA>' ~ NA
        ) |>
        forcats::fct(levels = c('11-20', '21-50', '51+')) |>
        forcats::fct_na_value_to_level(level = 'NA'),
      baseline_cavitation = baseline_cavitation |>
        forcats::fct(levels = c('no', 'yes')),
      radiologic_6m = radiologic_6m |>
        forcats::fct(levels = c('1_Death', '2_Considerable_deterioration', '3_Moderate_deterioration', '4_No_change', '5_Moderate_improvement', '6_Considerable_improvement'))
    )

  # create a list of variable = labels
  var_labels <- list(
    improved = 'Outcome of improvement',
    arm = 'Assigned treatment arm',
    dose_strep_g = 'Dose of Streptomycin (grams)',
    dose_PAS_g = 'DONT USE Dose of Para-Amino-Salicylate',
    gender = 'Gender',
    baseline_condition = 'Condition of the patient at baseline',
    baseline_temp = 'Temperature at baseline',
    baseline_esr = 'Erythrocyte Sedimentation Rate',
    baseline_cavitation = 'Cavitation on baseline X-ray',
    strep_resistance = 'Streptomycin resistance after 6 months of therapy'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  return(df)
}

get_df_diabetes <- function() {
  df <-
    medicaldata::diabetes |>
    janitor::clean_names()


  # create a list of variable = labels
  var_labels <- list(
    pregnancy_num = 'Number of pregnancies',
    glucose_mg_dl = 'Plasma glucose concentration (mg per dl)',
    dbp_mm_hg = 'Diastolic blood pressure (mmHg)',
    triceps_mm = 'Triceps skin fold thickness (mm)',
    insulin_microiu_ml = 'Serum insulin (microIU per ml)',
    bmi = 'Body mass index',
    pedigree = 'Diabetes pedigree score',
    age = 'Age (years)',
    diabetes_5y = 'Diagnosis of diabetes in following 5 years'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  return(df)
}

# Model functions --------------------------------------------------------------
get_lr_titanic <- function() {

  df <- get_df_titanic()
  lr <- stats::glm(
    data = df,
    family = 'binomial',
    formula = Survived ~ Class + Sex + Age
  )

  return(lr)
}

get_lr_infert <- function() {

  df <- get_df_infert()
  lr <- stats::glm(
    data = df,
    family = 'binomial',
    formula = case ~ age + parity + education + induced + spontaneous
  )

  return(lr)
}

get_lr_diabetes <- function() {
  df <- get_df_diabetes()
  lr <- stats::glm(
    data = df,
    family = 'binomial',
    formula = diabetes_5y ~ age + bmi + pregnancy_num + glucose_mg_dl + dbp_mm_hg + triceps_mm + insulin_microiu_ml
  )
}

get_lr_streptb <- function() {
  df <- get_df_streptb()
  lr <- stats::glm(
    data = df,
    family = 'binomial',
    formula = improved ~ gender + dose_strep_g + baseline_condition + baseline_temp + baseline_esr + baseline_cavitation
  )
}

# a non-logistic regression version of the model
get_nonlr_streptb <- function() {
  df <- get_df_streptb()
  lr <- stats::glm(
    data = df,
    formula = rad_num ~ baseline_temp
  )
}

# Get data ---------------------------------------------------------------------
# df
df_titanic <- get_df_titanic()
df_infert <- get_df_infert()
df_streptb <- get_df_streptb()
df_diabetes <- get_df_diabetes()

# lr
lr_titanic <- get_lr_titanic()
lr_infert <- get_lr_infert()
lr_diabetes <- get_lr_diabetes()
lr_streptb <- get_lr_streptb()

# non-lr
nonlr_streptb <- get_nonlr_streptb()

# Save data --------------------------------------------------------------------
# titanic
saveRDS(
  object = df_titanic,
  file = testthat::test_path('test_data', 'df_titanic.Rds')
)
saveRDS(
  object = lr_titanic,
  file = testthat::test_path('test_data', 'lr_titanic.Rds')
)

# infertility
saveRDS(
  object = df_infert,
  file = testthat::test_path('test_data', 'df_infert.Rds')
)
saveRDS(
  object = lr_infert,
  file = testthat::test_path('test_data', 'lr_infert.Rds')
)

# strep tb
saveRDS(
  object = df_streptb,
  file = testthat::test_path('test_data', 'df_streptb.Rds')
)
saveRDS(
  object = lr_streptb,
  file = testthat::test_path('test_data', 'lr_streptb.Rds')
)
saveRDS(
  object = nonlr_streptb,
  file = testthat::test_path('test_data', 'nonlr_streptb.Rds')
)

# diabetes
saveRDS(
  object = df_diabetes,
  file = testthat::test_path('test_data', 'df_diabetes.Rds')
)
saveRDS(
  object = lr_diabetes,
  file = testthat::test_path('test_data', 'lr_diabetes.Rds')
)
