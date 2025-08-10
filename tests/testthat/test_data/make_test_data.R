#' -----------------------------------------------------------------------------
#' MAKE TEST DATA
#'
#' This script is used to make the files used as part of the test suite.
#' -----------------------------------------------------------------------------

flag_save_data <- FALSE

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
      arm = arm |>
        as.character() |>
        forcats::fct(levels = c('Control', 'Streptomycin')),
      dose_strep_g = dose_strep_g |> as.numeric(),
      dose_PAS_g = dose_PAS_g |> as.numeric(),
      gender = gender |> forcats::fct_infreq(),
      baseline_condition = baseline_condition |>
        as.character() |>
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
        forcats::fct(
          levels = c('<= 37.2C', '37.3 - 37.7C', '37.8 - 38.2C', '>= 38.3C')
        ), #|>
      #forcats::fct_inorder(ordered = TRUE),
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
        forcats::fct(
          levels = c(
            '1_Death',
            '2_Considerable_deterioration',
            '3_Moderate_deterioration',
            '4_No_change',
            '5_Moderate_improvement',
            '6_Considerable_improvement'
          )
        )
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

get_df_correlated <- function() {
  set.seed(123)
  n <- 1000
  df <- tibble::tibble(
    outcome = sample(0:1, size = n, replace = TRUE) |>
      factor(
        levels = c(0, 1),
        labels = c('alive', 'died')
      ),
    pred1 = rpois(n = n, lambda = 5),
    pred2 = pred1 + rpois(n = n, lambda = 1),
    pred3 = sample(1:4, size = n, replace = TRUE) |>
      factor(
        levels = c(1, 2, 3, 4),
        labels = c('red', 'green', 'blue', 'yellow')
      ),
    pred4 = as.numeric(pred3) |>
      magrittr::add(sample(1:2, size = n, replace = TRUE)) |>
      factor(
        levels = c(2, 3, 4, 5, 6, 7),
        labels = c(
          'Tuesday',
          'Wednesday',
          'Thursday',
          'Friday',
          'Saturday',
          'Sunday'
        )
      )
  )

  # create a list of variable = labels
  var_labels <- list(
    outcome = 'Survival status',
    pred1 = 'Predictor 1 (numeric)',
    pred2 = 'Predictor 2 (numeric, correlated with pred1)',
    pred3 = 'Predictor 3 (factor, colours)',
    pred4 = 'Predictor 4 (factor, weekdays)'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  # return the result
  return(df)
}

get_df_triple_outcome <- function() {
  set.seed(123)
  n <- 1000
  df <- tibble::tibble(
    outcome = sample(0:2, size = n, replace = TRUE) |>
      factor(
        levels = c(0, 1, 2),
        labels = c('alive', 'died', 'NA')
      ),
    pred1 = rpois(n = n, lambda = 5),
    pred2 = rpois(n = n, lambda = 5)
  )

  # create a list of variable = labels
  var_labels <- list(
    outcome = 'Survival status with errant NA value',
    pred1 = 'Predictor 1',
    pred2 = 'Predictor 2'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  # return the result
  return(df)
}

get_df_separated <- function(rows = 1000) {
  set.seed(123)
  #n <- 1000
  n <- rows
  df <- tibble::tibble(
    # an outcome variable with 20:80 distribution of 'alive' vs 'died'
    outcome = sample(0:1, size = n, replace = TRUE, prob = c(0.2, 0.8)) |>
      factor(levels = 0:1, labels = c('Alive', 'Died')),

    # a separated numeric predictor variable
    pred1 = dplyr::if_else(
      condition = outcome == 'Alive',
      true = sample(0:10, size = n, replace = TRUE),
      false = sample(10:20, size = n, replace = TRUE)
    ),

    # a separated categorical predictor variable
    pred2 = dplyr::if_else(
      condition = outcome == 'Alive',
      true = sample(0:2, size = n, replace = TRUE),
      false = sample(1:3, size = n, replace = TRUE)
    ) |>
      factor(levels = 0:3, labels = c('North', 'South', 'East', 'West')),

    # two randomly allocated predictors
    pred3 = rpois(n = n, lambda = 10),

    pred4 = sample(0:3, size = n, replace = TRUE) |>
      factor(levels = 0:3, labels = c('red', 'green', 'yellow', 'blue')),
  )

  # create a list of variable = labels
  var_labels <- list(
    outcome = 'Survival status',
    pred1 = 'Predictor 1 - Quasi-complete separation',
    pred2 = 'Predictor 2 - Complete separation',
    pred3 = 'Predictor 3',
    pred4 = 'Predictor 4'
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  # return the result
  return(df)
}

get_df_birth_weight <- function() {
  df <-
    MASS::birthwt |>
    tibble::as_tibble() |>
    # convert categorical variables to factors with appropriate values
    dplyr::mutate(
      low = low |>
        factor(
          levels = 0:1,
          labels = c(
            "Birth weight above 2.5kg",
            "Low birth weight"
          )
        ),
      race = race |>
        factor(
          levels = 1:3,
          labels = c(
            "White",
            "Black",
            "Other"
          )
        ),
      smoke = smoke |>
        factor(
          levels = 0:1,
          labels = c(
            "Did not smoke",
            "Smoked"
          )
        ),
      ht = ht |>
        factor(
          levels = 0:1,
          labels = c(
            "No history of hypertension",
            "History of hypertension"
          )
        ),
      ui = ui |>
        factor(
          levels = 0:1,
          labels = c(
            "Absence of uterine irritability",
            "Presence of uterine irritability"
          )
        )
    )

  # create a list of variable = labels
  var_labels <- list(
    low = "Low birth weight (birth weight less than 2.5 kg)",
    age = "Mother's age (years)",
    lwt = "Mother's weight (pounds) at last menstrual period",
    race = "Mother's race",
    smoke = "Smoking status during pregnancy",
    ptl = "Number of previous premature labours",
    ht = "History of hypertension",
    ui = "Presence of uterine irritability",
    ftv = "Number of physician visits during the first trimester",
    bwt = "Birth weight (grams)"
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  # return the result
  return(df)
}

# df <- get_df_birth_weight()
# dplyr::glimpse(df)
# df |> dplyr::count(low)
# lr <- get_lr_birth_weight()
# plotor::plot_or(lr)
# plotor::table_or(lr, output = "gt")
# plotor::check_or(lr)

get_df_framingham <- function() {
  df <-
    R4HCR::Framingham |>
    dplyr::mutate(
      sex = sex |>
        factor(
          levels = c(0, 1),
          labels = c("Female", "Male")
        ),
      education = education |>
        factor(
          levels = c(1, 2, 3, 4),
          labels = c(
            "0-11 years",
            "High school diploma, GED",
            "Some college, vocational school",
            "College (BS, BA) degree or more"
          )
        ) |>
        forcats::fct_na_value_to_level("Not known"),
      currentsmoker = currentsmoker |>
        factor(
          levels = c(0, 1),
          labels = c(
            "Not current smoker",
            "Current smoker"
          )
        ) |>
        forcats::fct_na_value_to_level("Not known"),
      cigsperday = cigsperday |>
        factor(
          levels = c(0, 1),
          labels = c(
            "Not current smoker",
            "1-90 cigarettes per day"
          )
        ) |>
        forcats::fct_na_value_to_level("Not known"),
      bpmeds = bpmeds |>
        factor(
          levels = c(0, 1),
          labels = c(
            "Not currently used",
            "Current use"
          )
        ) |>
        forcats::fct_na_value_to_level("Not known"),
      prevalentstroke = prevalentstroke |>
        factor(
          levels = c(0, 1),
          labels = c(
            "Free of disease",
            "Prevalent stroke"
          )
        ) |>
        forcats::fct_na_value_to_level("Not known"),
      prevalenthyp = prevalenthyp |>
        factor(
          levels = c(0, 1),
          labels = c(
            "Free of disease",
            "Prevalent hypertension"
          )
        ) |>
        forcats::fct_na_value_to_level("Not known"),
      diabetes = diabetes |>
        factor(
          levels = c(0, 1),
          labels = c(
            "No diabetes",
            "Diabetes"
          )
        ) |>
        forcats::fct_na_level_to_value("Not known"),
      tenyearchd = tenyearchd |>
        factor(
          levels = c(0, 1),
          labels = c(
            "Free of CHD after 10 years",
            "Developed CHD within 10 years"
          )
        )
    )

  # create a list of variable = labels
  var_labels <- list(
    sex = "Sex of participant",
    age = "Age (years)",
    education = "Highest educational attainment",
    currentsmoker = "Current cigarette smoking at exam",
    cigsperday = "Number of cigarettes smoked each day",
    bpmeds = "Use of anti-hypertensive medication at exam",
    prevalentstroke = "Prevalent stroke",
    prevalenthyp = "Prevalent hypertension",
    diabetes = "Prevalent diabetic",
    totchol = "Serum Total Cholesterol (mg/dL)",
    sysbp = "Systolic blood pressure (mmHg)",
    diabp = "Diastolic blood pressure (mmHg)",
    bmi = "Body mass index (kg/m2)",
    heartrate = "Heart rate (beats/min)",
    glucose = "Casual serum glucose (mg/dL)",
    tenyearchd = "Developed Coronary Heart Disease within ten years"
  )

  # apply the labels
  labelled::var_label(df) <- var_labels

  # return the results
  return(df)
}

# df_framingham <- get_df_framingham()
#
#
# lr_framingham <- get_lr_framingham()

get_df_nhanes <- function() {
  df <-
    NHANES::NHANES
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
    formula = diabetes_5y ~
      age +
        bmi +
        pregnancy_num +
        glucose_mg_dl +
        dbp_mm_hg +
        triceps_mm +
        insulin_microiu_ml
  )
}

get_lr_streptb <- function() {
  df <- get_df_streptb()
  lr <- stats::glm(
    data = df,
    family = 'binomial',
    formula = improved ~
      gender +
        dose_strep_g +
        baseline_condition +
        baseline_temp +
        baseline_esr +
        baseline_cavitation
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

# an lr model with three outcomes
get_lr_triple_outcome <- function() {
  df <- get_df_triple_outcome()
  lr <- stats::glm(
    data = df,
    formula = outcome ~ pred1 + pred2,
    family = 'binomial'
  )
}

# an lr model with two correlated predictors
get_lr_correlated_two <- function() {
  df <- get_df_correlated()
  lr <- stats::glm(
    data = df,
    formula = outcome ~ pred1 + pred2,
    family = 'binomial'
  )
}

get_lr_correlated_four <- function() {
  df <- get_df_correlated()
  lr <- stats::glm(
    data = df,
    formula = outcome ~ pred1 + pred2 + pred3 + pred4,
    family = 'binomial'
  )
}

get_lr_separated <- function() {
  df <- get_df_separated()
  lr <- stats::glm(
    data = df,
    formula = outcome ~ pred1 + pred2 + pred3 + pred4,
    family = 'binomial'
  )
}

get_lr_separated_quasi <- function() {
  df <- get_df_separated()
  lr <-
    stats::glm(
      data = df,
      formula = outcome ~ pred1 + pred3,
      family = 'binomial'
    )
}

get_lr_separated_large <- function() {
  df <- get_df_separated(rows = 1e5)
  lr <- stats::glm(
    data = df,
    formula = outcome ~ pred1 + pred2,
    family = 'binomial'
  )
}

get_lr_birth_weight <- function() {
  df <- get_df_birth_weight()
  lr <- stats::glm(
    data = df,
    family = binomial,
    formula = low ~ age + lwt + race + smoke + ptl + ht + ui + ftv
  )

  return(lr)
}

get_lr_framingham <- function() {
  df <- get_df_framingham()
  lr <- stats::glm(
    data = df,
    family = binomial,
    formula = tenyearchd ~
      sex +
        age +
        education +
        currentsmoker +
        bpmeds +
        prevalentstroke +
        prevalenthyp +
        diabetes +
        totchol +
        sysbp +
        diabp +
        bmi +
        heartrate +
        glucose
  )
}

get_lr_nhanes <- function() {
  df <- get_df_nhanes()
  lr <- stats::glm(
    data = df,
    family = binomial,
    formula = Diabetes ~ Gender + BPSys3 + Education
  )
}

# Get data ---------------------------------------------------------------------
# df
df_titanic <- get_df_titanic()
df_infert <- get_df_infert()
df_streptb <- get_df_streptb()
df_diabetes <- get_df_diabetes()
df_triple_outcome <- get_df_triple_outcome()
df_correlated <- get_df_correlated()
df_separated <- get_df_separated()
df_separated_large <- get_df_separated(rows = 1e5)
df_birth_weight <- get_df_birth_weight()
df_framingham <- get_df_framingham()
df_nhanes <- get_df_nhanes()

# lr
lr_titanic <- get_lr_titanic()
lr_infert <- get_lr_infert()
lr_diabetes <- get_lr_diabetes()
lr_streptb <- get_lr_streptb()
lr_birth_weight <- get_lr_birth_weight()
lr_framingham <- get_lr_framingham()
lr_nhanes <- get_lr_nhanes()

# non-lr
nonlr_streptb <- get_nonlr_streptb()

# non-binary outcome
lr_triple_outcome <- get_lr_triple_outcome()

# correlated
lr_correlated_two <- get_lr_correlated_two()
lr_correlated_four <- get_lr_correlated_four()

# separated
lr_separated <- get_lr_separated()
lr_separated_large <- get_lr_separated_large()


# Save data --------------------------------------------------------------------

if (flag_save_data) {
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

  # triple outcomes
  saveRDS(
    object = df_triple_outcome,
    file = testthat::test_path('test_data', 'df_triple_outcome.Rds')
  )
  saveRDS(
    object = lr_triple_outcome,
    file = testthat::test_path('test_data', 'lr_triple_outcome.Rds')
  )

  # correlated
  saveRDS(
    object = df_correlated,
    file = testthat::test_path('test_data', 'df_correlated.Rds')
  )
  saveRDS(
    object = lr_correlated_two,
    file = testthat::test_path('test_data', 'lr_correlated_two.Rds')
  )
  saveRDS(
    object = lr_correlated_four,
    file = testthat::test_path('test_data', 'lr_correlated_four.Rds')
  )

  # separated
  saveRDS(
    object = df_separated,
    file = testthat::test_path('test_data', 'df_separated.Rds')
  )
  saveRDS(
    object = lr_separated,
    file = testthat::test_path('test_data', 'lr_separated.Rds')
  )

  # separated large
  saveRDS(
    object = lr_separated_large,
    file = testthat::test_path('test_data', 'lr_separated_large.Rds')
  )

  # framingham
  saveRDS(
    object = df_framingham,
    file = testthat::test_path('test_data', 'df_framingham.Rds')
  )
  saveRDS(
    object = lr_framingham,
    file = testthat::test_path('test_data', 'lr_framingham.Rds')
  )

  # birth weight
  saveRDS(
    object = df_birth_weight,
    file = testthat::test_path('test_data', 'df_birth_weight.Rds')
  )
  saveRDS(
    object = lr_birth_weight,
    file = testthat::test_path('test_data', 'lr_birth_weight.Rds')
  )

  # nhanes
  saveRDS(
    object = df_nhanes,
    file = testthat::test_path('test_data', 'df_nhanes.Rds')
  )
  saveRDS(
    object = lr_nhanes,
    file = testthat::test_path('test_data', 'lr_nhanes.Rds')
  )
}
