# table_or() produces output equivalent to a snapshot

    Code
      lr <- readRDS(file = testthat::test_path("test_data", "lr_titanic.Rds"))
      plotor::table_or(lr)
    Output
      # A tibble: 8 x 14
        label      level  rows outcome outcome_rate class estimate std.error statistic
        <fct>      <fct> <int>   <int>        <dbl> <chr>    <dbl>     <dbl>     <dbl>
      1 Passenger~ 3rd     706     178        0.252 fact~    0.169     0.172    -10.4 
      2 Passenger~ 1st     325     203        0.625 fact~   NA        NA         NA   
      3 Passenger~ 2nd     285     118        0.414 fact~    0.361     0.196     -5.19
      4 Passenger~ Crew    885     212        0.240 fact~    0.424     0.157     -5.45
      5 Gender     Male   1731     367        0.212 fact~   NA        NA         NA   
      6 Gender     Fema~   470     344        0.732 fact~   11.2       0.140     17.2 
      7 Age at de~ Child   109      57        0.523 fact~    2.89      0.244      4.35
      8 Age at de~ Adult  2092     654        0.313 fact~   NA        NA         NA   
      # i 5 more variables: p.value <dbl>, conf.low <dbl>, conf.high <dbl>,
      #   significance <chr>, comparator <dbl>

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

