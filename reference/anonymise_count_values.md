# Anonymise Count Values

Anonymises counts within a supplied numeric vector by applying specific
formatting rules.

## Usage

``` r
anonymise_count_values(var)
```

## Arguments

- var:

  A numeric vector containing count data.

## Value

A character vector of anonymised counts.

## Details

This function provides a vectorised approach to data anonymisation that:

- Suppresses counts below 10 by replacing them with the string "\<10".

- Rounds counts of 10 or more to the nearest multiple of 5.

- Formats counts using
  [`scales::label_number_auto()`](https://scales.r-lib.org/reference/label_number_auto.html)
  to include thousands separators, which respects the locale settings
  for choice of the big mark.

The function returns a vector of strings representing the anonymised
counts.
