#' Plot a confidence interval around a point
#'
#' @param gt_object An existing gt table
#' @param column The column that contains the mean of the sample. This can either be a single number per row, if you have calculated the values ahead of time, or a list of values if you want to calculate the confidence intervals.
#' @param ci_columns Optional columns representing the left/right confidence intervals of your sample.
#' @param ci The confidence interval, representing the percentage, ie `0.9` which represents `10-90` for the two tails.
#' @param palette A vector of color strings of exactly length 4. The colors represent the central point, the color of the range, the color of the stroke around the central point, and the color of the text, in that specific order.
#' @param width A number indicating the width of the plot in `"mm"`, defaults to `45`.
#' @param text_args A list of named arguments. Optional text arguments passed as a list to `scales::label_number`.
#' @param text_size A number indicating the size of the text indicators in the plot. Defaults to 1.5. Can also be set to `0` to "remove" the text itself.
#' @param ref_line A number indicating where to place reference line on x-axis.
#'
#' @return a gt table
#' @export
#'
#' @section Examples:
#' ```r
#' # gtExtras can calculate basic conf int
#' # using confint() function
#'
#' ci_table <- generate_df(
#'   n = 50, n_grps = 3,
#'   mean = c(10, 15, 20), sd = c(10, 10, 10),
#'   with_seed = 37
#' ) %>%
#'   dplyr::group_by(grp) %>%
#'   dplyr::summarise(
#'     n = dplyr::n(),
#'     avg = mean(values),
#'     sd = sd(values),
#'     list_data = list(values)
#'   ) %>%
#'   gt::gt() %>%
#'   gt_plt_conf_int(list_data, ci = 0.9)
#'
#' # You can also provide your own values
#' # based on your own algorithm/calculations
#' pre_calc_ci_tab <- dplyr::tibble(
#'   mean = c(12, 10), ci1 = c(8, 5), ci2 = c(16, 15),
#'   ci_plot = c(12, 10)
#' ) %>%
#'   gt::gt() %>%
#'   gt_plt_conf_int(
#'     ci_plot, c(ci1, ci2),
#'     palette = c("red", "lightgrey", "black", "red")
#'     )
#' ```
#' @section Figures:
#' \if{html}{\figure{gt_plt_ci_calc.png}{options: width=70\%}}
#' \if{html}{\figure{gt_plt_ci_vals.png}{options: width=70\%}}
#'
#' @family Themes
#' @section Function ID:
#' 3-10
gt_plt_conf_int_new <- function(gt_object,
                            column,
                            ci_columns,
                            ci = 0.9,
                            ref_line = NULL,
                            palette = c("black", "grey", "white", "black"),
                            width = 45,
                            text_args = list(accuracy = 1),
                            text_size = 1.5) {
  all_vals <- gt_index(gt_object, {{ column }}, as_vector = FALSE)

  stopifnot("Confidence level must be between 0 and 1" = dplyr::between(ci, 0, 1))
  # convert desired confidence interval from percentage
  # to a two-tailed level to be used in confint() function
  level <- 1 - ((1 - ci) * 2)

  # if user doesn't supply their own pre-defined columns
  # grab them or save as "none"
  if (!missing(ci_columns)) {
    ci_vals <- all_vals %>%
      dplyr::select({{ ci_columns }})

    ci_val1 <- ci_vals[[1]]
    ci_val2 <- ci_vals[[2]]
  } else {
    ci_val1 <- "none"
  }

  column_vals <- all_vals %>%
    dplyr::select({{ column }}) %>%
    dplyr::pull()

  if ("none" %in% ci_val1) {
    stopifnot(
      "Must provide list column if no defined Confidence Intervals" =
        (class(column_vals) %in% c("list"))
    )

    # create a list of dataframes with
    # roughly calculated confidence intervals
    data_in <- lapply(column_vals, function(x) {
      dplyr::tibble(x = stats::na.omit(x), y = "1a") %>%
        dplyr::summarise(
          mean = mean(.data$x, na.rm = TRUE),
          y = unique(.data$y, na.rm = TRUE),
          lm_out = list(stats::lm(x ~ 1)),
          ci = list(stats::confint(.data$lm_out[[1]], level = level)),
          ci1 = ci[[1]][1],
          ci2 = ci[[1]][2]
        ) %>%
        dplyr::mutate(y = "1a")
    })
  } else {
    stopifnot(
      "Must provide single values per row if defining Confidence Intervals" =
        !(class(column_vals) %in% "list")
    )

    data_in <- dplyr::tibble(mean = column_vals, y = "1a") %>%
      dplyr::mutate(
        ci1 = ci_val1,
        ci2 = ci_val2,
        row_n = dplyr::row_number()
      ) %>%
      split(.$row_n)
  }

  # calculate the total range so the x-axis can be shared across rows
  all_ci_min <- min(dplyr::bind_rows(data_in)$ci1, na.rm = TRUE)
  all_ci_max <- max(dplyr::bind_rows(data_in)$ci2, na.rm = TRUE)

  ext_range <- scales::expand_range(
    c(all_ci_min, all_ci_max),
    mul = 0.1,
    zero_width = 1
  )

  ref_line <- if (is.null(ref_line)) {
    list("none")
  } else {
    list(ref_line)
  }

  gt_object %>%
    gt::text_transform(
      locations = gt::cells_body(columns = {{ column }}),
      fn = function(x) {
        tab_built <- mapply(
          FUN = add_ci_plot,
          data_in,
          list(palette),
          width,
          list(ext_range),
          list(text_args),
          text_size,
          list(ref_line),
          SIMPLIFY = FALSE
        )

        tab_built
      }
    ) %>%
    gt::cols_align(align = "left", columns = {{ column }})
}



#' Add a confidence interval plot inside a specific row
#'
#' @param data_in A dataframe of length 1
#' @param pal_vals A length 4 palette to be used for coloring points, segments and text
#' @param width Width of the output plot in `'mm'`
#' @param ext_range A length two vector of the full range across all values
#' @param text_args A list of optional text arguments passed to `scales::label_number()`
#' @inheritParams gt_plt_conf_int
#' @noRd
#'
#' @return SVG/HTML
add_ci_plot <- function(data_in,
                        pal_vals,
                        width,
                        ext_range,
                        text_args = list(scale_cut = cut_short_scale()),
                        text_size,
                        ref_line) {
  if (NA %in% unlist(data_in)) {
    return("&nbsp;")
  }

  if (unlist(ref_line) == "none") {
    base_plot <- data_in %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$mean, y = "1a"))
  } else {
    base_plot <- data_in %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$mean, y = "1a")) +
      ggplot2::geom_text(
        ggplot2::aes(
          x = unlist(ref_line) * 1.01,
          label = do.call(scales::label_number, text_args)(unlist(ref_line))
        ),
        color = pal_vals[4],
        vjust = 1.1,
        size = text_size,
        hjust = 0,
        position = ggplot2::position_nudge(y = -0.25),
        family = "mono",
        fontface = "bold"
      ) +
      ggplot2::geom_vline(xintercept = ref_line[[1]], color = pal_vals[4])
  }

  plot_out <- base_plot +
    ggplot2::geom_segment(
      ggplot2::aes(x = .data$ci1, xend = .data$ci2, y = .data$y, yend = .data$y),
      lineend = "round",
      linewidth = 1,
      color = pal_vals[2],
      alpha = 0.75
    ) +
    ggplot2::geom_point(
      ggplot2::aes(x = .data$mean, y = .data$y),
      size = 2,
      shape = 21,
      fill = pal_vals[1],
      color = pal_vals[3],
      stroke = 0.75
    ) +
    ggplot2::geom_label(
      ggplot2::aes(
        x = .data$ci2,
        label = do.call(scales::label_number, text_args)(.data$ci2)
      ),
      color = pal_vals[4],
      hjust = 1.1,
      size = text_size,
      vjust = 0,
      fill = "transparent",
      position = ggplot2::position_nudge(y = 0.25),
      family = "mono",
      fontface = "bold",
      linewidth = 0,
      label.padding = ggplot2::unit(0.05, "lines"),
      label.r = ggplot2::unit(0, "lines")
    ) +
    # ggplot2::geom_label(
    #   ggplot2::aes(x = .data$ci1, label = do.call(scales::label_number, text_args)(.data$ci1)),
    #   position = ggplot2::position_nudge(y = 0.25),
    #   color = pal_vals[4],
    #   hjust = -0.1,
    #   size = text_size,
    #   vjust = 0,
    #   fill = "transparent",
    #   family = "mono",
    #   fontface = "bold",
    #   linewidth = ggplot2::unit(0, "lines"),
    #   #label.size = ggplot2::unit(0, "lines"),
    #   label.padding = ggplot2::unit(0.05, "lines"),
    #   label.r = ggplot2::unit(0, "lines")
    # ) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = ggplot2::margin(0, 0, 0, 0, "pt"),
      plot.background = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    ) +
    ggplot2::coord_cartesian(ylim = c(0.9, 1.5), xlim = ext_range)

  out_name <- file.path(tempfile(
    pattern = "file",
    tmpdir = tempdir(),
    fileext = ".svg"
  ))

  ggplot2::ggsave(
    out_name,
    plot = plot_out,
    dpi = 25.4,
    height = 5,
    width = width,
    units = "mm",
    device = "svg"
  )

  img_plot <- readLines(out_name) %>%
    paste0(collapse = "") %>%
    gt::html()

  on.exit(file.remove(out_name), add = TRUE)

  img_plot
}


#' Return the underlying data, arranged by the internal index
#' @description This is a utility function to extract the underlying data from
#' a `gt` table. You can use it with a saved `gt` table, in the pipe (`%>%`)
#' or even within most other `gt` functions (eg `tab_style()`). It defaults to
#' returning the column indicated as a vector, so that you can work with the
#' values. Typically this is used with logical statements to affect one column
#' based on the values in that specified secondary column.
#' Alternatively, you can extract the entire ordered data according to the
#' internal index as a `tibble`. This allows for even more complex steps
#' based on multiple indices.
#'
#' @param gt_object An existing gt table object
#' @param column The column name that you intend to extract, accepts tidyeval semantics (ie `mpg` instead of `"mpg"`)
#' @param as_vector A logical indicating whether you'd like just the column indicated as a vector, or the entire dataframe
#' @return A vector or a `tibble`
#' @export
#'
#' @examples
#' library(gt)
#'
#' # This is a key step, as gt will create the row groups
#' # based on first observation of the unique row items
#' # this sampling will return a row-group order for cyl of 6,4,8
#'
#' set.seed(1234)
#' sliced_data <- mtcars %>%
#'   dplyr::group_by(cyl) %>%
#'   dplyr::slice_head(n = 3) %>%
#'   dplyr::ungroup() %>%
#'   # randomize the order
#'   dplyr::slice_sample(n = 9)
#'
#' # not in "order" yet
#' sliced_data$cyl
#'
#' # But unique order of 6,4,8
#' unique(sliced_data$cyl)
#'
#' # creating a standalone basic table
#' test_tab <- sliced_data %>%
#'   gt(groupname_col = "cyl")
#'
#' # can style a specific column based on the contents of another column
#' tab_out_styled <- test_tab %>%
#'   tab_style(
#'     locations = cells_body(mpg, rows = gt_index(., am) == 0),
#'     style = cell_fill("red")
#'   )
#'
#' # OR can extract the underlying data in the "correct order"
#' # according to the internal gt structure, ie arranged by group
#' # by cylinder, 6,4,8
#' gt_index(test_tab, mpg, as_vector = FALSE)
#'
#' # note that the order of the index data is
#' # not equivalent to the order of the input data
#' # however all the of the rows still match
#' sliced_data
#' @section Figures:
#' \if{html}{\figure{gt_index_style.png}{options: width=50\%}}
#'
#' @family Utilities
#' @section Function ID:
#' 2-20

gt_index <- function(gt_object, column, as_vector = TRUE) {
  stopifnot("'gt_object' must be a 'gt_tbl', have you accidentally passed raw data?" = "gt_tbl" %in% class(gt_object))
  stopifnot("'as_vector' must be a TRUE or FALSE" = is.logical(as_vector))

  if (length(gt_object[["_row_groups"]]) >= 1) {
    # if the data is grouped you need to identify the group column
    # and arrange by that column. I convert to a factor so that the
    # columns don't default to arrange by other defaults
    #  (ie alphabetical or numerical)
    gt_row_grps <- gt_object[["_row_groups"]]

    grp_vec_ord <- gt_object[["_stub_df"]] %>%
      dplyr::mutate(group_id = factor(group_id, levels = gt_row_grps)) %>%
      dplyr::arrange(group_id) %>%
      dplyr::pull(rownum_i)

    df_ordered <- gt_object[["_data"]] %>%
      dplyr::slice(grp_vec_ord)
  } else {
    # if the data is not grouped, then it will just "work"
    df_ordered <- gt_object[["_data"]]
  }

  # return as vector or tibble in correct, gt-indexed ordered
  if (isTRUE(as_vector)) {
    df_ordered %>%
      dplyr::pull({{ column }})
  } else {
    df_ordered
  }
}
