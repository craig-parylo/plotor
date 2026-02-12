# generate OR plot ----
lr <- get_lr_titanic()
p <-
  plot_or(
    glm_model_results = lr,
    conf_level = 0.95,
    confint_fast_estimate = FALSE,
    assumption_checks = FALSE
  ) +
  ggplot2::labs(
    title = "Likelihood of surviving the Titanic disaster"
  )
# view it
p
# save as png
ggplot2::ggsave(
  filename = here::here("man", "figures", "readme_plot_or.png"),
  plot = p,
  units = "cm",
  width = 16,
  height = 8,
  dpi = 350
)

# generate OR table ----
t <-
  table_or(
    glm_model_results = lr,
    conf_level = 0.95,
    output = "gt",
    output_type = "multivariable",
    assumption_checks = FALSE,
    anonymise_counts = FALSE,
    use_model_data_only = TRUE
  )
# view it
t
# save as html first
gt::gtsave(
  data = t,
  filename = here::here("man", "figures", "readme_table_or.html")
)
# convert html -> png
webshot2::webshot(
  url = here::here("man", "figures", "readme_table_or.html"),
  file = here::here("man", "figures", "readme_table_or.png"),
  zoom = 2.5, # increases resolution
  vwidth = 1200, # viewport width
  vheight = 1200 # viewport height
)


# generate assumptions check ----
res <- callr::r(
  function(model) {
    # Force cli to emit colour even in non-interactive mode
    Sys.setenv(
      CLI_FORCE_COLOR = "1",
      CLI_NUM_COLORS = "256",
      CLI_DYNAMIC = "true",
      TERM = "xterm-256color"
    )

    # Capture stderr (cli output)
    out <- c(
      utils::capture.output(plotor:::check_or(model)),
      utils::capture.output(plotor:::check_or(model), type = "message")
    )

    paste(out, collapse = "\n")
  },
  args = list(lr)
)

txt <- res

# convert ansi -> html using cli
html_body <- cli::ansi_html(txt)

# wrap in minimal html
html <- paste0(
  "<html><head><style>",
  "body { font-family: monospace; font-size: 14px; padding: 20px; white-space: pre-wrap; }",
  "</style></head><body>",
  html_body,
  "</body></html>"
)


# save html
html_file <- here::here("man", "figures", "readme_check_or.html")
writeLines(html, html_file)

# convert html -> png
webshot2::webshot(
  url = html_file,
  file = here::here("man", "figures", "readme_check_or.png"),
  zoom = 2, # increases resolution
  vwidth = 690, # viewport width
  vheight = 850 # viewport height
)
