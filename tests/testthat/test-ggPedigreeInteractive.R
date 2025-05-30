test_that("ggPedigreeInteractive returns a plotly object", {
  library(BGmisc)
  data("potter")

  # Test with hints
  p_widget <- ggPedigreeInteractive(potter,
    famID = "famID",
    personID = "personID",
    as_widget = TRUE
  )

  expect_s3_class(p_widget, "plotly")
  expect_s3_class(p_widget, "htmlwidget")

  # Test with hints
  p <- ggPedigreeInteractive(potter,
    famID = "famID",
    personID = "personID",
    as_widget = FALSE
  )

  expect_s3_class(p, "plotly")
  expect_s3_class(p, "htmlwidget")
})
test_that("ggPedigreeInteractive returns a gg object", {
  library(BGmisc)
  data("potter")

  static <- ggPedigreeInteractive(
    potter,
    famID = "famID",
    personID = "personID",
    momID = "momID",
    dadID = "dadID",
    config = list(
      label_nudge_y = -.25,
      include_labels = TRUE,
      label_method = "geom_text",
      sex_color = TRUE,
      return_static = TRUE
    ),
    tooltip_cols = c("personID", "name")
  )
  expect_s3_class(static, "gg")
})
