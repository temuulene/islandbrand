# widget arguments reject repeated or unnamed options

    Code
      islh_reactable(data.frame(x = 1), embed_fonts = FALSE, columns = list(),
      columns = list())
    Condition
      Error in `islh_reactable()`:
      ! Arguments in `...` must have unique names.

---

    Code
      islh_reactable(data.frame(x = 1), NULL, NULL, NULL, TRUE, FALSE, 10, FALSE, 1)
    Condition
      Error in `islh_reactable()`:
      ! Arguments in `...` must have unique names.

---

    Code
      islh_reactable(data.frame(x = 1), embed_fonts = FALSE, filterable = NA)
    Condition
      Error in `islh_reactable()`:
      ! `filterable` must be a single TRUE or FALSE.

---

    Code
      islh_install_deps("docx", interactive = TRUE)
    Condition
      Error in `islh_install_deps()`:
      ! `interactive` requires `format = "html"` or `format = "both"`.

# interactive inputs produce helpful errors

    Code
      islh_reactable(1:3, embed_fonts = FALSE)
    Condition
      Error in `islh_reactable()`:
      ! `data` must be a data frame.

---

    Code
      islh_reactable(data.frame(x = 1), page_size = 0, embed_fonts = FALSE)
    Condition
      Error in `islh_reactable()`:
      ! `page_size` must be one positive whole number.

---

    Code
      islh_reactable(data.frame(x = 1), searchable = NA, embed_fonts = FALSE)
    Condition
      Error in `islh_reactable()`:
      ! `searchable` must be a single TRUE or FALSE.

---

    Code
      islh_reactable(data.frame(x = 1), title = c("a", "b"), embed_fonts = FALSE)
    Condition
      Error in `islh_reactable()`:
      ! `title` must be one string or NULL.

---

    Code
      islh_reactable(data.frame(x = 1), FALSE, embed_fonts = FALSE)
    Condition
      Error in `islh_reactable()`:
      ! `title` must be one string or NULL.

---

    Code
      islh_plotly(1:3, embed_fonts = FALSE)
    Condition
      Error in `islh_plotly()`:
      ! `plot` must be a ggplot, such as the result of `islh_epi_curve()`.

---

    Code
      islh_plotly(ggplot2::ggplot(), height = -1, embed_fonts = FALSE)
    Condition
      Error in `islh_plotly()`:
      ! `height` must be one positive number of pixels or NULL.

# non-HTML renders explain how to use static output

    Code
      islh_reactable(data.frame(x = 1), embed_fonts = FALSE)
    Condition
      Error in `islh_reactable()`:
      ! Interactive output needs an HTML report.
      i For Word or PDF, print the original ggplot or use `islh_flextable()`.
      i In a report with multiple formats, use `knitr::is_html_output()` to choose.

# interactive dependencies are opt-in for HTML reports

    Code
      islh_check("docx", interactive = TRUE, quiet = TRUE)
    Condition
      Error in `islh_check()`:
      ! `interactive` requires `format = "html"`.

