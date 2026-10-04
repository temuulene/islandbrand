# Row identity for combinations of columns.
#
# Rows share a group only when every column matches. A missing value matches
# another missing value and nothing else, so a missing site and a site called
# "NA" stay apart. Labels pasted together with a separator, as interaction()
# builds them, let ("A.B", "C") and ("A", "B.C") become one group, and no
# choice of separator prevents that.

# One integer per row, numbering the groups in order of first appearance.
.islh_group_ids <- function(data, cols) {
  n <- nrow(data)
  id <- rep(1, n)
  for (col in cols) {
    x <- data[[col]]
    code <- match(x, unique(x))
    # Both parts are at most n, so the combined key is exact in a double.
    key <- (id - 1) * n + code
    id <- match(key, unique(key))
  }
  as.integer(id)
}

# The sum of `x` within each group, in group order.
.islh_group_sums <- function(x, ids) {
  as.numeric(rowsum(x, ids, reorder = TRUE))
}
