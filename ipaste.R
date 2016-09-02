library("stringr")

ipaste <- function(s) {
  ## interpolates the string with the value of the expressions between { and }
  ## usage example:
  ## > tableName <- "foods"
  ## > taste <- "spicy"
  ## > ipaste("SELECT FROM {tableName} WHERE taste='{taste}'")
  ## returns:
  ## "SELECT FROM foods WHERE taste='spicy'"
  loc <- str_locate(s, "\\{.+?\\}")
  stop <- all(is.na(loc))
  while (!stop) {
    # extract expr
    expr <- str_sub(s, loc[1] + 1, loc[2] - 1)
    # evaluate
    val <- eval(parse(text = expr))
    # process
    if (is.null(val)) {
      val <- ""
    } else {
      val <- as.character(val)
    }
    # replace
    str_sub(s, loc[1], loc[2]) <- val
    # search again
    loc <- str_locate(s, "\\{.+?\\}")
    stop <- all(is.na(loc))
  }
  s
}
