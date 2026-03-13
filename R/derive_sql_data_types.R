#' @title Derive SQL Data Types
#' @description Derive equivalent SQL data types from an R dataframe
#' @param df R dataframe
#' @param buffer use to increase size of varchar fields - default is 0.
#'
#' @returns Creates a vector of column names and derived SQL data types.
#' Can be used within data types parameter of dbWriteTable() from DBI package.
#' @export
#' @import dplyr, purrr
#'
#' @examples derive_sql_data_types(mtcars)

derive_sql_data_types <- function(df, buffer = 0) {

  # Calculate max length by column
  max_lengths <- df |>
    summarise(across(
      where(is.character),
      ~ max(nchar(., type = "bytes"), na.rm = TRUE)
    )) |>
    as.list()

  # Map R types to SQL types
  r_to_sql <- list(
    character = function(name) {
      varchar_len <- max_lengths[[name]] + buffer
      sprintf("varchar(%s)", varchar_len)
    },
    integer = function(x) "int",
    numeric = function(x) "float",
    double  = function(x) "float",
    logical = function(x) "bit",
    Date    = function(x) "date",
    POSIXct = function(x) "datetime"
  )

  # Create output vector of column names and data types
  out <- map_chr(names(df), function(col) {
    col_class <- class(df[[col]])[1]
    mapper <- r_to_sql[[col_class]]

    if (is.null(mapper)) {
      warning(sprintf(
        "No SQL mapping defined for R class '%s'. Using varchar(max).",
        col_class
      ))
      return("varchar(max)")
    }

    mapper(col)
  })

  names(out) <- names(df)
  return(out)
}

