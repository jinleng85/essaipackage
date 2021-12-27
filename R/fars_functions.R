#' Importing a data
#'
#' Importing data from a file name
#'
#' @param filename a string that shows the pathname of the file
#'
#' @return a dataframe containing the data of the filename
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @examples
#' \dontrun{fars_read("accident_2013.csv.bz2")}
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make a filename
#'
#' Make a filename of the select year
#'
#' @param year a string of the selected year
#'
#' @return a string of the related filename
#'
#' @examples
#' \dontrun{make_filename("2013")}

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Creating a list of dataframes
#'
#' Create a list of dataframes of selected years
#'
#' @param years a list of selected years
#'
#' @return a list of the related dataframes
#'
#' @importFrom dplyr mutate select
#'
#' @examples
#' \dontrun{fars_read_years(c("2013","2014"))}

fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Make a summary of the number of accident per year
#'
#' Make tibble summing up the number of accidents per selected year
#'
#' @param years a list of selected years
#'
#' @return a tibble summing up the number of accidents per selected year
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#'
#' @examples
#' \dontrun{make_filename("2013")}
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Map the accidents
#'
#' Make a mapping of the accidents of the selected state and selected year
#'
#' @param state.num an integer of the selected state
#' @param year a string of the selected year
#'
#' @return a plot of the map
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{fars_map_state(1,"2013")}
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
