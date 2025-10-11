#' Interpolate missing pupil data within trials
#'
#' @param x Data frame or tibble containing pupil data
#' @param column Name of the column with pupil size values to interpolate
#' @param new_column Name for the new column storing interpolated values
#' @param type Interpolation method ("cubic-spline" or "linear"), default is "cubic-spline"
#' @param maxgap Maximum allowed gap (in ms) for interpolation. Default: Inf
#' @param hz Sampling rate in Hz (optional; inferred from "UpSampled" column if present)
#' @param plot Logical; if TRUE, calls pupil_plot() to visualize interpolation
#' @param plot_trial Specific trial number to plot (or "all" for all trials)
#' @param trial_column Column identifying trials, default "CURRENT_TRIAL_NUMBER"
#' @param time_column Optional column indicating time within trials, default "new_time"
#' @return Tibble with interpolated pupil data
#' @export
pupil_interpolate <- function(x, column, new_column, type = "cubic-spline",
                              maxgap = Inf, hz = "", plot = FALSE, plot_trial = "all",
                              trial_column = "CURRENT_TRIAL_NUMBER", time_column = "new_time") {
  x_before <- dplyr::as_tibble(x)
  
  if ("UpSampled" %in% colnames(x)) hz <- 1000
  if (maxgap != Inf) maxgap <- round(maxgap / (1000 / hz))
  
  x <- dplyr::mutate(x, pupil_val = .data[[column]])
  
  interpolate <- function(x, type, maxgap) {
    x <- dplyr::group_by(x, .data[[trial_column]])
    if (type == "cubic-spline") {
      x <- dplyr::mutate(x,
                         Missing.Total = ifelse(is.na(pupil_val), 1, 0),
                         Missing.Total = sum(Missing.Total, na.rm = TRUE) / dplyr::n(),
                         index = ifelse(is.na(pupil_val), as.numeric(NA), dplyr::row_number()),
                         index = zoo::na.approx(index, na.rm = FALSE),
                         pupil_val = zoo::na.spline(pupil_val, na.rm = FALSE, x = index, maxgap = maxgap))
      x <- dplyr::select(x, -index, -Missing.Total)
    } else if (type == "linear") {
      x <- dplyr::mutate(x, pupil_val = zoo::na.approx(pupil_val, na.rm = FALSE, maxgap = maxgap))
    }
    
    if (!is.null(time_column) && time_column %in% colnames(x)) {
      x <- dplyr::arrange(x, .data[[trial_column]], .data[[time_column]])
    } else {
      x <- dplyr::arrange(x, .data[[trial_column]])
    }
    
    x <- dplyr::ungroup(x)
    return(x)
  }
  
  x <- dplyr::as_tibble(x)
  x <- dtplyr::lazy_dt(x)
  x <- interpolate(x, type, maxgap)
  x <- dplyr::as_tibble(x)
  
  x[[new_column]] <- x$pupil_val
  x$pupil_val <- NULL
  
  if (plot == TRUE) pupil_plot(x_before, x, trial = plot_trial,
                               sub_title = paste0("pupil_interpolate(type = \"", type,
                                                  "\", maxgap = ", maxgap,  ")"))
  
  return(x)
}
