#' Calculate crime dispersion
#'
#' A function to calculate the offense dispersion index (ODI) for crime counts
#' in unit subregions of an overall region at two time periods, t1 and t2.
#'
#' @param data1 data frame with a minimum of 3 columns with area ID, Rt1 count, Rt2 count
#' @param unitID a vector of identifiers for spatial units
#' @param time1 a vector of crime counts at time 1 (numeric object)
#' @param time2 a vector of crime counts at time 2 (numeric object)
#' @return A list containing a data frame, values, and a plot
#' @import dplyr ggplot2
#' @export
#' @references Ratcliffe, JH (2010) The spatial dependency of crime increase dispersion, Security Journal, 23(1): 18-36.
#'


crimedispersion <- function
(data1, unitID, time1, time2, method = "match") {

  # define variables to limit build warnings
  adjusted <- Ut1 <- Ut2 <- Rt1 <- Rt2 <- chg <- pct <- NULL

  # ERROR CHECKING. Has user passed a data frame?
  if (!is.data.frame(data1)) {
    stop("The input data specified is not a data.frame object. Please fix.")
  }

  # Build a local data.frame and populate with passed arguments
  source_rows <- nrow(data1)
  df1 <- data.frame(matrix(ncol = 3, nrow = source_rows))
  colnames(df1) <- c("unit", "time1", "time2")
  df1$unit <- data1[, unitID]
  df1$time1 <- data1[, time1]
  df1$time2 <- data1[, time2]
  if (method == "remove") {
    analysisMethod <- "remove"
  } else {
    analysisMethod <- "match"
  }


  # ERROR CHECKING. Did user pass numeric columns where needed?
  try (df1$time1 <- as.numeric(df1$time1), silent = TRUE)
  try (df1$time2 <- as.numeric(df1$time2), silent = TRUE)

  if (!class(df1$time1)[1] == "numeric") {
    stop("The time1 field is not a numeric object. Please fix.")
  }
  if (!class(df1$time2)[1] == "numeric") {
    stop("The time2 field is not a numeric object. Please fix.")
  }

  # MORE ERROR CHECKING:
  # What if the user has NA or missing data?
  # What if the crime problem is decreasing?
  # Fun tasks for later...


  # Set up parameters -------------------------------------------------------

  # Set up initial parameters
  count_Rt1 <- sum(df1$time1)
  count_Rt2 <- sum(df1$time2)
  chg_Rt1_Rt2 <- count_Rt2 - count_Rt1
  pct_Rt1_Rt2 <- (chg_Rt1_Rt2 / count_Rt1) *100

  # Add the field that has the volume of change, and order by it
  df1 <- df1 %>%
    mutate (diff = time2 - time1) %>%
    mutate (diffPct = 100*(diff/time1)) %>%
    arrange(desc(diff))

  # Grab some basic statistics here
  numPositive <- length(which(df1$diff > 0))
  numNeutral <- length(which(df1$diff == 0))
  numNegative <- length(which(df1$diff < 0))


  # Create the new data frame to hold the result
  df2 <- data.frame(matrix(ncol =8, nrow = 0))
  colnames(df2) <- c("unit", "adjusted", "Ut1", "Ut2", "Rt1", "Rt2", "chg", "pct")
  df2 <- df2 %>%
    mutate(unit = as.character(unit)) %>%
    mutate(adjusted = as.numeric(adjusted)) %>%
    mutate(Ut1 = as.numeric(Ut1)) %>%
    mutate(Ut2 = as.numeric(Ut2)) %>%
    mutate(Rt1 = as.numeric(Rt1)) %>%
    mutate(Rt2 = as.numeric(Rt2)) %>%
    mutate(chg = as.numeric(chg)) %>%
    mutate(pct = as.numeric(pct))

  # set up the initial row in the result data frame
  df2 <- df2 %>% add_row(unit = "[ ALL AREAS ]", adjusted = 0,
                         Ut1 = 0, Ut2 = 0,
                         Rt1 = count_Rt1, Rt2 = count_Rt2,
                         chg = chg_Rt1_Rt2, pct = pct_Rt1_Rt2)

  gain_from_row_removal <- row_to_remove <- NULL


  # Loop through each row of the data
  for (master_loop in 1:(source_rows)){

    df1 <- df1 %>%  # order the data frame
      arrange(desc(diff))

    if (analysisMethod == "match"){
      #### 'Zero change the row' approach
      count_Rt1_temp <- count_Rt1
      count_Rt2_temp <- count_Rt2 - df1$diff[master_loop]
      pct_Rt1_Rt2 <- ((count_Rt1_temp - count_Rt2_temp) / count_Rt1) *100
    }
    else { #analysisMethod == "remove"
      #### 'Remove entire row' approach, including remove t1 value
      count_Rt1_temp <- count_Rt1 - df1$time1[master_loop]
      count_Rt2_temp <- count_Rt2 - df1$time2[master_loop]
      pct_Rt1_Rt2 <- ((count_Rt1_temp - count_Rt2_temp) / count_Rt1) *100
    }

    row_to_remove <- 1  # Always row 1, but this is a legacy from
    # when I used a different approach...
    # Here, the row we are removing is
    # stored in row_to_remove

    if (analysisMethod == "remove"){
      #### Remove entire row approach
      #    This approach removes the impact of the area by subtracting
      #    both Rt1 and Rt2
      count_Rt1 <- count_Rt1 - df1$time1[row_to_remove]
      count_Rt2 <- count_Rt2 - df1$time2[row_to_remove]
      chg_Rt1_Rt2 <- count_Rt2 - count_Rt1
      pct_Rt1_Rt2 <- (chg_Rt1_Rt2 / count_Rt1) *100
      named_areas <- df1$unit[row_to_remove]
    }

    if (analysisMethod == "match"){
      #### Zero change the row approach, as if Rt2 == Rt1 in the row
      #    The best row to remove is has been exhaustively calculated
      #    Here, the row we are removing is stored in row_to_remove
      count_Rt1 <- count_Rt1
      count_Rt2 <- count_Rt2 - df1$diff[row_to_remove]
      chg_Rt1_Rt2 <- count_Rt2 - count_Rt1
      pct_Rt1_Rt2 <- (chg_Rt1_Rt2 / count_Rt1) *100
      named_areas <- df1$unit[row_to_remove]
    }

    # Add result to the output data frame
    df2 <- df2 %>% add_row(unit = named_areas, adjusted = master_loop,
                           Ut1 = df1$time1[row_to_remove], Ut2 = df1$time2[row_to_remove],
                           Rt1 = count_Rt1, Rt2 = count_Rt2,
                           chg = chg_Rt1_Rt2, pct = pct_Rt1_Rt2)

    # Adjust the row we just used in one of two ways:
    # 1. remove the actual row entirely
    if (analysisMethod == "remove"){
      df1 <-df1[-c(row_to_remove), ]
    }
    #2. adjust the Rt2 to match Rt1 resulting in a zero diff
    #   but show that diff as < lowest diff in the data set so that
    #   the program does not stall with too many zeros
    if (analysisMethod == "match"){
      df1$time2[row_to_remove] <- df1$time1[row_to_remove]
      df1$diff[row_to_remove] <- -999 # this should be changed to always less than
      # the lowest diff score in the data set
    }

  } # end master_loop


  # Calculate ODI and NCDI indices -----------------------------------------
  NumContributed <- length(which(df2$chg > 0))
  ODI <- NumContributed / source_rows
  NCDI <- (numPositive - NumContributed) / source_rows
  ODI.text <- paste("O.D.I. = ", format(ODI, digits = 3), "after \nadjusting",
                    NumContributed, "of the", source_rows, "units")

  # Tidy up names for data frame --------------------------------------------

  df2 <- df2 %>%
    rename(unit_t1 = Ut1, unit_t2 = Ut2, region_t1 = Rt1, region_t2 = Rt2)

  # Plot --------------------------------------------------------------------

  df3 <- df2
  plot.adjustment <- ""
  if (nrow(df3) > 101)  {
    df3 <- df3[1:101, ]
    plot.adjustment <- "Plot only shows first\n100 areas adjusted"
  }


  p <- ggplot(df3, aes(x=reorder(unit, adjusted), y=pct, group = 1)) +
    geom_line(color="#3277a8") +
    geom_point(shape=21, color="white", fill="#3277a8", size=2) +
    geom_hline(color="grey", yintercept=0) +
    labs(title="Dispersion of crime rate change",
         x ="Area adjusted", y = "Remaining crime rate for region") +
    annotate(
      geom = "curve", x = NumContributed+4, y = 1.5,
      xend = NumContributed+1, yend = 0.2,
      curvature = .2, arrow = arrow(length = unit(2, "mm"))
    ) +
    annotate(geom = "text", x = NumContributed+4.1, y = 1.5,
             label = ODI.text, hjust = "left") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +

    annotate(geom = "text", x = 2, y = df2$pct[1],
             label = paste0(format(df2$pct[1], digits = 3),"% overall"), hjust = "left") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  if (plot.adjustment != "") {
    p <- p +
      annotate(geom = "text", x = 100, y = df3$pct[1]-1, label = plot.adjustment, hjust = "right")
  }

  p


  # Create return list ------------------------------------------------------

  output <- list(df2, p, NumContributed, ODI, NCDI)
  return(output)
}

