# Plotly chart helpers, 2020/04/19
# Stefan Angrick

# Default colour palette
mycolpal <- c("#000000",
              ggthemes::gdocs_pal()(10),
              tolower(RColorBrewer::brewer.pal(7, "Spectral")))

# Calculate growth rate or simple difference
# annual diff has day_lag = 364, quarterly diff has day_lag = 91
growth_rate <- function(xts_obj, day_lag = 91, rate = TRUE, name_suffix = "") {
  # Build data set containing lagged data
  xts_obj_ref <- xts_obj

  new_max     <- max(zoo::index(xts_obj)) - lubridate::days(day_lag)
  xts_obj_ref <- xts_obj[paste0("/", new_max)]
  xts_obj_ref <- xts(rbind(matrix(NA,
                                  nrow = (nrow(xts_obj) - nrow(xts_obj_ref)),
                                  ncol = ncol(xts_obj)),
                           coredata(xts_obj_ref)),
                     order.by = index(xts_obj))

  # Calculate growth rate
  if (rate) { # In percentage terms
    growth_rate <- (xts_obj - xts_obj_ref) * 100 / xts_obj_ref
  } else {    # In terms of simple first differences
    growth_rate <- (xts_obj - xts_obj_ref)
  }

  # Add suffix to names if requested
  names(growth_rate) <- paste0(names(growth_rate), name_suffix)
  return(growth_rate)
}

# Calculate contributions to growth (first column is main variable)
# annual diff has day_lag = 364, quarterly diff has day_lag = 91
growth_contribs <- function(xts_obj, day_lag = 12, rate = TRUE,
                            name_suffix = "", check_threshold = 0.1) {
  # Build data set containing lagged data
  xts_obj_ref <- xts_obj

  new_max     <- max(zoo::index(xts_obj)) - lubridate::days(day_lag)
  xts_obj_ref <- xts_obj[paste0("/", new_max)]
  xts_obj_ref <- xts(rbind(matrix(NA,
                                  nrow = (nrow(xts_obj) - nrow(xts_obj_ref)),
                                  ncol = ncol(xts_obj)),
                           coredata(xts_obj_ref)),
                     order.by = index(xts_obj))

  # Calculate contributions to growth
  if (rate) { # In percentage terms
    xts_obj_contr <- (xts_obj - xts_obj_ref) * 100
    coredata(xts_obj_contr) <-
      sapply(xts_obj_contr,
             FUN = function(x) coredata(x) / coredata(xts_obj_ref[, 1]))
  } else {    # In terms of simple first differences
    xts_obj_contr <- (xts_obj - xts_obj_ref)
  }

  # Consistency check
  consistency_checks <- xts_obj_contr[, 1] - rowSums(xts_obj_contr[, -1])
  consistency_checks <- round(consistency_checks, 1)
  if (any(consistency_checks > check_threshold, na.rm = TRUE)) {
    warning("Consistency check for contributions of growth failed")
  }

  # Add suffix to names if requested
  names(xts_obj_contr) <- paste0(names(xts_obj_contr), name_suffix)
  return(xts_obj_contr)
}

# Plotly line plot with automatic calculation of q/q and y/y growth
plotly_line_growth <- function(xts_a, xts_b = NULL, x_lab = "", y_lab = "",
                               add_q = TRUE, add_y = TRUE,
                               rate = TRUE, colpal = mycolpal,
                               l_orientation = "h") {
  # Save variable names and colours
  var_names <- c(names(xts_a), names(xts_b))
  var_cols  <- c(setNames(colpal[seq_len(length(var_names))], var_names))

  # Add quarterly growth rates
  if (add_q) {
    var_cols <- c(var_cols,
                  setNames(colpal[seq_len(length(var_names))],
                           paste0(var_names, ifelse(rate, "_q", "_dq"))))
    xts_a_q <- growth_rate(xts_a, 91, rate, ifelse(rate, "_q", "_dq"))
    if (!is.null(xts_b)) {
      xts_b_q <- growth_rate(xts_b, 91, rate, "_q")
    }
  }

  # Add annual growth rates
  if (add_y) {
    var_cols <- c(var_cols,
                  setNames(colpal[seq_len(length(var_names))],
                           paste0(var_names, ifelse(rate, "_y", "_dy"))))
    xts_a_y <- growth_rate(xts_a, 364, rate, ifelse(rate, "_y", "_dy"))
    if (!is.null(xts_b)) {
      xts_b_y <- growth_rate(xts_b, 364, rate, "_y")
    }
  }

  # Merge everything
  if (add_q) {
    xts_a   <- xts::merge.xts(xts_a, xts_a_q)
    if (!is.null(xts_b)) {
      xts_b   <- xts::merge.xts(xts_b, xts_b_q)
    }
  }

  if (add_y) {
    xts_a   <- xts::merge.xts(xts_a, xts_a_y)
    if (!is.null(xts_b)) {
      xts_b   <- xts::merge.xts(xts_b, xts_b_y)
    }
  }

  # Set up plotly object with an x scale compatible with both xts objects
  pp <- plotly::plot_ly()

  # Add traces from xts_a
  for (i in seq_len(ncol(xts_a))) {
    pp <-
      plotly::add_trace(pp, name = colnames(xts_a)[i], x = index(xts_a),
                        y = data.frame(coredata(xts_a))[, i],
                        type = "scatter", mode = "lines",
                        line = list(color = var_cols[colnames(xts_a)[i]]),
                        visible = (colnames(xts_a)[i] %in% var_names))
  }

  # Add traces from xts_b
  if (!is.null(xts_b)) {
    for (i in seq_len(ncol(xts_b))) {
      pp <-
        plotly::add_trace(pp, name = colnames(xts_b)[i], x = index(xts_b),
                          y = data.frame(coredata(xts_b))[, i],
                          type = "scatter", mode = "lines",
                          line = list(color = var_cols[colnames(xts_b)[i]]),
                          visible = (colnames(xts_b)[i] %in% var_names))
    }
  }

  # Configure buttons
  button_list <- list(list(label = "L",
                           execute = TRUE,
                           method = "update",
                           args = list(list(
                             visible = (c(names(xts_a), names(xts_b)) %in%
                                          var_names)))))

  if (add_q) {
    button_list[[length(button_list) + 1]] <-
      list(label = ifelse(rate, "Q", "dQ"),
           method = "update",
           args = list(list(
             visible = (c(names(xts_a), names(xts_b)) %in%
                          paste0(var_names, ifelse(rate, "_q", "_dq"))))))
  }

  if (add_y) {
    button_list[[length(button_list) + 1]] <-
      list(label = ifelse(rate, "Y", "dY"),
           method = "update",
           args = list(list(
             visible = (c(names(xts_a), names(xts_b)) %in%
                          paste0(var_names, ifelse(rate, "_y", "_dy"))))))
  }

  # Configure update buttons
  updatemenus <- list(
    list(
      active = 0,
      type = "buttons",
      direction = "right",
      xanchor = "center",
      yanchor = "bottom",
      x = 0.5,
      y = 0,
      pad = list(l = 5, r = 5, b = 5, t = 5, pad = 1),
      font = list(size = 12),
      buttons = button_list
    )
  )

  # Configure plot layout
  pp <- layout(pp,
               plot_bgcolor = "rgba(235,235,235,1)",
               paper_bgcolor = "rgba(255,255,255,1)",
               margin = list(l = 5, r = 15, b = 5, t = 5, pad = 1),
               xaxis = list(title = x_lab,
                            titlefont = list(size = 12),
                            gridcolor = toRGB("white"),
                            rangeselector = list(
                              buttons = list(
                                list(count = 6, label = "6m", step = "month",
                                     stepmode = "backward"),
                                list(count = 1, label = "1y", step = "year",
                                     stepmode = "backward"),
                                list(count = 3, label = "3y", step = "year",
                                     stepmode = "backward"),
                                list(count = 5, label = "5y", step = "year",
                                     stepmode = "backward"),
                                list(step = "all")))),
               yaxis = list(title = y_lab,
                            titlefont = list(size = 12),
                            fixedrange = FALSE,
                            gridcolor = toRGB("white")),
               updatemenus = updatemenus,
               legend = list(orientation = l_orientation))
  pp <- plotly::config(pp, displaylogo = FALSE)

  return(pp)
}

# Plotly line and bar plot with automatic calculation of q/q and y/y contribs
plotly_line_bar_growth <- function(xts_obj, x_lab = "", y_lab = "",
                                   add_q = TRUE, add_y = TRUE, add_shr = TRUE,
                                   bar_mode = "relative", rate = TRUE,
                                   add_others = TRUE, check_threshold = 0.1,
                                   colpal = mycolpal, l_orientation = "h") {
  # Add column containing difference between main variable and rest
  if (add_others) {
    all_others <- xts_obj[, 1] - rowSums(xts_obj[, -1], na.rm = TRUE)
    all_others <- setNames(all_others, "oth")
    xts_obj    <- xts::merge.xts(xts_obj, all_others)
  }

  # Calculate shares of total
  shares <- data.frame(matrix(NA, 0, 0))

  if (add_shr) {
    shares <- as.data.frame(
      sapply(xts_obj[, -1], FUN = function(x)
        round(coredata(x) * 100 / coredata(xts_obj[, 1]), 2)),
      stringsAsFactors = FALSE)
  }

  # Save variable names and colours
  var_names <- c(names(xts_obj))
  var_cols  <- setNames(colpal[seq_len(length(var_names))], var_names)
  line_vars <- names(xts_obj)[1]

  # Add quarterly growth rates
  if (add_q) {
    var_cols <- c(var_cols,
                  setNames(colpal[seq_len(length(var_names))],
                           paste0(var_names, ifelse(rate, "_cq", "_dcq"))))
    xts_obj_q <- growth_contribs(xts_obj, 91, rate, ifelse(rate, "_cq", "_dcq"),
                                 check_threshold)
  }

  # Add annual growth rates
  if (add_y) {
    var_cols  <- c(var_cols,
                   setNames(colpal[seq_len(length(var_names))],
                            paste0(var_names, ifelse(rate, "_cy", "_dcy"))))
    xts_obj_y <- growth_contribs(xts_obj, 364, rate,
                                 ifelse(rate, "_cy", "_dcy"), check_threshold)
  }

  # Merge everything
  if (add_q) {
    xts_obj   <- xts::merge.xts(xts_obj, xts_obj_q)
    line_vars <- c(line_vars, names(xts_obj_q)[1])
  }

  if (add_y) {
    xts_obj   <- xts::merge.xts(xts_obj, xts_obj_y)
    line_vars <- c(line_vars, names(xts_obj_y)[1])
  }

  # Set up plotly object
  pp <- plotly::plot_ly()

  for (i in seq_len(ncol(xts_obj))) {
    if (names(xts_obj)[i] %in% line_vars) {
      pp <-
        plotly::add_trace(pp, name = names(xts_obj)[i], x = index(xts_obj),
                          y = data.frame(zoo::coredata(xts_obj))[, i],
                          type = "scatter", mode = "lines",
                          line = list(color = var_cols[names(xts_obj)[i]],
                                      width = 2),
                          visible = (colnames(xts_obj)[i] %in% var_names))
    } else {
      if (names(xts_obj)[i] %in% names(shares)) {
        pp <-
          plotly::add_trace(pp, name = names(xts_obj)[i], x = index(xts_obj),
                            y = data.frame(zoo::coredata(xts_obj))[, i],
                            text = shares[, names(xts_obj)[i]],
                            hovertemplate = paste0("%{x}, %{y}, ",
                                                   "Share: %{text}%"),
                            type = "bar",
                            marker = list(color = var_cols[names(xts_obj)[i]]),
                            visible = (colnames(xts_obj)[i] %in% var_names))
      } else {
        pp <-
          plotly::add_trace(pp, name = names(xts_obj)[i], x = index(xts_obj),
                            y = data.frame(zoo::coredata(xts_obj))[, i],
                            type = "bar",
                            marker = list(color = var_cols[names(xts_obj)[i]]),
                            visible = (colnames(xts_obj)[i] %in% var_names))
      }
    }
  }

  # Configure buttons
  button_list <-
    list(list(label = "L", execute = TRUE, method = "update",
              args = list(list(
                visible = (c(names(xts_obj)) %in%
                             var_names)))))

  if (add_q) {
    button_list[[length(button_list) + 1]] <-
      list(label = ifelse(rate, "Q", "dQ"), method = "update",
           args = list(list(
             visible = (c(names(xts_obj)) %in%
                          paste0(var_names, ifelse(rate, "_cq", "_dcq"))))))
  }

  if (add_y) {
    button_list[[length(button_list) + 1]] <-
      list(label = ifelse(rate, "Y", "dY"), method = "update",
           args = list(list(
             visible = (c(names(xts_obj)) %in%
                          paste0(var_names, ifelse(rate, "_cy", "_dcy"))))))
  }

  # Configure update buttons
  updatemenus <- list(
    list(
      active = 0,
      type = "buttons",
      direction = "right",
      xanchor = "center",
      yanchor = "bottom",
      x = 0.5,
      y = 0,
      pad = list(l = 5, r = 5, b = 5, t = 5, pad = 1),
      font = list(size = 12),
      buttons = button_list
    )
  )

  # Configure plot layout
  pp <- layout(pp,
               barmode = bar_mode,
               plot_bgcolor = "rgba(235,235,235,1)",
               paper_bgcolor = "rgba(255,255,255,1)",
               margin = list(l = 5, r = 15, b = 5, t = 5, pad = 1),
               xaxis = list(title = x_lab,
                            titlefont = list(size = 12),
                            gridcolor = toRGB("white"),
                            rangeselector = list(
                              buttons = list(
                                list(count = 6, label = "6m", step = "month",
                                     stepmode = "backward"),
                                list(count = 1, label = "1y", step = "year",
                                     stepmode = "backward"),
                                list(count = 3, label = "3y", step = "year",
                                     stepmode = "backward"),
                                list(count = 5, label = "5y", step = "year",
                                     stepmode = "backward"),
                                list(step = "all")))),
               yaxis = list(title = y_lab,
                            titlefont = list(size = 12),
                            fixedrange = FALSE,
                            gridcolor = toRGB("white")),
               updatemenus = updatemenus,
               legend = list(orientation = l_orientation))
  pp <- plotly::config(pp, displaylogo = FALSE)

  return(pp)
}
