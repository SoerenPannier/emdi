#' Exports an emdiObject to an Excel File or OpenDocument Spreadsheet
#'
#' Function \code{write.excel} enables the user to export point and MSE
#' estimates as well as diagnostics from the \code{summary} to an Excel file.
#' The user can choose if the results should be reported in one or several Excel
#' sheets. Furthermore, a selection of indicators can be specified.
#' Respectively the function \code{write.ods} enables the export to OpenDocument
#' Spreadsheets. Note that while \code{write.exel} will create a single document
#' \code{write.ods} will create a group of files.
#' @param object an object of type "emdi", representing point and
#' MSE estimates.
#' @param file path and filename of the spreadsheet to create. It should end on
#' .xlsx or .ods respectively.
#' @param indicator optional character vector that selects which indicators
#' shall be returned: (i) all calculated indicators ("all");
#' (ii) each indicator name: "Mean", "Quantile_10", "Quantile_25", "Median",
#' "Quantile_75", "Quantile_90", "Head_Count",
#' "Poverty_Gap", "Gini", "Quintile_Share" or the function name/s of
#' "custom_indicator/s"; (iii) groups of indicators: "Quantiles", "Poverty" or
#' "Inequality". Note, additional custom indicators can be
#' defined as argument for model-based approaches (see also \code{\link{ebp}})
#' and do not appear in groups of indicators even though these might belong to
#' one of the groups. If the \code{model} argument is of type "fh",
#' indicator can be set to "all", "Direct", FH", or "FH_Bench" (if emdi
#' object is overwritten by function benchmark). Defaults to "all".
#' @param MSE logical. If \code{TRUE}, the MSE of the emdiObject is exported.
#' Defaults to \code{FALSE}.
#' @param CV logical. If \code{TRUE}, the CV of the emdiObject is exported.
#' Defaults to \code{FALSE}.
#' @param split logical. If \code{TRUE}, point estimates, MSE and CV are written
#' to different sheets in the Excel file. In \code{write.ods} \code{TRUE} will
#' result in different files for point estimates and their precisions.
#' Defaults to \code{FALSE}.
#' @return An Excel file is created in your working directory, or at the given
#' path. Alternatively multiple ODS files are created at the given path.
#' @details These functions create an Excel file via the package
#' \pkg{\link{openxlsx}} and ODS files via the package
#' \pkg{readODS}.
#' Both packages require a zip application to be available to \R. If this is not
#' the case the authors of \pkg{\link{openxlsx}} suggest the first of the
#' following two ways.
#' \itemize{
#' \item Install Rtools from: http://cran.r-project.org/bin/windows/Rtools/ and
#' modify the system PATH during installation.
#' \item If Rtools is installed, but no system path variable is set. One can
#' set such a variable temporarily to \R by a command like:
#' \code{Sys.setenv("R_ZIPCMD" = "PathToTheRToolsFolder/bin/zip.exe")}.
#' }
#' To check if a zip application is available they recommend the command
#' \code{shell("zip")}.
#' @seealso \code{\link{direct}}, \code{\link{emdiObject}}, \code{\link{ebp}},
#' \code{\link{fh}}
#' @examples
#' \dontrun{
#' # Loading data - population and sample data
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # Generate emdi object with two additional indicators; here via function
#' # ebp()
#' emdi_model <- ebp(
#'   fixed = eqIncome ~ gender + eqsize + cash +
#'     self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
#'     fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'   pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'   threshold = function(y) {
#'     0.6 * median(y)
#'   }, L = 50, MSE = TRUE, B = 50,
#'   custom_indicator = list(
#'     my_max = function(y) {
#'       max(y)
#'     },
#'     my_min = function(y) {
#'       min(y)
#'     }
#'   ), na.rm = TRUE, cpus = 1
#' )
#'
#' # Example 1: Export estimates for all indicators and uncertainty measures
#' # and diagnostics to Excel
#' write.excel(emdi_model,
#'   file = "excel_output_all.xlsx", indicator = "all",
#'   MSE = TRUE, CV = TRUE
#' )
#'
#' # Example 2: Single Excel sheets for point, MSE and CV estimates
#' write.excel(emdi_model,
#'   file = "excel_output_all_split.xlsx",
#'   indicator = "all", MSE = TRUE, CV = TRUE, split = TRUE
#' )
#'
#' # Example 3: Same as example 1 but for an ODS output
#' write.ods(emdi_model,
#'   file = "ods_output_all.ods", indicator = "all",
#'   MSE = TRUE, CV = TRUE
#' )
#' }
#'
#' @export
#' @importFrom openxlsx createWorkbook createStyle freezePane
#' @importFrom openxlsx addWorksheet writeData saveWorkbook
#' @importFrom openxlsx addStyle writeDataTable setColWidths
#'
write.excel <- function(object,
                        file = "excel_output.xlsx",
                        indicator = "all",
                        MSE = FALSE,
                        CV = FALSE,
                        split = FALSE) {
  writeexcel_check(
    object = object,
    file = file,
    split = split
  )

  wb <- createWorkbook()

  headlines_cs <- createStyle(
    fontColour = "#ffffff",
    halign = "center",
    valign = "center",
    fgFill = NULL,
    textDecoration = "Bold",
    border = "Bottom",
    borderStyle = "medium"
  )

  if (inherits(object, "direct")) {
    wb <- add_summary_direct(
      object = object,
      wb = wb,
      headlines_cs = headlines_cs
    )
  } else if (inherits(object, "ebp")) {
    wb <- add_summary_ebp(
      object = object, wb = wb,
      headlines_cs = headlines_cs
    )
  } else if (inherits(object, "fh")) {
    wb <- add_summary_fh(
      object = object, wb = wb,
      headlines_cs = headlines_cs
    )
  }

  if (!split && (MSE || CV)) {
    wb <- add_estims(
      object = object,
      indicator = indicator,
      wb = wb,
      headlines_cs = headlines_cs,
      MSE = MSE,
      CV = CV
    )
  } else {
    wb <- add_pointests(
      wb = wb,
      object = object,
      indicator = indicator,
      headlines_cs = headlines_cs
    )
    if (MSE || CV) {
      wb <- add_precisions(
        object = object,
        indicator = indicator,
        MSE = MSE,
        wb = wb,
        headlines_cs = headlines_cs,
        CV = CV
      )
    }
  }
  saveWorkbook(wb, file, overwrite = TRUE)
}

add_summary_ebp <- function(object, wb, headlines_cs) {
  su <- summary(object)

  title_cs <- createStyle(
    fontSize = 14,
    border = "Bottom",
    halign = "left",
    borderStyle = "thick",
    textDecoration = "bold"
  )

  df_nobs <- data.frame(Count = c(
    su$out_of_smp,
    su$in_smp, su$size_pop,
    su$size_smp
  ))
  rownames(df_nobs) <- c(
    "out of sample domains",
    "in sample domains",
    "out of sample observations",
    "in sample observations"
  )
  df_size_dom <- as.data.frame(su$size_dom)

  addWorksheet(wb, sheetName = "summary", gridLines = FALSE)

  writeData(
    wb = wb, sheet = "summary",
    x = "Empirical Best Prediction", colNames = FALSE
  )
  addStyle(
    wb = wb, sheet = "summary", cols = 1, rows = 1,
    style = title_cs, stack = TRUE
  )

  starting_row <- 5
  writeDataTable(
    x = df_nobs,
    withFilter = FALSE,
    wb = wb,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  starting_row <- starting_row + 2 + nrow(df_nobs)

  writeDataTable(
    x = df_size_dom,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  starting_row <- starting_row + 2 + nrow(df_size_dom)


  if (!is.null(su$transform)) {
    writeDataTable(
      x = su$transform,
      wb = wb,
      withFilter = FALSE,
      sheet = "summary",
      startRow = starting_row,
      startCol = 3,
      rowNames = FALSE,
      headerStyle = headlines_cs,
      colNames = TRUE,
      tableStyle = "TableStyleMedium2"
    )

    starting_row <- starting_row + 2 + nrow(su$transform)
  }


  writeDataTable(
    x = su$normality,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )
  starting_row <- starting_row + 2 + nrow(su$normality)
  writeDataTable(
    x = su$coeff_determ,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 4,
    rowNames = FALSE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  setColWidths(
    wb = wb,
    sheet = "summary",
    cols = 3:9,
    widths = "auto"
  )
  return(wb)
}

add_summary_fh <- function(object, wb, headlines_cs) {
  su <- summary(object)

  title_cs <- createStyle(
    fontSize = 14,
    border = "Bottom",
    halign = "left",
    borderStyle = "thick",
    textDecoration = "bold"
  )

  df_nobs <- data.frame(Count = c(
    su$out_of_smp,
    su$in_smp
  ))
  rownames(df_nobs) <- c(
    "out of sample domains",
    "in sample domains"
  )

  addWorksheet(wb, sheetName = "summary", gridLines = FALSE)

  writeData(
    wb = wb, sheet = "summary", x = "Fay-Herriot Approach",
    colNames = FALSE
  )
  addStyle(
    wb = wb, sheet = "summary", cols = 1, rows = 1,
    style = title_cs, stack = TRUE
  )

  starting_row <- 5
  writeDataTable(
    x = df_nobs,
    withFilter = FALSE,
    wb = wb,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  starting_row <- starting_row + 2 + nrow(df_nobs)

  if (su$model$correlation == "no") {
    estimMethods <- data.frame(su$method$method, su$model$variance,
      su$method$MSE_method,
      row.names = ""
    )
    names(estimMethods) <- c(
      "Variance estimation", "Estimated variance",
      "MSE estimation"
    )
    if (su$method$method == "reblup") {
      estimMethods$k <- su$model$k
      estimMethods <- estimMethods[, c(
        "Variance estimation",
        "Estimated variance",
        "k", "MSE estimation"
      )]
    } else if (su$method$method == "reblupbc") {
      estimMethods$k <- su$model$k
      estimMethods$mult_constant <- su$model$mult_constant
      estimMethods <- estimMethods[, c(
        "Variance estimation",
        "Estimated variance",
        "k", "mult_constant",
        "MSE estimation"
      )]
    }
  } else if (su$model$correlation == "spatial") {
    estimMethods <- data.frame(su$method$method, su$model$variance["variance"],
      su$model$variance["correlation"],
      su$method$MSE_method,
      row.names = ""
    )
    names(estimMethods) <- c(
      "Variance estimation", "Estimated variance",
      "Spatial correlation", "MSE estimation"
    )
    if (su$method$method == "reblup") {
      estimMethods$k <- su$model$k
      estimMethods <- estimMethods[, c(
        "Variance estimation",
        "Estimated variance",
        "k", "Spatial correlation",
        "MSE estimation"
      )]
    } else if (su$method$method == "reblupbc") {
      estimMethods$k <- su$model$k
      estimMethods$mult_constant <- su$model$mult_constant
      estimMethods <- estimMethods[, c(
        "Variance estimation",
        "Estimated variance",
        "k", "mult_constant",
        "Spatial correlation",
        "MSE estimation"
      )]
    }
  }


  writeDataTable(
    x = estimMethods,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = FALSE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  starting_row <- starting_row + 2 + nrow(estimMethods)


  if (!is.null(su$transform)) {
    writeDataTable(
      x = su$transform,
      wb = wb,
      withFilter = FALSE,
      sheet = "summary",
      startRow = starting_row,
      startCol = 3,
      rowNames = FALSE,
      headerStyle = headlines_cs,
      colNames = TRUE,
      tableStyle = "TableStyleMedium2"
    )

    starting_row <- starting_row + 2 + nrow(su$transform)
  }


  writeDataTable(
    x = su$normality,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )
  starting_row <- starting_row + 2 + nrow(su$normality)

  if (su$model$correlation == "no" && !(su$method$method %in%
    c("reblup", "reblupbc") |
    su$method$method == "me")) {
    writeDataTable(
      x = su$model$model_select,
      wb = wb,
      withFilter = FALSE,
      sheet = "summary",
      startRow = starting_row,
      startCol = 3,
      rowNames = FALSE,
      headerStyle = headlines_cs,
      colNames = TRUE,
      tableStyle = "TableStyleMedium2"
    )
  }


  setColWidths(
    wb = wb,
    sheet = "summary",
    cols = 3:9,
    widths = "auto"
  )
  return(wb)
}

add_summary_direct <- function(object, wb, headlines_cs) {
  su <- summary(object)

  title_cs <- createStyle(
    fontSize = 14,
    border = "Bottom",
    halign = "left",
    borderStyle = "thick",
    textDecoration = "bold"
  )

  df_nobs <- data.frame(Count = c(su$in_smp, su$size_smp))
  rownames(df_nobs) <- c(
    "in sample domains",
    "in sample observations"
  )
  df_size_dom <- as.data.frame(su$size_dom)

  addWorksheet(wb, sheetName = "summary", gridLines = FALSE)

  writeData(
    wb = wb, sheet = "summary", x = "Direct Estimation",
    colNames = FALSE
  )
  addStyle(
    wb = wb, sheet = "summary", cols = 1, rows = 1,
    style = title_cs, stack = TRUE
  )

  starting_row <- 5
  writeDataTable(
    x = df_nobs,
    withFilter = FALSE,
    wb = wb,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  starting_row <- starting_row + 2 + nrow(df_nobs)

  writeDataTable(
    x = df_size_dom,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = TRUE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  starting_row <- starting_row + 2 + nrow(df_size_dom)

  df_smp_sizes <- as.data.frame(su$smp_size_tab)
  colnames(df_smp_sizes) <- c("Domain", "Frequency")
  writeDataTable(
    x = df_smp_sizes,
    wb = wb,
    withFilter = FALSE,
    sheet = "summary",
    startRow = starting_row,
    startCol = 3,
    rowNames = FALSE,
    headerStyle = headlines_cs,
    colNames = TRUE,
    tableStyle = "TableStyleMedium2"
  )

  setColWidths(
    wb = wb,
    sheet = "summary",
    cols = 3:9,
    widths = "auto"
  )
  return(wb)
}


add_pointests <- function(object, indicator, wb, headlines_cs) {
  addWorksheet(wb, sheetName = "Point Estimators", gridLines = FALSE)

  if (is.null(indicator) || !all(indicator == "all" |
    indicator == "Quantiles" |
    indicator == "quantiles" |
    indicator == "Poverty" |
    indicator == "poverty" |
    indicator == "Inequality" |
    indicator == "inequality" |
    indicator == "Custom" |
    indicator == "custom" |
    indicator %in% names(object$ind[-1]))) {
    stop(strwrap(prefix = " ", initial = "",
                 paste0("The argument indicator is set to ", indicator, ". The
                        argument only allows to be set to all, a name of
                        estimated indicators or indicator groups as described
                        in help(estimators.emdi).")))
  }

  data <- point_emdi(object = object, indicator = indicator)$ind

  writeDataTable(
    x = data,
    sheet = "Point Estimators",
    wb = wb,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    headerStyle = headlines_cs,
    tableStyle = "TableStyleMedium2",
    withFilter = FALSE
  )

  setColWidths(
    wb = wb,
    sheet = "Point Estimators",
    cols = seq_len(ncol(data)),
    widths = "auto"
  )

  freezePane(
    wb = wb,
    sheet = "Point Estimators",
    firstRow = TRUE,
    firstCol = TRUE
  )
  return(wb)
}

add_precisions <- function(object, indicator, MSE, wb, headlines_cs, CV) {
  precisions <- mse_emdi(object = object, indicator = indicator, CV = TRUE)

  if (MSE) {
    addWorksheet(wb, sheetName = "MSE Estimators", gridLines = FALSE)

    writeDataTable(
      x = precisions$ind,
      sheet = "MSE Estimators",
      wb = wb,
      startRow = 1,
      startCol = 1,
      rowNames = FALSE,
      headerStyle = headlines_cs,
      tableStyle = "TableStyleMedium2",
      withFilter = FALSE
    )
    setColWidths(
      wb = wb,
      sheet = "MSE Estimators",
      cols = seq_len(ncol(precisions$ind)),
      widths = "auto"
    )
    freezePane(
      wb = wb,
      sheet = "MSE Estimators",
      firstRow = TRUE,
      firstCol = TRUE
    )
  }
  if (CV) {
    addWorksheet(wb, sheetName = "CV Estimators", gridLines = FALSE)

    writeDataTable(precisions$ind_cv,
      wb          = wb,
      sheet       = "CV Estimators",
      startRow    = 1,
      startCol    = 1,
      rowNames    = FALSE,
      headerStyle = headlines_cs,
      tableStyle  = "TableStyleMedium2",
      withFilter  = FALSE
    )

    setColWidths(
      wb = wb,
      sheet = "CV Estimators",
      cols = seq_len(ncol(precisions$ind_cv)),
      widths = "auto"
    )

    freezePane(
      wb = wb,
      sheet = "CV Estimators",
      firstRow = TRUE,
      firstCol = TRUE
    )
  }
  return(wb)
}

add_estims <- function(object, indicator, wb, headlines_cs, MSE, CV) {
  addWorksheet(wb, sheetName = "Estimates", gridLines = FALSE)
  data <- estimators(
    object = object, indicator = indicator,
    MSE = MSE, CV = CV
  )$ind

  writeDataTable(
    x = data,
    sheet = "Estimates",
    wb = wb,
    startRow = 1,
    startCol = 1,
    rowNames = FALSE,
    headerStyle = headlines_cs,
    tableStyle = "TableStyleMedium2",
    withFilter = FALSE
  )

  setColWidths(
    wb = wb,
    sheet = "Estimates",
    cols = seq_len(ncol(data)),
    widths = "auto"
  )

  freezePane(
    wb = wb,
    sheet = "Estimates",
    firstRow = TRUE,
    firstCol = TRUE
  )
  return(wb)
}
