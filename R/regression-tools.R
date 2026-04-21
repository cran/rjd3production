#' @title Manage regression components in JDemetra+ workspaces
#'
#' @description
#' These functions allow extracting, exporting, importing and assigning
#' regression components used in JDemetra+ workspaces.
#'
#' @inheritParams make_ws_crunchable
#'
#' @param outliers [\link[base]{data.frame}] A data.frame created with
#' [retrieve_outliers] or [import_outliers]. See Format section for more
#' information about the format of this argument.
#'
#' @param td [\link[base]{data.frame}] A data.frame created by [retrieve_td]
#' or [import_td]. See Format section for more information about the format
#' of this argument.
#'
#' @param path [character] Path to a YAML file to read or write a table.
#'
#' @param domain Boolean indicating if outliers should be extracted from
#' the domain specification.
#'
#' @param estimation Boolean indicating if outliers should be extracted
#' from the estimation specification.
#'
#' @param point Boolean indicating if outliers should be extracted
#' from the point specification.
#'
#' @details
#'
#' Two types of regression components are currently supported:
#'
#' - **Outliers**
#' - **Trading-day regressors (TD)**
#'
#' @section Format:
#'
#' \subsection{Outliers table}{
#'
#' Outliers are represented by a `data.frame` with **three columns**:
#'
#' - `series` : name of the series in the workspace.
#' - `type` : type of outlier (`AO`, `LS`, `TC` or `SO`).
#' - `date` : date of the outlier in `YYYY-MM-DD` format.
#'
#' These tables are typically created with [retrieve_outliers()] or
#' [import_outliers()].
#'
#' }
#'
#' \subsection{Trading-day table}{
#'
#' Trading-day specifications are represented by a `data.frame`
#' with **two columns**:
#'
#' - `series` : name of the series in the workspace.
#' - `regs` : name of the trading-day regressor set to apply
#' (e.g. `REG1`, `REG2`, ..., optionally with `LY`).
#'
#' These tables are typically created with [retrieve_td()] or
#' [import_td()].
#'
#' }
#'
#' @section Workflow:
#'
#' The workflow typically follows these steps:
#'
#' 1. Extract regression information from a workspace (`retrieve_XXX()`)
#' 2. Optionally export it to a YAML file (`export_XXX()`)
#' 3. Import it later from the YAML file (`import_XXX()`)
#' 4. Assign the regression specification to another workspace (`assign_XXX()`)
#'
#' @section Other:
#'
#' The assignment functions (`assign_XXX()`) modify the **first SA-Processing**
#' of the workspace.
#'
#' Currently, regression information can be extracted (`retrieve_XXX()`) from
#' the point, estimation or domainSpec, while the assignment step
#' (`assign_XXX()`) is performed in both the domainSpec and the estimationSpec.
#'
#' @returns
#'
#' - `retrieve_outliers()` and `import_outliers()` returns data.frame
#'   representing the outliers.
#' - `retrieve_td()` and `import_td()` returns data.frame representing the
#'   trading days variables
#' - `export_outliers()` and `export_td()` functions invisibly return the path
#'   of the YAML file written.
#' - `assign_XXX()` functions invisibly return the updated workspace `jws`.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#'
#' library("rjd3workspace")
#' library("rjd3toolkit")
#' \donttest{
#' my_data <- ABS[, 1:3]
#' jws <- create_ws_from_data(my_data)
#' set_context(jws, create_insee_context(start = c(2015L, 1L)))
#'
#' ## Outliers
#'
#' # Read all the outliers from a workspace
#' outs <- retrieve_outliers(jws, point = TRUE, domain = FALSE)
#'
#' # Export outliers
#' path_outs <- tempfile(pattern = "outliers-table", fileext = ".yaml")
#' export_outliers(outs, path_outs)
#'
#' # Import outliers from a file
#' outs2 <- import_outliers(path_outs)
#'
#' # Assign the outliers to a WS
#' assign_outliers(jws = jws, outliers = outs2)
#'
#'
#' ## Trading day workflow
#'
#' # Read all the td variables from a workspace
#' td <- retrieve_td(jws)
#'
#' # Export td variables
#' path_td <- tempfile(pattern = "td-table", fileext = ".yaml")
#' export_td(td, path_td)
#'
#' # Import td variable from a file
#' td2 <- import_td(path_td)
#'
#' # Select td
#' td3 <- select_td(my_data)
#'
#' # Assign the td variables to a WS
#' assign_td(jws = jws, td = td3)
#' }
#'
#' @name regression_tools
#' @family regression tools
NULL
