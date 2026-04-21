#' @importFrom stats is.ts
regroup_ts <- function(x) {
    if (is.ts(x)) {
        return(list(x))
    }
    if (is.list(x)) {
        output <- lapply(x, FUN = regroup_ts) |>
            do.call(what = c)
        return(output)
    }
    return(NULL)
}

#' @title Extract all series from a SA-Item
#'
#' @description
#' Extracts all available time series (pre-adjustment, decomposition, and final)
#' from a seasonal adjustment item (`jsai`) inside a JDemetra+ workspace.
#'
#' @param x The object to extract the series
#' @param name Name of the SA object
#' @param ... Additional argument
#'
#' @details
#' `x` can be a Java SAI object, typically obtained via [jsap_sai()] after
#' opening and computing a workspace with [jws_open()] and [jws_compute()].
#'
#' @returns A `data.frame` with columns:
#' - `SAI`: name of the SAI,
#' - `series`: the type of series (e.g. `"y"`, `"sa"`, `"trend"`),
#' - `date`: observation dates,
#' - `value`: numeric values of the series.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#'
#' library("rjd3toolkit")
#' library("rjd3workspace")
#'
#' \donttest{
#' # Demo workspace
#' jws <- create_ws_from_data(ABS)
#' jws_compute(jws)
#' jsap <- jws_sap(jws, 1L)
#' jsai <- jsap_sai(jsap, 1L)
#'
#' df <- get_series(jsai)
#' head(df)
#' }
#'
#' @importFrom rjd3workspace read_sai sai_name
#' @importFrom zoo as.Date
#'
#' @rdname get_series
#' @export
get_series <- function(x, ...) {
    UseMethod("get_series", x)
}

#' @rdname get_series
#' @exportS3Method get_series JD3_TRAMOSEATS_RSLTS
#' @method get_series JD3_TRAMOSEATS_RSLTS
#' @export
#' @importFrom stats time
get_series.JD3_TRAMOSEATS_RSLTS <- function(x, name, ...) {
    if (is.null(x)) {
        stop("Please compute your workspace")
    }
    output <- NULL
    all_series <- regroup_ts(list(
        stochastics = x$decomposition$stochastics,
        final = x$final[-1L]
    ))

    for (s in names(all_series)) {
        series <- all_series[[s]]
        if (!is.null(series)) {
            output <- rbind(
                output,
                data.frame(
                    series = s,
                    date = series |> time() |> zoo::as.Date(),
                    value = as.numeric(series)
                )
            )
        }
    }
    return(cbind(SAI = name, output))
}

#' @rdname get_series
#' @exportS3Method get_series JD3_X13_RSLTS
#' @method get_series JD3_X13_RSLTS
#' @export
#' @importFrom stats time
get_series.JD3_X13_RSLTS <- function(x, name, ...) {
    if (is.null(x)) {
        stop("Please compute your workspace")
    }
    output <- NULL
    all_series <- c(x$preadjust, x$decomposition, x$final)
    for (s in names(all_series)) {
        series <- all_series[[s]]
        if (!is.null(series)) {
            output <- rbind(
                output,
                data.frame(
                    series = s,
                    date = series |> time() |> zoo::as.Date(),
                    value = as.numeric(series)
                )
            )
        }
    }
    return(cbind(SAI = name, output))
}

#' @rdname get_series
#' @exportS3Method get_series jobjRef
#' @method get_series jobjRef
#' @export
get_series.jobjRef <- function(x, ...) {
    output <- get_series(
        x = (rjd3workspace::read_sai(x))$results,
        name = rjd3workspace::sai_name(x)
    )
    return(output)
}

#' @title Retrieve a SA-Item by its name
#'
#' @description
#' Searches a workspace for a seasonal adjustment item (SAI) whose name matches
#' the user-supplied string and returns the corresponding object.
#'
#' @inheritParams make_ws_crunchable
#' @param series_name [character] Name of the SAI to retrieve.
#'
#' @returns A Java Seasonal Adjustment Item object (`jsai`).
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#'
#' library("rjd3toolkit")
#' library("rjd3workspace")
#' \donttest{
#' # Demo workspace
#' jws <- create_ws_from_data(ABS)
#' jws_compute(jws)
#' jsap <- jws_sap(jws, 1L)
#'
#' jsai <- get_jsai_by_name(jws, "X0.2.09.10.M")
#' df <- get_series(jsai)
#' head(df)
#' }
#'
#' @importFrom rjd3workspace jws_sap sap_sai_names jsap_sai
#'
#' @export
get_jsai_by_name <- function(jws, series_name) {
    jsap <- rjd3workspace::jws_sap(jws, idx = 1L)
    sai_names <- rjd3workspace::sap_sai_names(jsap)
    id <- which(sai_names == series_name)
    if (length(id) == 0L) {
        stop("No SAI are named after ", series_name)
    }
    if (length(id) > 1L) {
        stop("More than one SAI is named after ", series_name)
    }
    return(rjd3workspace::jsap_sai(jsap, idx = id))
}

#' @title Retrieve all the auxiliary variables from a workspace
#'
#' @description
#' Lists all the variables in a modelling context.
#'
#' @param context a modelling context
#'
#' @returns a list with all the groups and named variables
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' context_FR <- create_insee_context()
#' get_named_variables(context_FR)
#'
#' @export
#'
get_named_variables <- function(context = NULL) {
    if (is.null(context)) {
        message("Without context, the output is NULL.")
        return(invisible(NULL))
    }
    all_vars <- context$variables
    named_vars <- lapply(seq_along(all_vars), function(k) {
        paste0(names(all_vars)[k], ".", names(all_vars[[k]]))
    })
    names(named_vars) <- names(all_vars)
    return(named_vars)
}
