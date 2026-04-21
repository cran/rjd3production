#' @title Remove non-significant outliers from a JDemetra+ workspace
#'
#' @description
#' This function scans a JDemetra+ workspace (`.xml`) and removes
#' regression outliers whose p-values are above a given threshold.
#' Both the estimation specification and the domain specification are
#' updated accordingly, and the workspace file is saved in place.
#'
#' Typical use case: after estimation with user pre-specified outliers, outliers with
#' weak statistical significance (e.g. `p > 0.3`) are dropped to
#' simplify the regression specification.
#'
#' @param ws_path [\link[base]{character}] Path to a JDemetra+ workspace file
#' (usually with extension `.xml`).
#' @param threshold [\link[base]{numeric}] Maximum p-value for keeping
#' an outlier. Outliers with `Pr(>|t|) > threshold` are removed.
#' Default is `0.3`.
#' @param domain Boolean indicating if the domain specification should be
#' modified.
#' @param estimation Boolean indicating if the estimation specification should
#' be modified.
#' @inheritParams make_ws_crunchable
#'
#' @details
#' The function:
#'
#' - iterates over all the series (SA-Items) in the workspace,
#' - identifies outliers in the `regarima` specification,
#' - checks their p-values in the pre-processing regression summary,
#' - removes those with p-values above the threshold from both
#'   `estimationSpec` and, if present, `domainSpec`,
#' - saves the workspace file.
#'
#' @returns
#' The function invisibly returns `NULL`, but it **modifies the workspace file
#' in place** (saved at the same location as `ws_path`).
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#'
#' library("rjd3workspace")
#' library("rjd3x13")
#' library("rjd3toolkit")
#'
#' \donttest{
#' new_spec <- x13_spec() |>
#'     add_outlier(type = "LS", date = "1990-01-01")
#' jws <- create_ws_from_data(x = ABS[, 1, drop = FALSE], spec = new_spec)
#' path_ws <- tempfile(pattern = "ws", fileext = ".xml")
#' save_workspace(jws, file = path_ws)
#'
#' # Remove non-significant outliers (p > 0.3) from a workspace
#' remove_non_significative_outliers(path_ws, threshold = 0.3, domain = TRUE)
#' }
#'
#' @importFrom rjd3workspace jws_open jws_compute jws_sap sap_sai_count jsap_sai
#' @importFrom rjd3workspace read_sai sai_name set_specification
#' @importFrom rjd3workspace set_domain_specification set_name save_workspace
#' @importFrom rjd3toolkit remove_outlier
#' @importFrom tools file_path_sans_ext
#' @export
remove_non_significative_outliers <- function(
    ws_path,
    threshold = 0.3,
    domain = FALSE,
    estimation = FALSE,
    verbose = TRUE
) {
    if (!domain && !estimation) {
        warning(
            "No SA-Items will be modified if neither domainspec nor estimationspec are selected."
        )
        return(invisible(NULL))
    }
    ws_name <- tools::file_path_sans_ext(basename(ws_path))
    if (verbose) {
        cat("\n\U1F3F7 WS ", ws_name, "\n")
    }
    jws <- rjd3workspace::jws_open(file = ws_path)
    rjd3workspace::jws_compute(jws)
    jsap <- rjd3workspace::jws_sap(jws, 1L)
    nb_sai <- rjd3workspace::sap_sai_count(jsap)

    outliers_table <- data.frame(
        series = character(),
        name = character(),
        stringsAsFactors = FALSE
    )

    for (id_sai in seq_len(nb_sai)) {
        if (verbose) {
            cat("\U1F4CC SAI n\UB0", id_sai, "\n")
        }
        jsai <- rjd3workspace::jsap_sai(jsap, idx = id_sai)
        sai <- rjd3workspace::read_sai(jsai)
        series_name <- rjd3workspace::sai_name(jsai)
        new_estimationSpec <- estimationSpec <- sai$estimationSpec
        new_domainSpec <- domainSpec <- sai$domainSpec
        outliers <- estimationSpec$regarima$regression$outliers
        outliers_domain <- domainSpec$regarima$regression$outliers
        outliers_name_domain <- do.call(
            lapply(X = outliers_domain, FUN = function(outlier) {
                paste0(outlier$code, " (", outlier$pos, ")")
            }),
            what = c
        )
        xregs <- summary(sai$results)$preprocessing$xregs
        outliers_to_remove <- NULL
        for (id_out in seq_along(outliers)) {
            outlier <- outliers[[id_out]]
            outlier_name <- paste0(outlier$code, " (", outlier$pos, ")")
            if (
                outlier_name %in%
                    rownames(xregs) &&
                    !is.na(xregs[outlier_name, "Pr(>|t|)"]) &&
                    xregs[outlier_name, "Pr(>|t|)"] > threshold
            ) {
                if (verbose) {
                    cat("\U274C Suppression de l'outlier :", outlier_name, "\n")
                }
                new_estimationSpec <- rjd3toolkit::remove_outlier(
                    new_estimationSpec,
                    type = outlier$code,
                    date = outlier$pos
                )
                if (outlier_name %in% outliers_name_domain) {
                    new_domainSpec <- rjd3toolkit::remove_outlier(
                        new_domainSpec,
                        type = outlier$code,
                        date = outlier$pos
                    )
                    if (verbose) {
                        cat("L'outlier est dans la domainSpec.\n")
                    }
                }
                outliers_to_remove <- c(outlier_name, outliers_to_remove)
            }
        }
        if (estimation) {
            rjd3workspace::set_specification(jsap, id_sai, new_estimationSpec)
        }
        if (domain) {
            rjd3workspace::set_domain_specification(
                jsap,
                id_sai,
                new_domainSpec
            )
        }
        rjd3workspace::set_name(jsap, idx = id_sai, name = series_name)
        outliers_table <- rbind(
            outliers_table,
            data.frame(series = series_name, name = outliers_to_remove)
        )
    }
    if (verbose) {
        cat("\U1F4BE Saving WS file\n")
    }
    rjd3workspace::save_workspace(
        jws = jws,
        file = ws_path,
        replace = TRUE
    )
}

#' @title Set span minimum to a value
#'
#' @param spec Specification (object of class `JD3_X13_SPEC` or
#' `JD3_TRAMOSEATS_SPEC`
#' @param d0 characters in the format "YYYY-MM-DD" to specify first date of the
#' span
#' @param model_span Boolean. Should the estimation (= model) span be modifed?
#' @param series_span Boolean. Should the series (= basic) span be modifed?
#' @param without_outliers Boolean. Should the outliers set before the starting
#' date be removed?
#' (Small crutch while waiting for the resolution of jdemetra/jdplus-main issue
#' 858.)
#'
#' @details
#' model_span = estimation_span
#' series_span = basic_span
#'
#' @importFrom zoo as.Date
#' @importFrom rjd3toolkit set_basic set_estimate
#'
#' @returns the modify specification (an `JD3_X13_SPEC` or `JD3_TRAMOSEATS_SPEC`
#'  object).
#'
#' @export
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#'
#' library("rjd3toolkit")
#' library("rjd3x13")
#' library("rjd3workspace")
#'
#' \donttest{
#' # Two demo workspaces (RSA3 and RSA5)
#' spec <- x13_spec("rsa3")
#' set_minimum_span(spec, "2012-01-01")
#' }
#'
set_minimum_span <- function(
    spec,
    d0,
    model_span = TRUE,
    series_span = TRUE,
    without_outliers = TRUE
) {
    if ((model_span || series_span) && without_outliers) {
        outliers <- spec$regarima$regression$outliers
        date <- vapply(
            X = outliers,
            FUN = base::`[[`,
            FUN.VALUE = double(1L),
            "pos"
        ) |>
            as.Date()
        cond <- date < as.Date(d0)
        if (!is.null(outliers) && any(cond)) {
            spec$regarima$regression$outliers <- outliers[!cond]
        }
    }

    if (series_span) {
        span <- d0
        current_span <- spec |>
            base::`[[`("regarima") |>
            base::`[[`("basic") |>
            base::`[[`("span") |>
            base::`[[`("d0")
        if (!is.null(current_span) && as.Date(span) < as.Date(current_span)) {
            span <- current_span
        }
        spec <- rjd3toolkit::set_basic(x = spec, type = "From", d0 = span)
    }
    if (model_span) {
        span <- d0
        current_span <- spec |>
            base::`[[`("regarima") |>
            base::`[[`("estimate") |>
            base::`[[`("span") |>
            base::`[[`("d0")
        if (!is.null(current_span) && as.Date(span) < as.Date(current_span)) {
            span <- current_span
        }
        spec <- rjd3toolkit::set_estimate(x = spec, type = "From", d0 = span)
    }
    return(spec)
}
