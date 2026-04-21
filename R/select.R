#' @importFrom stats frequency time
is_compatible <- function(series, reg) {
    if (stats::frequency(series) != stats::frequency(reg)) {
        warning("The series and the regressors doesn't have same frequency.")
        return(FALSE)
    } else if (stats::time(series)[1L] < stats::time(reg)[1L]) {
        warning("The regressors starts after the beginning of the series.")
        return(FALSE)
    } else if (rev(stats::time(series))[1L] > rev(stats::time(reg))[1L]) {
        warning("The regressors ends before the end of the series.")
        return(FALSE)
    }
    return(TRUE)
}

#' @title Diagnostics Extraction on Calendar Correction with different sets of regressors
#'
#' @description
#' These functions allow to extract diagnostics from X13-Arima models with
#' different sets of calendar regressors in order to evaluate different
#' specifications and select the most appropriate calendar regressors set (with
#'  or without leap-year effect) to correct a given series.
#'
#' @details
#' - `get_LY_info()` extracts coefficient and p-value of the leap-year (LY)
#' effect.
#' - `one_diagnostic()` applies one X13 specification to a series and computes
#' diagnostics.
#' - `all_diagnostics()` evaluates all specifications in a set and summarizes
#' diagnostics.
#' - `verif_LY()` checks whether the leap-year effect should be kept or removed.
#' - `select_td_one_series()` selects the best calendar regressors set for a
#' single series.
#'
#' @param mod [list] An X13 model.
#' @param series [\link[stats]{ts} or numeric] Time series to analyse.
#' @param spec [list] A X13 specification (from [rjd3x13::x13_spec()]).
#' @param context [list] Modelling context with regressors and calendars
#'   (from [rjd3toolkit::modelling_context()]).
#' @param jeu [character] Name of the tested regression set.
#' @param diags [data.frame] Diagnostics table produced by `all_diagnostics()`.
#' @param name [character] Name of the series (for messages).
#' @param specs_set [\link[base]{list} or NULL] List of X13 specifications. If
#'   `NULL`, generated via [create_specs_set()].
#' @param ... Additional arguments passed to [create_specs_set()] controlling
#'   the generation of X13 specifications. Possible arguments include:
#'   \describe{
#'     \item{outliers}{Optional list of outliers with elements `type` (vector
#'     of types, e.g., "AO", "LS", "TC") and `date` (vector of dates).}
#'     \item{span_start}{Starting date of the estimation (character, format
#'     `"YYYY-MM-DD"`).}
#'     \item{...}{Other arguments accepted by [create_specs_set()].}
#'   }
#' @inheritParams make_ws_crunchable
#'
#' @returns
#' - `get_LY_info()` : A data.frame with `LY_coeff` and `LY_p_value`.
#' - `one_diagnostic()` : A data.frame with diagnostics for one specification.
#' - `all_diagnostics()` : A data.frame with diagnostics for all specifications.
#' - `verif_LY()` : Name of the chosen regression set (possibly without LY).
#' - `select_td_one_series()` : Name of the selected regression set.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' library("rjd3toolkit")
#'
#' # Create a modelling context
#' my_context <- create_insee_context(s = ABS)
#'
#' # Generate specification sets
#' my_set <- create_specs_set(context = my_context)
#'
#' # Extract LY info
#' mod <- rjd3x13::x13(ABS[, 1], spec = "RSA3")
#' rjd3production:::get_LY_info(summary(mod))
#'
#' # Compute diagnostics for one spec
#' spec <- my_set[[8L]]
#' rjd3production:::one_diagnostic(series = ABS[, 1], spec, context = my_context)
#'
#' # Compute diagnostics for all specs
#' rjd3production:::all_diagnostics(series = ABS[, 1], specs_set = my_set, context = my_context)
#'
#' # Check whether LY should be removed
#' diags <- rjd3production:::all_diagnostics(
#'     series = ABS[, 1],
#'     specs_set = my_set,
#'     context = my_context
#' )
#' rjd3production:::verif_LY("REG6_LY", diags)
#'
#' # Select regressions for one series
#' rjd3production:::select_td_one_series(series = ABS[, 1], context = my_context)
#'
#'@dev
#'
get_LY_info <- function(mod, verbose = TRUE) {
    ud_var <- mod$result_spec$regarima$regression$td$users
    if (
        length(ud_var) == 0L ||
            !any(grepl(pattern = ".LY", x = ud_var, fixed = TRUE))
    ) {
        return(data.frame(LY_coeff = NA, LY_p_value = NA))
    }

    smod <- summary(mod)
    reg_table <- smod$preprocessing$xregs
    idx <- grep(pattern = ".LY", x = rownames(reg_table), fixed = TRUE)
    idx2 <- grep(
        pattern = "usertd",
        x = rownames(reg_table),
        fixed = TRUE
    )
    if (length(idx) > 1L) {
        stop("Plusieurs variables portent le nom LY.")
    } else if (length(idx) == 0L && length(idx2) == 1L) {
        idx <- idx2
    }
    LY_coeff <- reg_table[idx, "Estimate"]
    LY_p_value <- reg_table[idx, "Pr(>|t|)"]

    return(data.frame(LY_coeff = LY_coeff, LY_p_value = LY_p_value))
}

#' @importFrom rjd3x13 x13
one_diagnostic <- function(series, spec, context, verbose = TRUE) {
    if (length(spec$regarima$regression$td$users) > 0L) {
        condition <- spec$regarima$regression$td$users |>
            strsplit(split = ".", fixed = TRUE) |>
            lapply(FUN = \(.x) context$variables[[.x[1L]]][[.x[2L]]]) |>
            vapply(
                FUN = is_compatible,
                FUN.VALUE = logical(1L),
                series = series
            )
        if (!all(condition)) {
            stop("One of the regressors doesn't have the good properties.")
        }
    }

    mod <- rjd3x13::x13(
        ts = series,
        spec = spec,
        context = context,
        userdefined = c("diagnostics.td-sa-all", "diagnostics.td-i-all")
    )

    # Si res_td < 0.05 -> il y a des tradings days residuals
    res_td <- sapply(
        X = mod$user_defined,
        FUN = `[[`,
        "pvalue"
    )

    # Plus la note est élevé, moins bine c'est.
    note <- sum((res_td < 0.05) * 2L:1L)
    aicc <- mod$result$preprocessing$estimation$likelihood$aicc
    mode <- c("Additive", "Multiplicative")[
        mod$result$preprocessing$description$log + 1L
    ]

    LY_info <- get_LY_info(mod, verbose = verbose)

    diag <- cbind(
        data.frame(note = note, aicc = aicc, mode = mode),
        LY_info
    )

    return(diag)
}

all_diagnostics <- function(series, specs_set, context, verbose = TRUE) {
    diags <- lapply(X = seq_along(specs_set), FUN = function(k) {
        spec <- specs_set[[k]]
        if (verbose) {
            cat("Computing spec", names(specs_set)[k], "...")
        }
        diag <- one_diagnostic(
            series = series,
            spec = spec,
            context = context,
            verbose = verbose
        )
        if (verbose) {
            cat("Done !\n")
        }
        return(diag)
    })

    diags <- do.call(what = rbind, args = diags)
    diags <- cbind(
        regs = names(specs_set),
        diags
    )
    rownames(diags) <- diags$regs

    return(diags)
}

verif_LY <- function(jeu, diags) {
    if (!grepl(pattern = "LY", x = jeu, ignore.case = TRUE)) {
        return(jeu)
    }
    id_jeu <- which(diags$regs == jeu)

    LY_coeff <- diags[id_jeu, "LY_coeff"]
    LY_p_value <- diags[id_jeu, "LY_p_value"]
    mode <- diags[id_jeu, "mode"]

    if (jeu == "LY") {
        jeu_without_LY <- "No_TD"
    } else {
        jeu_without_LY <- gsub(
            pattern = "_LY",
            replacement = "",
            x = jeu,
            ignore.case = TRUE
        )
    }
    id_jeu_without_LY <- which(diags$regs == jeu_without_LY)

    # On reprend le choix avec et sans LY
    diags_jeu <- diags[c(id_jeu, id_jeu_without_LY), ]

    if (diags_jeu$note[1L] != diags_jeu$note[2L]) {
        return(rownames(diags_jeu)[which.min(diags_jeu$note)])
    }

    if (mode == "Multiplicatif") {
        LY_coeff <- 100.0 * LY_coeff
    }
    LY_coeff <- round(LY_coeff)

    # On considere le coeff LY incoherent si negatif ou superieur à 12
    coef_incoherent <- (LY_coeff <= 0.0) | (LY_coeff > 12.0)
    # Coeff non signif si pvalue > 10%
    coef_non_signif <- LY_p_value > 0.1

    jeu_final <- ifelse(
        test = coef_incoherent | coef_non_signif,
        yes = jeu_without_LY,
        no = jeu
    )

    return(jeu_final)
}

#' @importFrom stats time
#' @importFrom utils tail
select_td_one_series <- function(
    series,
    name = "",
    specs_set = NULL,
    context = NULL,
    ...,
    verbose = TRUE
) {
    if (is.null(context)) {
        context <- create_insee_context(s = series)
    }
    if (is.null(specs_set)) {
        specs_set <- create_specs_set(context = context, ...)
    }

    if ("No_TD" %in% names(specs_set)) {
        diag_no_td <- one_diagnostic(
            series = series,
            spec = specs_set$No_TD,
            context = context,
            verbose = TRUE
        )
        # Note de 0 = note parfaite
        if (diag_no_td$note == 0L) {
            return("No_TD")
        }
    }

    diags <- all_diagnostics(
        series,
        specs_set = specs_set,
        context = context,
        verbose = verbose
    )
    diags_wo_na <- diags[!is.na(diags$note) & !is.na(diags$aicc), ]

    if (nrow(diags_wo_na) == 0L) {
        stop(
            "Erreur lors du calcul de l'aicc et des p-value.
             Aucun jeu de regresseur n'a pu \u00eatre s\u00e9lectionn\u00e9. ",
            ifelse(nzchar(name), paste0("(S\u00e9rie ", name, ")"), "")
        )
    }

    best_regs <- diags_wo_na[order(diags_wo_na$note, diags_wo_na$aicc), ]

    return(verif_LY(jeu = best_regs[1L, "regs"], diags = diags))
}

#' @title Select Calendar Regressors for One or Multiple Series
#'
#' @description
#' Applies the X13 regression selection procedure to one or more time series.
#' If multiple series are provided as columns of a matrix or data.frame, each series
#' is processed separately. The function returns the selected set of regressors for each series.
#'
#' @param series [\link[stats]{ts} or mts or matrix or \link[base]{data.frame}] A univariate time series (`ts`) or a
#'   multivariate series (columns as separate series).
#' @param context [list] Modeling context created by
#' [rjd3toolkit::modelling_context()].
#' @inheritParams get_LY_info
#'
#' @returns A data.frame with two columns:
#' \describe{
#'   \item{series}{Name of the series (column name if `series` is multivariate).}
#'   \item{regs}{Name of the selected regressor set.}
#' }
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' library("rjd3toolkit")
#'
#' \donttest{
#' # Single series
#' select_td(ABS[, 1])
#'
#' # Multiple series
#' select_td(ABS)
#'
#' # Restrict regressors sets
#' my_context <- create_insee_context(s = ABS)
#' my_context$variables <- my_context$variables[c("REG1", "REG1_LY", "REG6", "REG6_LY")]
#' select_td(ABS, context = my_context)
#' }
#' @export
#'
#' @importFrom stats is.ts is.mts
select_td <- function(series, context = NULL, ..., verbose = TRUE) {
    if (is.null(context)) {
        context <- create_insee_context(s = series)
    }
    specs_set <- create_specs_set(context = context, ...)

    # Ne marche pas avec ABS
    # if (!stats::is.ts(series)) {
    #     stop("Series must be (m)ts object.")
    # }
    if (stats::is.ts(series) && !stats::is.mts(series)) {
        attr(series, "dim") <- c(length(series), 1L)
        attr(series, "class") <- c("mts", "ts", "matrix", "array")
        colnames(series) <- "my_series"
    }

    output <- sapply(X = seq_len(ncol(series)), FUN = function(k) {
        series_name <- colnames(series)[k]
        outliers <- NULL

        # if (with_outliers) {
        #     # On récupère les outliers
        #     sai_ref <- sap_ref |> RJDemetra::get_object(which(series_name_ref == series_name))
        #     sai_mod <- sai_ref |> RJDemetra::get_model(workspace = ws_ref)
        #     regressors <- sai_mod$regarima$regression.coefficients |> rownames()
        #     regressors <- regressors[substr(regressors, 1, 2) %in% c("AO", "TC", "LS", "SO")]
        #
        #     if (length(regressors) > 0) {
        #         outliers_type <- regressors |> substr(start = 1, stop = 2)
        #         outliers_date <- regressors |>
        #             substr(start = 5, stop = nchar(regressors) - 1) |>
        #             paste0("01-", ... = _) |>
        #             as.Date(format = "%d-%m-%Y")
        #
        #         outliers_type <- outliers_type[outliers_date >= as.Date(span_start)]
        #         outliers_date <- outliers_date[outliers_date >= as.Date(span_start)]
        #
        #         if (length(outliers_date) > 0) {
        #             outliers <- list(type = outliers_type,
        #                              date = outliers_date)
        #         }
        #     }
        # }

        if (verbose) {
            cat(
                paste0(
                    "\nS\u00e9rie ",
                    series_name,
                    " en cours... ",
                    k,
                    "/",
                    ncol(series)
                ),
                "\n"
            )
        }

        return(select_td_one_series(
            series = series[, k],
            name = series_name,
            specs_set = specs_set,
            context = context,
            ...,
            verbose = verbose
        ))
    })

    output <- cbind(series = colnames(series), regs = output)
    return(as.data.frame(output))
}
