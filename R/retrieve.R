#' @importFrom rjd3workspace read_workspace
#' @family regression tools
#' @rdname regression_tools
#' @export
retrieve_outliers <- function(
    jws,
    domain = TRUE,
    estimation = FALSE,
    point = FALSE,
    verbose = TRUE
) {
    if (domain + point + estimation != 1L) {
        stop("You have to choose one specification.")
    }

    ws <- rjd3workspace::read_workspace(jws, compute = TRUE)
    # Waiting for #108
    # if (point) {
    #     ws <- rjd3workspace::read_workspace(jws, compute = TRUE)
    # } else {
    #     ws <- rjd3workspace::read_workspace(jws, compute = FALSE)
    # }

    sap <- ws[["processing"]][[1L]]
    ps_outliers <- data.frame(
        series = character(),
        type = character(),
        date = character(),
        stringsAsFactors = FALSE
    )

    for (id_sai in seq_along(sap)) {
        series_name <- names(sap)[id_sai]

        if (verbose) {
            cat(paste0(
                "S\u00e9rie ",
                series_name,
                ", ",
                id_sai,
                "/",
                length(sap),
                "\n"
            ))
        }

        sai <- sap[[id_sai]]

        if (domain) {
            regression_section <- sai[["domainSpec"]][["regarima"]][[
                "regression"
            ]]
        } else if (estimation) {
            regression_section <- sai[["estimationSpec"]][["regarima"]][[
                "regression"
            ]]
        } else if (point) {
            regression_section <- sai[["pointSpec"]][["regarima"]][[
                "regression"
            ]]
        }

        outliers <- unique(regression_section[["outliers"]])

        if (!is.null(outliers)) {
            type <- vapply(
                X = outliers,
                FUN = base::`[[`,
                FUN.VALUE = character(1L),
                "code"
            )
            date <- vapply(
                X = outliers,
                FUN = base::`[[`,
                FUN.VALUE = double(1L),
                "pos"
            ) |>
                as.Date() |>
                as.character()

            ps_outliers <- rbind(
                ps_outliers,
                data.frame(
                    series = series_name,
                    type = type,
                    date = date
                )
            )
        }
    }

    return(ps_outliers)
}

extract_td <- function(spec) {
    regression_section <- spec[["regarima"]][["regression"]]

    regressors_td <- regression_section[["td"]]
    if (regressors_td[["td"]] != "TD_NONE") {
        return(regressors_td[["td"]])
    } else if (regressors_td[["w"]] != 0L) {
        return("STOCK_TD")
    }

    regressors_ud <- regression_section[["td"]][["users"]]
    if (is.null(regressors_ud) || length(regressors_ud) == 0L) {
        return("No_TD")
    }

    if (any(grepl(pattern = "REG1", x = regressors_ud, ignore.case = TRUE))) {
        regs_td <- "REG1"
    } else if (
        any(grepl(pattern = "REG5", x = regressors_ud, ignore.case = TRUE))
    ) {
        regs_td <- "REG5"
    } else if (
        any(grepl(pattern = "REG2", x = regressors_ud, ignore.case = TRUE))
    ) {
        regs_td <- "REG2"
    } else if (
        any(grepl(pattern = "REG3", x = regressors_ud, ignore.case = TRUE))
    ) {
        regs_td <- "REG3"
    } else if (
        any(grepl(pattern = "REG6", x = regressors_ud, ignore.case = TRUE))
    ) {
        regs_td <- "REG6"
    }
    if (
        any(
            grepl(
                pattern = "LeapYear",
                x = regressors_ud,
                ignore.case = TRUE
            ) |
                grepl(pattern = "LY", x = regressors_ud, ignore.case = TRUE)
        )
    ) {
        regs_td <- paste0(regs_td, "_LY")
    }
    return(regs_td)
}

#' @importFrom rjd3workspace read_workspace
#' @family regression tools
#' @rdname regression_tools
#' @export
retrieve_td <- function(
    jws,
    domain = TRUE,
    estimation = FALSE,
    point = FALSE,
    verbose = TRUE
) {
    if (domain + point + estimation != 1L) {
        stop("You have to choose one specification.")
    }

    ws <- rjd3workspace::read_workspace(jws, compute = TRUE)
    # Waiting for #108
    # if (point) {
    #     ws <- rjd3workspace::read_workspace(jws, compute = TRUE)
    # } else {
    #     ws <- rjd3workspace::read_workspace(jws, compute = FALSE)
    # }

    sap <- ws[["processing"]][[1L]]
    td <- data.frame(
        series = names(sap),
        regs = character(length(sap)),
        stringsAsFactors = FALSE
    )

    for (id_sai in seq_along(sap)) {
        series_name <- names(sap)[id_sai]
        if (verbose) {
            cat(paste0(
                "S\u00e9rie ",
                series_name,
                ", ",
                id_sai,
                "/",
                length(sap),
                "\n"
            ))
        }

        sai <- sap[[id_sai]]

        if (domain) {
            spec <- sai[["domainSpec"]]
        } else if (estimation) {
            spec <- sai[["estimationSpec"]]
        } else if (point) {
            spec <- sai[["pointSpec"]]
        }

        td[id_sai, "regs"] <- extract_td(spec)
    }

    return(td)
}
