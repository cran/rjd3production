#' @importFrom constructive construct
keep_format <- function(x) {
    if (is.list(x)) {
        output <- x |>
            lapply(FUN = keep_format) |>
            lapply(FUN = paste0, collapse = "\n\t")
    } else {
        output <- x |>
            constructive::construct() |>
            base::`[[`("code")
    }
    return(output)
}

rev_add_outlier <- function(x) {
    if (is.null(x$regarima$regression$outliers)) {
        return(NULL)
    }
    args <- list()
    outliers <- x$regarima$regression$outliers

    args$type <- vapply(
        X = outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "code"
    )
    args$date <- vapply(
        X = outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "pos"
    )
    args$name <- vapply(
        X = outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "name"
    )
    args$coef <- outliers |>
        lapply(FUN = "[[", "coef") |>
        lapply(FUN = "[[", "value") |>
        lapply(FUN = \(coeff) {
            if (is.null(coeff)) {
                coeff <- 0L
            }
            return(coeff)
        }) |>
        as.double()

    code <- paste0(
        "rjd3toolkit::add_outlier(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_add_ramp <- function(x) {
    if (is.null(x$regarima$regression$ramps)) {
        return(NULL)
    }
    args <- list()
    ramps <- x$regarima$regression$ramps

    args$start <- vapply(
        X = ramps,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "start"
    )
    args$end <- vapply(
        X = ramps,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "end"
    )
    args$name <- vapply(
        X = ramps,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "name"
    )
    args$coef <- ramps |>
        lapply(FUN = "[[", "coef") |>
        lapply(FUN = "[[", "value") |>
        lapply(FUN = \(coeff) {
            if (is.null(coeff)) {
                coeff <- 0L
            }
            return(coeff)
        }) |>
        as.double()

    code <- paste0(
        "rjd3toolkit::add_ramp(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_one_usrdefvar <- function(args) {
    args$label <- args$name

    group_name <- strsplit(x = args$id, split = ".", fixed = TRUE)[[1L]]
    args$group <- group_name[1L]
    args$name <- group_name[2L]
    args$id <- NULL

    if (!is.null(args$coef)) {
        args$coef <- args$coef$value
    }

    code <- paste0(
        "rjd3toolkit::add_usrdefvar(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_add_usrdefvar <- function(x) {
    if (is.null(x$regarima$regression$users)) {
        return(NULL)
    }
    code <- vapply(
        x$regarima$regression$users,
        FUN = rev_one_usrdefvar,
        FUN.VALUE = character(1L)
    ) |>
        paste(collapse = " |>\n")
    return(code)
}

rev_set_x11 <- function(x) {
    args <- x$x11

    args$lsigma <- args$lsig
    args$lsig <- NULL
    args$usigma <- args$usig
    args$usig <- NULL
    args$fcasts <- args$nfcasts
    args$nfcasts <- NULL
    args$bcasts <- args$nbcasts
    args$nbcasts <- NULL
    args$seasonal.comp <- args$seasonal
    args$seasonal <- NULL
    args$henderson.filter <- args$henderson
    args$henderson <- NULL
    args$seasonal.filter <- args$sfilters
    args$sfilters <- NULL
    args$calendar.sigma <- args$sigma
    args$sigma <- NULL
    args$sigma.vector <- args$vsigmas
    args$vsigmas <- NULL
    args$exclude.forecast <- args$excludefcasts
    args$excludefcasts <- NULL

    args$mode <- switch(
        args$mode,
        UNKNOWN = "UNDEFINED",
        args$mode
    )
    args$seasonal.filter <- gsub(
        pattern = "FILTER_",
        replacement = "",
        x = args$seasonal.filter,
        fixed = TRUE
    )
    args$bias <- switch(
        args$bias,
        RATIO = NA,
        args$bias
    )
    if (length(args$sigma.vector) == 0L) {
        args$sigma.vector <- NULL
    }

    code <- paste0(
        "rjd3x13::set_x11(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_transform <- function(x) {
    args <- x$regarima$transform

    args$fun <- switch(
        args$fn,
        LEVEL = "NONE",
        args$fn
    )
    args$fn <- NULL
    code <- paste0(
        "rjd3toolkit::set_transform(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_easter <- function(x) {
    args <- x$regarima$regression$easter
    args$enabled <- toupper(args$type) != "UNUSED"
    if (args$type == "JULIAN") {
        args$julian <- TRUE
    }

    args$type <- NULL

    if (!is.null(args$coefficient)) {
        args$coef <- args$coefficient$value
        args$coef.type <- args$coefficient$type
    }
    args$coefficient <- NULL
    code <- paste0(
        "rjd3toolkit::set_easter(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_basic <- function(x) {
    args <- c(x$regarima$basic, x$regarima$basic$span)
    args$span <- NULL
    names(args)[names(args) == "preliminaryCheck"] <- "preliminary.check"
    code <- paste0(
        "rjd3toolkit::set_basic(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_estimate <- function(x) {
    args <- c(x$regarima$estimate, x$regarima$estimate$span)
    args$span <- NULL
    code <- paste0(
        "rjd3toolkit::set_estimate(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_automodel <- function(x) {
    args <- x$regarima$automodel
    args$acceptdefault <- args$acceptdef
    args$acceptdef <- NULL
    args$ljungboxlimit <- args$ljungbox
    args$ljungbox <- NULL
    args$reducecv <- args$predcv
    args$predcv <- NULL
    args$fct <- NULL
    code <- paste0(
        "rjd3toolkit::set_automodel(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_arima <- function(x) {
    args <- c(x$regarima$arima, x$regarima$regression$mean)
    args$mean <- args$value
    args$value <- NULL
    args$mean.type <- args$type
    args$type <- NULL
    if ("phi" %in% names(args) && is.null(args$phi)) {
        args$p <- NULL
    } else if (is.null(args$phi)) {
        args$p <- 0L
    } else {
        args$p <- ncol(args$phi)
        args$coef <- c(args$coef, as.numeric(args$phi[1L, ]))
        args$coef.type <- c(args$coef.type, as.character(args$phi[2L, ]))
    }
    if ("theta" %in% names(args) && is.null(args$theta)) {
        args$q <- NULL
    } else if (is.null(args$theta)) {
        args$q <- 0L
    } else {
        args$q <- ncol(args$theta)
        args$coef <- c(args$coef, as.numeric(args$theta[1L, ]))
        args$coef.type <- c(args$coef.type, as.character(args$theta[2L, ]))
    }
    if ("bphi" %in% names(args) && is.null(args$bphi)) {
        args$bp <- NULL
    } else if (is.null(args$bphi)) {
        args$bp <- 0L
    } else {
        args$bp <- ncol(args$bphi)
        args$coef <- c(args$coef, as.numeric(args$bphi[1L, ]))
        args$coef.type <- c(args$coef.type, as.character(args$bphi[2L, ]))
    }
    if ("btheta" %in% names(args) && is.null(args$btheta)) {
        args$bq <- NULL
    } else if (is.null(args$btheta)) {
        args$bq <- 0L
    } else {
        args$bq <- ncol(args$btheta)
        args$coef <- c(args$coef, as.numeric(args$btheta[1L, ]))
        args$coef.type <- c(args$coef.type, as.character(args$btheta[2L, ]))
    }
    args$phi <- NULL
    args$theta <- NULL
    args$bphi <- NULL
    args$btheta <- NULL
    args$period <- NULL
    code <- paste0(
        "rjd3toolkit::set_arima(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_benchmarking <- function(x) {
    args <- x$benchmarking
    if (!is.null(args$target)) {
        args$target <- switch(
            args$target,
            TARGET_CALENDARADJUSTED = "CALENDARADJUSTED",
            TARGET_ORIGINAL = "ORIGINAL",
            NA
        )
    }
    if (!is.null(args$bias)) {
        args$bias <- switch(
            args$bias,
            BIAS_MULTIPLICATIVE = "MULTIPLICATIVE",
            BIAS_ADDITIVE = "ADDITIVE",
            BIAS_NONE = "NONE",
            NA
        )
    }
    code <- paste0(
        "rjd3toolkit::set_benchmarking(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_outlier <- function(x) {
    args <- c(x$regarima$outlier, x$regarima$outlier$span)
    args$outliers.type <- vapply(
        X = args$outliers,
        FUN = "[[",
        FUN.VALUE = character(1L),
        "type"
    )
    args$critical.value <- vapply(
        X = args$outliers,
        FUN = "[[",
        FUN.VALUE = numeric(1L),
        "va"
    )
    args$outliers <- NULL
    args$span <- NULL
    args$tc.rate <- args$monthlytcrate
    args$monthlytcrate <- NULL
    args$defva <- NULL
    args$span.type <- args$type
    args$type <- NULL
    code <- paste0(
        "rjd3toolkit::set_outlier(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

rev_set_tradingdays <- function(x) {
    args <- x$regarima$regression$td

    if (!is.null(args$lpcoefficient)) {
        args$leapyear.coef <- args$lpcoefficient$value
        args$leapyear.coef.type <- args$lpcoefficient$type
    }
    args$lpcoefficient <- NULL

    if (!is.null(args$tdcoefficients)) {
        args$coef <- as.numeric(args$tdcoefficients[1L, ])
        if (!all(is.na(args$coef)) && all(args$coef == 0L)) {
            args$coef <- NULL
        }
        args$coef.type <- as.character(args$tdcoefficients[2L, ])
    }
    args$tdcoefficients <- NULL

    args$calendar.name <- args$holidays
    args$holidays <- NULL

    args$automatic <- switch(
        args$auto,
        AUTO_NO = "UNUSED",
        gsub(x = args$auto, pattern = "AUTO_", replacement = "", fixed = TRUE)
    )
    args$auto <- NULL
    args$option <- switch(
        args$td,
        TD7 = "TradingDays",
        TD2 = "WorkingDays",
        gsub(x = args$td, pattern = "TD_", replacement = "", fixed = TRUE)
    )
    args$td <- NULL
    args$leapyear <- args$lp
    args$lp <- NULL

    if (
        args$option == "NONE" &&
            (length(args$users) == 0L || is.null(args$users)) &&
            !nzchar(args$calendar.name) &&
            is.null(args$coef)
    ) {
        args$stocktd <- args$w
    }
    args$w <- NULL

    args$uservariable <- args$users
    args$users <- NULL
    args$ptest1 <- NULL
    args$ptest2 <- NULL

    code <- paste0(
        "rjd3toolkit::set_tradingdays(\n\t",
        paste(names(args), "=", keep_format(args), collapse = ",\n\t"),
        "\n)"
    )
    return(code)
}

#' @title Reverse Engineering of rjd3 Specifications
#'
#' @description
#' This family of functions reconstructs executable R code from a X13
#' specification object.
#' the generated code uses only the packages \{rjd3toolkit\} and \{rjd3x13\}.
#'
#' The main entry point is `rev_spec()`, which aggregates all reverse-generating
#'  helpers.
#'
#' @param x A JDemetra+ specification object
#'
#' @details
#'
#' The functions are taking a specification (argument `x` ) as input and returns
#'  A corresponding code that generates the object `x`.
#'
#' `rev_spec()` is the main function and calls all other helper functions
#' (`rev_XXX`). These helper functions (auxiliary functions) do NOT provide
#' sufficient code to reproduce the specification, but only the part dedicated
#' to them (outliers, trading days regressors, x11 filters, etc.).
#'
#' The generated code is neither unique nor optimal.
#'
#' That is, different codes (other than the one generated by rev_spec) can
#' generate the same specification.
#' It is not optimal because it does not use
#' the default values of the functions but clearly redefines all the parameters.
#'
#' @returns
#' Each `rev_XXX()` function returns a character string containing executable R
#' code.
#' `rev_spec()` returns a complete multi-line pipeline.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#'
#' spec_init <- rjd3x13::x13_spec("RSA3") |>
#'     rjd3toolkit::set_basic(type = "All") |>
#'     rjd3toolkit::set_automodel(enabled = FALSE)
#' code <- rev_spec(spec_init)
#' cat(code)
#' spec_rebuilt <- eval(parse(text = code))
#'
#' @name translate-spec
#'
#' @export
rev_spec <- function(x) {
    code <- c(
        rev_add_outlier(x),
        rev_add_ramp(x),
        rev_add_usrdefvar(x),
        rev_set_x11(x),
        rev_set_automodel(x),
        rev_set_arima(x),
        rev_set_transform(x),
        rev_set_easter(x),
        rev_set_basic(x),
        rev_set_estimate(x),
        rev_set_outlier(x),
        rev_set_tradingdays(x),
        rev_set_benchmarking(x)
    ) |>
        paste(collapse = " |>\n") |>
        paste("rjd3x13::x13_spec() |>\n", ... = _) |>
        gsub(pattern = "\n", replacement = "\n\t", fixed = TRUE)

    return(code)
}
