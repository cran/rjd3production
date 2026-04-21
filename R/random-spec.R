random_flag <- function() {
    random_choice(x = c(NA, TRUE, FALSE))
}

random_name <- function(n = n) {
    nom <- paste(
        sample(x = c(0L:9L, letters), size = n, replace = TRUE),
        collapse = ""
    )
    return(nom)
}

random_choice <- function(x) {
    sample(x = x, size = 1L)
}

#' @importFrom stats runif
random_numeric_or_null <- function() {
    random_choice(list(NULL, NA_real_, stats::runif(1L)))[[1L]]
}

random_span <- function() {
    out <- list()

    out$type <- random_choice(c(
        NA_character_,
        "All",
        "From",
        "To",
        "Between",
        "Last",
        "First",
        "Excluding"
    ))
    val_n0 <- random_choice(0L:20L)
    val_n1 <- random_choice(0L:20L)
    val_d0 <- base::as.Date(sample.int(15000L, size = 1L))
    val_d1 <- base::as.Date(val_d0 + sample.int(5000L, size = 1L))
    if (is.na(out$type)) {
        out$d0 <- format(val_d0)
        out$d1 <- format(val_d1)
        out$n0 <- val_n0
        out$n1 <- val_n1
    } else if (out$type == "From") {
        out$d0 <- format(val_d0)
    } else if (out$type == "To") {
        out$d1 <- format(val_d1)
    } else if (out$type == "Between") {
        out$d0 <- format(val_d0)
        out$d1 <- format(val_d1)
    } else if (out$type == "Last") {
        out$n1 <- val_n1
    } else if (out$type == "First") {
        out$n0 <- val_n0
    } else if (out$type == "Excluding") {
        out$n0 <- val_n0
        out$n1 <- val_n1
    }

    return(out)
}

#' @importFrom rjd3toolkit add_outlier
#' @importFrom stats rnorm
random_add_outlier <- function(x) {
    args <- list(x = x)

    n <- sample.int(15L, size = 1L)
    args$type <- sample(c("AO", "LS", "TC", "SO"), size = n, replace = TRUE)
    args$date <- as.character(as.Date(sample.int(20000L, size = n)))
    args$coef <- sample(c(rep(0.0, n), stats::rnorm(n)), size = n)
    args$name <- sample(
        x = c(
            paste0(args$type, " (", args$date, ")"),
            paste0(args$type, seq_len(n), "_rnd")
        ),
        size = n
    )

    output <- do.call(rjd3toolkit::add_outlier, args)
    return(output)
}

#' @importFrom rjd3x13 set_x11
#' @importFrom stats runif
random_set_x11 <- function(x) {
    args <- list(x = x)

    args$mode <- random_choice(c(
        NA_character_,
        "Undefined",
        "Additive",
        "Multiplicative",
        "LogAdditive",
        "PseudoAdditive"
    ))
    args$seasonal.comp <- random_flag()
    args$seasonal.filter <- random_choice(c(
        NA_character_,
        "Msr",
        "Stable",
        "X11Default",
        "S3X1",
        "S3X3",
        "S3X5",
        "S3X9",
        "S3X15"
    ))
    args$henderson.filter <- random_choice(c(0L, 2L * seq_len(25L) + 1L))
    args$lsigma <- stats::runif(n = 1L, 0.6, 3.0)
    args$usigma <- stats::runif(n = 1L, 3.0, 10.0)
    args$bcasts <- random_choice(0L:30L)
    args$fcasts <- random_choice(0L:30L)
    args$calendar.sigma <- random_choice(c("None", "All", "Signif", "Select"))
    args$exclude.forecast <- random_flag()
    args$sigma.vector <- random_choice(list(NULL, 1L, 2L))[[1L]]

    output <- do.call(rjd3x13::set_x11, args)
    return(output)
}

#' @importFrom rjd3toolkit set_transform
random_set_transform <- function(x) {
    args <- list(x = x)

    args$fun <- random_choice(c(NA_character_, "None", "Auto", "Log"))
    args$adjust <- random_choice(c(
        NA_character_,
        "None",
        "LeapYear",
        "LengthOfPeriod"
    ))
    args$outliers <- random_flag()
    args$aicdiff <- random_numeric_or_null()

    output <- do.call(rjd3toolkit::set_transform, args)
    return(output)
}

#' @importFrom rjd3toolkit set_easter
random_set_easter <- function(x) {
    args <- list(x = x)

    args$enabled <- random_flag()
    args$julian <- random_flag()
    args$duration <- random_choice(1L:20L)
    args$test <- random_choice(c("Add", "Remove", "None"))
    args$coef <- random_numeric_or_null()
    args$coef.type <- random_choice(c(NA_character_, "Estimated", "Fixed"))

    output <- do.call(rjd3toolkit::set_easter, args)
    return(output)
}

#' @importFrom rjd3toolkit set_tradingdays
#' @importFrom stats runif
random_set_tradingdays <- function(x) {
    args <- list(x = x)

    args$option <- random_choice(c(
        NA_character_,
        "TradingDays",
        "WorkingDays",
        "TD2c",
        "TD3",
        "TD3c",
        "TD4",
        "None",
        "UserDefined"
    ))

    args$coef <- random_choice(list(NULL, NA_real_, stats::runif(1L)))[[1L]]
    args$leapyear.coef <- random_choice(list(
        NULL,
        NA_real_,
        stats::runif(1L)
    ))[[1L]]
    args$test <- random_choice(c(NA_character_, "None", "Remove", "Add"))

    if (is.na(args$option) || args$option == "None") {
        args$stocktd <- random_choice(list(NA_integer_, NULL, 0L, 1L, 2L))[[1L]]
        args$test <- "None"
        args$coef <- NULL
    } else if (args$option == "UserDefined") {
        args$uservariable <- random_name(6L)
    }

    if (!is.null(args$coef) || !is.null(args$leapyear.coef)) {
        args$test <- "None"
    }

    args$calendar.name <- random_choice(c(NA_character_, "calA", "calB"))
    args$coef.type <- random_choice(c(NA_character_, "Fixed", "Estimated"))
    args$automatic <- random_choice(c(
        NA_character_,
        "Unused",
        "WaldTest",
        "Aic",
        "Bic"
    ))
    args$autoadjust <- random_flag()
    args$leapyear <- random_choice(c(
        NA_character_,
        "LeapYear",
        "LengthOfPeriod",
        "None"
    ))
    args$leapyear.coef.type <- random_choice(c(
        NA_character_,
        "Fixed",
        "Estimated"
    ))

    output <- do.call(rjd3toolkit::set_tradingdays, args)
    return(output)
}

#' @importFrom rjd3toolkit set_arima
#' @importFrom stats rnorm
random_set_arima <- function(x) {
    args <- list(x = x)

    args$mean <- random_choice(c(NA_integer_, 0L, -2L:2L))
    args$mean.type <- random_choice(c(
        NA_character_,
        "Undefined",
        "Fixed",
        "Initial"
    ))
    args$p <- random_choice(c(NA_integer_, 0L:3L))
    args$d <- random_choice(c(NA_integer_, 0L:2L))
    args$q <- random_choice(c(NA_integer_, 0L:3L))
    args$bp <- random_choice(c(NA_integer_, 0L:2L))
    args$bd <- random_choice(c(NA_integer_, 0L:2L))
    args$bq <- random_choice(c(NA_integer_, 0L:2L))
    args$coef <- random_choice(list(
        NULL,
        stats::rnorm(sum(args$p, args$q, args$bp, args$bq, na.rm = TRUE))
    ))[[1L]]
    args$coef.type <- random_choice(c(
        NA_character_,
        "Undefined",
        "Fixed",
        "Initial"
    ))

    output <- do.call(rjd3toolkit::set_arima, args)
    return(output)
}

#' @importFrom rjd3toolkit set_automodel
#' @importFrom stats rnorm
random_set_automodel <- function(x) {
    args <- list(x = x)

    args$enabled <- random_flag()
    args$acceptdefault <- random_flag()
    args$cancel <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$ub1 <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$ub2 <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$reducecv <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$ljungboxlimit <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$tsig <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$ubfinal <- random_choice(c(NA, abs(stats::rnorm(1L))))
    args$checkmu <- random_flag()
    args$mixed <- random_flag()
    args$balanced <- random_flag()

    output <- do.call(rjd3toolkit::set_automodel, args)
    return(output)
}

#' @importFrom rjd3toolkit set_benchmarking
random_set_benchmarking <- function(x) {
    args <- list(x = x)

    args$enabled <- random_flag()
    args$target <- random_choice(c(
        NA_character_,
        "CalendarAdjusted",
        "Original"
    ))
    args$rho <- random_numeric_or_null()
    args$lambda <- random_numeric_or_null()
    args$forecast <- random_flag()
    args$bias <- random_choice(c("None", "Additive", "Multiplicative"))

    output <- do.call(rjd3toolkit::set_benchmarking, args)
    return(output)
}

#' @importFrom rjd3toolkit add_ramp
#' @importFrom stats rnorm
random_add_ramp <- function(x) {
    args <- list(x = x)

    n <- sample.int(15L, size = 1L)
    args$start <- sample.int(18000L, size = n)
    args$end <- args$start + sample.int(2000L, size = n)
    args$start <- as.character(as.Date(args$start))
    args$end <- as.character(as.Date(args$end))
    args$coef <- sample(c(rep(0.0, n), stats::rnorm(n)), size = n)
    args$name <- sample(
        x = c(
            paste0(args$type, " (", args$date, ")"),
            paste0("Ramp", seq_len(n), "_rnd")
        ),
        size = n
    )

    output <- do.call(rjd3toolkit::add_ramp, args)
    return(output)
}

#' @importFrom rjd3toolkit set_basic
random_set_basic <- function(x) {
    args <- list(x = x)

    args <- c(args, random_span())
    args$preliminary.check <- random_flag()
    args$preprocessing <- random_flag()

    output <- do.call(rjd3toolkit::set_basic, args)
    return(output)
}

#' @importFrom stats runif
#' @importFrom rjd3toolkit set_estimate
random_set_estimate <- function(x) {
    args <- list(x = x)

    args <- c(args, random_span())
    args$tol <- random_choice(list(NULL, NA_real_, abs(stats::runif(1L))))[[1L]]
    args$exact.ml <- random_flag()
    args$unit.root.limit <- random_flag()

    output <- do.call(rjd3toolkit::set_estimate, args)
    return(output)
}

#' @importFrom stats rnorm
#' @importFrom rjd3toolkit set_outlier
random_set_outlier <- function(x) {
    args <- list(x = x)

    args <- c(args, random_span())
    args$span.type <- args$type
    args$type <- NULL
    args$outliers.type <- random_choice(list(
        NA,
        sample(
            c("AO", "LS", "TC", "SO"),
            size = random_choice(seq_len(4L)),
            replace = FALSE
        )
    ))[[1L]]
    if (!anyNA(args$outliers.type)) {
        args$critical.value <- random_choice(list(
            NA,
            NULL,
            abs(stats::rnorm(length(args$outliers.type)))
        ))[[1L]]
    }
    args$tc.rate <- random_choice(c(
        NA,
        abs(random_choice(seq(0.1, 1.0, length.out = 200L)))
    ))
    args$maxiter <- random_choice(c(NA, 1L:60L))
    args$lsrun <- random_choice(c(NA, 0L:10L))
    args$method <- random_choice(c(NA_character_, "AddOne", "AddAll"))

    output <- do.call(rjd3toolkit::set_outlier, args)
    return(output)
}

#' @importFrom rjd3toolkit add_usrdefvar
#' @importFrom stats rnorm
random_add_usrdefvar <- function(x) {
    output <- x

    nb_usrdefvar <- random_choice(1L:10L)
    for (j in seq_len(nb_usrdefvar)) {
        args <- list(x = output)

        args$group <- random_name(3L)
        args$name <- random_name(4L)
        args$lag <- random_choice(0L:20L)
        args$regeffect <- random_choice(c(
            "Undefined",
            "Trend",
            "Seasonal",
            "Irregular",
            "Series",
            "SeasonallyAdjusted"
        ))

        args$coef <- random_choice(list(NULL, stats::rnorm(1L)))[[1L]]
        args$label <- random_choice(list(NULL, NA, random_name(5L)))[[1L]]

        output <- do.call(rjd3toolkit::add_usrdefvar, args)
    }

    return(output)
}

#' @title Random JDemetra+ Specifications Generator
#'
#' @description
#' `random_spec()` allows you to create a random specification based on a set
#' of helper functions (auxiliary functions).
#' These specifications are created from scratch.
#'
#' @details
#' The objective is to enable:
#'
#' * examples
#' * tests of other functions (notably for reverse engineering)
#' * other tests and demonstrations
#'
#' @returns a JD+ Specification
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' set.seed(1L)
#' spec <- random_spec()
#'
#' @name random-spec
#' @export
#' @importFrom rjd3x13 x13_spec
#'
random_spec <- function() {
    output <- rjd3x13::x13_spec("RSA3") |>
        random_add_outlier() |>
        random_add_ramp() |>
        random_add_usrdefvar() |>
        random_set_x11() |>
        random_set_automodel() |>
        random_set_arima() |>
        random_set_transform() |>
        random_set_easter() |>
        random_set_basic() |>
        random_set_estimate() |>
        random_set_outlier() |>
        random_set_tradingdays() |>
        random_set_benchmarking()

    return(output)
}
