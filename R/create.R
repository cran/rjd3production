#' @title French modelling context, calendar and trading days regressors.
#'
#' @description
#' These functions allow to construct the standard regressors and modelling
#' context used by INSEE for seasonal adjustment:
#'
#' - [create_french_calendar()] creates the French national calendar.
#' - [create_insee_regressors()] generates trading day regressors and leap-year
#' effect (LY).
#' - [create_insee_regressors_sets()] organizes these regressors into standard
#' sets (REG1, REG2, …, REG6, with or without LY).
#' - [create_insee_context()] combines the regressors and calendar into a
#' `modelling_context` object
#'   that can be used directly with `rjd3toolkit`.
#'
#' @param start [\link[base]{integer} vector] Start period in the format
#' `c(year, month)` (default `c(1990, 1)`).
#' @param frequency [integer] Series frequency (default `12L`).
#' @param length [integer] Series length (default `492L`).
#' @param s [\link[base]{numeric} or NULL] Optional argument for adjustment
#' (passed to `rjd3toolkit`).
#' @param cal a calendar of class `JD3_CALENDAR`.
#'
#' @returns
#' - `create_french_calendar()` returns a `national_calendar` object.
#' - `create_insee_regressors()` returns a matrix of regressors (working days
#' + LY).
#' - `create_insee_regressors_sets()` returns a list of regressor sets (`REG1`,
#' `REG2`, …, `REG6`, with or without LY).
#' - `create_insee_context()` returns a `modelling_context` object.
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' # 1. Create the French calendar
#' cal <- create_french_calendar()
#' cal
#'
#' # 2. Generate regressors
#' regs <- create_insee_regressors(start = c(2000, 1), frequency = 12, length = 240)
#' head(regs)
#'
#' # 3. Organize into standard sets
#' sets <- create_insee_regressors_sets(start = c(2000, 1), frequency = 12, length = 240)
#' names(sets)
#'
#' # 4. Build a complete context for rjd3toolkit
#' context <- create_insee_context(start = c(2000, 1), frequency = 12, length = 240)
#' context
#'
#' @name insee_modelling
NULL


#' @rdname insee_modelling
#' @importFrom rjd3toolkit national_calendar fixed_day special_day
#' @export
create_french_calendar <- function() {
    cal_FR <- rjd3toolkit::national_calendar(
        days = list(
            Bastille_day = rjd3toolkit::fixed_day(7L, 14L), # Bastille Day
            Victory_day = rjd3toolkit::fixed_day(
                5L,
                8L,
                validity = list(start = "1982-05-08")
            ), # Victoire 2nd guerre mondiale
            NEWYEAR = rjd3toolkit::special_day("NEWYEAR"), # Nouvelle année
            CHRISTMAS = rjd3toolkit::special_day("CHRISTMAS"), # Noël
            MAYDAY = rjd3toolkit::special_day("MAYDAY"), # 1er mai
            EASTERMONDAY = rjd3toolkit::special_day("EASTERMONDAY"), # Lundi de Pâques
            ASCENSION = rjd3toolkit::special_day("ASCENSION"), # attention +39 et pas 40 jeudi ascension
            WHITMONDAY = rjd3toolkit::special_day("WHITMONDAY"), # Lundi de Pentecôte (1/2 en 2005 a verif)
            ASSUMPTION = rjd3toolkit::special_day("ASSUMPTION"), # Assomption
            ALLSAINTSDAY = rjd3toolkit::special_day("ALLSAINTSDAY"), # Toussaint
            ARMISTICE = rjd3toolkit::special_day("ARMISTICE")
        )
    )

    return(cal_FR)
}

#' @importFrom rjd3toolkit calendar_td lp_variable
#' @rdname insee_modelling
#' @export
create_insee_regressors <- function(
    start = c(1990L, 1L),
    frequency = 12L,
    length = 492L,
    s = NULL,
    cal = NULL
) {
    if (is.null(cal)) {
        cal <- create_french_calendar()
    }

    groups <- list(
        REG1 = c(1L, 1L, 1L, 1L, 1L, 0L, 0L),
        REG2 = c(1L, 1L, 1L, 1L, 1L, 2L, 0L),
        REG3 = c(1L, 2L, 2L, 2L, 2L, 3L, 0L),
        REG5 = c(1L, 2L, 3L, 4L, 5L, 0L, 0L),
        REG6 = c(1L, 2L, 3L, 4L, 5L, 6L, 0L)
    )

    if (!missing(s) && !is.null(ncol(s)) && ncol(s) > 1L) {
        s <- s[, 1L]
    }

    regs_td <- lapply(
        X = groups,
        FUN = rjd3toolkit::calendar_td,
        calendar = cal,
        frequency = frequency,
        start = start,
        length = length,
        s = s
    ) |>
        do.call(what = cbind)
    cols <- colnames(regs_td) |>
        gsub(pattern = ".", replacement = "_", fixed = TRUE)
    regs_td <- cbind(
        LY = rjd3toolkit::lp_variable(
            frequency = frequency,
            start = start,
            length = length,
            s = s,
            type = "LeapYear"
        ),
        regs_td
    )
    colnames(regs_td)[-1L] <- cols

    return(regs_td)
}

#' @rdname insee_modelling
#' @export
create_insee_regressors_sets <- function(
    start = c(1990L, 1L),
    frequency = 12L,
    length = 492L,
    s = NULL,
    cal = NULL
) {
    regs_td <- create_insee_regressors(
        frequency = frequency,
        start = start,
        length = length,
        s = s,
        cal = cal
    )

    n <- colnames(regs_td)
    id_REG1 <- startsWith(n, prefix = "REG1")
    id_REG2 <- startsWith(n, prefix = "REG2")
    id_REG3 <- startsWith(n, prefix = "REG3")
    id_REG5 <- startsWith(n, prefix = "REG5")
    id_REG6 <- startsWith(n, prefix = "REG6")
    id_LY <- startsWith(n, prefix = "LY")

    REG1 <- regs_td[, id_REG1, drop = FALSE]
    attr(REG1, "class") <- c("mts", "ts", "matrix", "array")

    LY <- regs_td[, id_LY, drop = FALSE]
    attr(LY, "class") <- c("mts", "ts", "matrix", "array")

    REG2 <- regs_td[, id_REG2]
    colnames(REG2) <- substr(colnames(REG2), 6L, 50L)

    REG3 <- regs_td[, id_REG3]
    colnames(REG3) <- substr(colnames(REG3), 6L, 50L)

    REG5 <- regs_td[, id_REG5]
    colnames(REG5) <- substr(colnames(REG5), 6L, 50L)

    REG6 <- regs_td[, id_REG6]
    colnames(REG6) <- substr(colnames(REG6), 6L, 50L)

    REG1_LY <- regs_td[, id_REG1 | id_LY]

    REG2_LY <- regs_td[, id_REG2 | id_LY]
    colnames(REG2_LY)[-1L] <- substr(colnames(REG2_LY)[-1L], 6L, 50L)

    REG3_LY <- regs_td[, id_REG3 | id_LY]
    colnames(REG3_LY)[-1L] <- substr(colnames(REG3_LY)[-1L], 6L, 50L)

    REG5_LY <- regs_td[, id_REG5 | id_LY]
    colnames(REG5_LY)[-1L] <- substr(colnames(REG5_LY)[-1L], 6L, 50L)

    REG6_LY <- regs_td[, id_REG6 | id_LY]
    colnames(REG6_LY)[-1L] <- substr(colnames(REG6_LY)[-1L], 6L, 50L)

    sets <- list(
        REG1 = REG1,
        REG2 = REG2,
        REG3 = REG3,
        REG5 = REG5,
        REG6 = REG6,
        LY = LY,
        REG1_LY = REG1_LY,
        REG2_LY = REG2_LY,
        REG3_LY = REG3_LY,
        REG5_LY = REG5_LY,
        REG6_LY = REG6_LY
    )

    return(sets)
}

#' @importFrom rjd3toolkit modelling_context
#' @rdname insee_modelling
#' @export
create_insee_context <- function(
    start = c(1990L, 1L),
    frequency = 12L,
    length = 492L,
    s = NULL
) {
    cal_fr <- create_french_calendar()
    variables_fr <- create_insee_regressors_sets(
        start = start,
        frequency = frequency,
        length = length,
        s = s
    )
    context <- rjd3toolkit::modelling_context(
        variables = variables_fr,
        calendars = list(FR = cal_fr)
    )
    return(context)
}


#' @title Creating a set of X13 specifications
#'
#' @description
#' Builds a set of X13 specifications from a start date,
#' a modelling context (explanatory variables) and outliers (optional).
#'
#' @param spec_0 Basic specification
#' @param span_start [character] Estimation start date (format "YYYY-MM-DD").
#' @param context [list] Modeling context created by
#' [rjd3toolkit::modelling_context()].
#' @param outliers [\link[base]{list} or NULL] Optional list with elements :
#' - `type`: vector of outlier types (e.g. "AO", "LS", "TC")
#' - `date`: vector of corresponding dates
#'
#' @returns A list of named X13 specifications (TD and variants).
#'
#' @examplesIf rjd3toolkit::get_java_version() >= rjd3toolkit::minimal_java_version
#' my_context <- create_insee_context()
#' create_specs_set(context = my_context)
#'
#' @importFrom rjd3x13 x13_spec
#' @importFrom rjd3toolkit set_estimate add_outlier set_tradingdays
#' @export
create_specs_set <- function(
    spec_0 = NULL,
    context = NULL,
    outliers = NULL,
    span_start = NULL
) {
    if (is.null(context)) {
        context <- create_insee_context()
    }
    var_names <- get_named_variables(context)
    if (is.null(spec_0)) {
        spec_0 <- rjd3x13::x13_spec(name = "RSA3")
    }
    if (!is.null(span_start)) {
        spec_0 <- rjd3toolkit::set_estimate(
            x = spec_0,
            type = "From",
            d0 = span_start
        )
    }
    if (!is.null(outliers)) {
        spec_0 <- spec_0 |>
            rjd3toolkit::add_outlier(
                type = outliers$type,
                date = as.character(outliers$date)
            )
    }
    specs_set <- c(
        list(No_TD = spec_0),
        lapply(
            X = var_names,
            FUN = rjd3toolkit::set_tradingdays,
            x = spec_0,
            option = "UserDefined",
            test = "None",
            calendar.name = NA
        )
    )
    return(specs_set)
}
