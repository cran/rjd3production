set.seed(2026L)

test_that("rev_set_x11 works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_x11()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_x11(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_add_ramp works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_add_ramp()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_add_ramp(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_transform works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_transform()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_transform(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_easter works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_easter()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_easter(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_basic works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_basic()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_basic(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_estimate works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_estimate()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_estimate(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_add_usrdefvar works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_add_usrdefvar()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_add_usrdefvar(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_automodel works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_automodel()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_automodel(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_arima works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        suppressWarnings({
            spec_ref <- rjd3x13::x13_spec("RSA3") |>
                random_set_arima()

            spec_test <- eval(
                expr = parse(
                    text = paste0(
                        "rjd3x13::x13_spec(\"RSA3\") |>\n",
                        rev_set_arima(spec_ref)
                    )
                ),
                envir = .GlobalEnv
            )
        })
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_benchmarking works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_benchmarking()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_benchmarking(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_outlier works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_outlier()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_outlier(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_set_tradingdays works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        spec_ref <- rjd3x13::x13_spec("RSA3") |>
            random_set_tradingdays()

        spec_test <- eval(
            expr = parse(
                text = paste0(
                    "rjd3x13::x13_spec(\"RSA3\") |>\n",
                    rev_set_tradingdays(spec_ref)
                )
            ),
            envir = .GlobalEnv
        )
        testthat::expect_identical(spec_ref, spec_test)
    }
})

test_that("rev_spec works", {
    cond_skip_java <- rjd3toolkit::get_java_version() <
        rjd3toolkit::minimal_java_version
    testthat::skip_if(
        condition = cond_skip_java,
        message = "Java version is not sufficient."
    )
    for (k in seq_len(100L)) {
        suppressWarnings({
            spec_ref <- random_spec()
            spec_test <- eval(
                expr = parse(
                    text = rev_spec(spec_ref)
                ),
                envir = .GlobalEnv
            )
        })
        testthat::expect_identical(spec_ref, spec_test)
    }
})
