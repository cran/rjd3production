## -----------------------------------------------------------------------------
#| label: setup
#| eval: true
#| echo: false

knitr::opts_chunk$set(
    collapse = TRUE,
    echo = TRUE,
    eval = rjd3jars::check_java_version(silent = TRUE),
    comment = "#>"
)


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false
#| label: setup-rjd3production

library("rjd3production")


## -----------------------------------------------------------------------------
#| echo: true
#| label: "select-td"

library("rjd3toolkit")
td_table <- select_td(ABS[, seq_len(3L)])
print(td_table)


## -----------------------------------------------------------------------------
#| echo: true
#| label: "io-td"

path_td <- tempfile(pattern = "td-table", fileext = ".yaml")
export_td(td_table, path_td)
td_table2 <- import_td(path = path_td)
waldo::compare(td_table, td_table2)


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
#| label: "assign-td"

# library("rjd3workspace")
# my_ws <- jws_open("my_workspace")
# assign_td(td_table, my_ws)


## -----------------------------------------------------------------------------
#| echo: true
#| label: "structure-of-context"


str(create_insee_context(), max.level = 2L)


## -----------------------------------------------------------------------------
#| echo: true
#| label: "custom-td-creation"

series_example <- ABS[, 1L]

TD2_TB <- calendar_td(
    s = series_example,
    groups = c(1L, 1L, 1L, 2L, 1L, 0L, 0L)
)

TD3_TB <- cbind(
    calendar_td(
        s = series_example,
        groups = c(1L, 1L, 1L, 2L, 1L, 0L, 0L)
    ),
    lp_variable(s = series_example)
)
colnames(TD3_TB) <- c("group_1", "group_2", "ly")

TD6_TB <- calendar_td(
    s = series_example,
    groups = c(1L, 2L, 3L, 4L, 5L, 6L, 0L)
)

my_regressors_sets <- list(
    TD2_TB = TD2_TB,
    TD3_TB = TD3_TB,
    TD6_TB = TD6_TB
)
my_context <- modelling_context(variables = my_regressors_sets)


## -----------------------------------------------------------------------------
#| echo: true
#| label: "select-td-advanced"

my_td_table <- select_td(ABS[, seq_len(3L)], context = my_context)
print(my_td_table)

