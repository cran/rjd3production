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
#| label: setup-rjd3production
library("rjd3production")


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
#| warning: false
#| label: init-project

# path_project <- tempfile(pattern = "my_sa_project")
# init_env(path = path_project)


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false
#| label: setup-rjd3toolkit

library("rjd3toolkit")
path_ABS <- system.file("extdata", "ABS.csv", package = "rjd3providers")
my_data <- ABS[, seq_len(3L)]
colnames(my_data) <- substr(colnames(my_data), start = 2L, stop = 12L)


## -----------------------------------------------------------------------------
#| echo: true
#| label: select-td

td <- select_td(my_data)


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false
#| label: setup-rjd3workspace

library("rjd3workspace")


## -----------------------------------------------------------------------------
#| echo: true
#| label: create-context

my_context <- create_insee_context(s = my_data[, 1L])


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false
#| label: setup-rjd3x13

library("rjd3x13")


## -----------------------------------------------------------------------------
#| echo: true
#| label: create-ws-from-0

jws <- jws_new(modelling_context = my_context)
jsap <- jws_sap_new(jws, "Nouveau SAP")
add_sa_item(jsap = jsap, name = "Première série", x = my_data[, 1L], spec = x13_spec())
add_sa_item(jsap = jsap, name = "Seconde série", x = my_data[, 2L], spec = x13_spec())
#... avec autant de commande que de séries


## -----------------------------------------------------------------------------
#| echo: true
#| label: create-ws-from-data

jws <- create_ws_from_data(my_data)
set_context(jws, create_insee_context(s = my_data))


## -----------------------------------------------------------------------------
#| echo: true
#| label: assign-td

jws_compute(jws)
assign_td(td = td, jws = jws)


## -----------------------------------------------------------------------------
#| echo: true
#| label: update-ts-metadata

add_raw_data_path(jws, path_ABS, delimiter = "COMMA")


## -----------------------------------------------------------------------------
#| echo: true
#| eval: false
#| label: save-workspace

# path_ws <- file.path(path_project, "Workspaces", "workspace_travail", "my_ws.xml")
# save_workspace(jws, path_ws, replace = TRUE)

