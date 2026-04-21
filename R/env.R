#' @title Initialize a seasonal adjustment project environment
#'
#' @description
#' This function creates a complete project structure for a seasonal adjustment
#' production workflow. It initializes an R project, sets up useful directories,
#' configuration files, and development tools.
#'
#' The generated structure is designed for workflows based on the 'rjdverse'.
#'
#' @param path A character string. Path where the project will be created.
#' @param open Boolean. Should the project be opened in RStudio after creation?
#' Default is \code{FALSE}.
#'
#' @returns The project path invisibly.
#'
#' @examples
#' project_path <- tempfile(pattern = "my-project")
#'
#' \dontrun{
#' # Create a new project
#' init_env(path = project_path)
#' }
#'
#' @export
#' @importFrom usethis create_project use_readme_rmd use_git use_description
#' @importFrom lintr use_lintr
#'
init_env <- function(path, open = FALSE) {
    if (dir.exists(path)) {
        stop("The project exist already.")
    }

    old_path <- getwd()
    on.exit(expr = {
        setwd(old_path)
    })
    setwd(path)

    dir.create(path, recursive = TRUE)
    usethis::create_project(rstudio = TRUE, path = path, open = open)

    file.create(file.path(path, "README.Rmd"))
    writeLines(
        text = paste0(
            "# ",
            basename(path),
            "\n\nCha\UEEne de production de d\U0E9saisonnalisation. \n\n Structure du projet :",
            paste(
                "un dossier `data/` : nos donn\U0E9es brutes",
                "un dossier `Workspaces/` : nos workspaces",
                "un dossier `output/` : les s\U0E9ries, tableaux et graphiques en sortie",
                "un dossier `specs/` : les sp\U0E9cifications propres au workspace (r\U0E9gresseurs de calendrier, outliers...)",
                "un dossier `BQ/` : les bilans qualit\U0E9 et fichiers de d\U0E9cisions",
                "un fichier DESCRIPTION pour g\U0E9rer les d\U0E9pendances de notre projet",
                "un fichier `.lintr` pour faire l'analyse statique du code (bonnes pratiques de formattage)",
                "un fichier README.md pour expliquer notre projet",
                sep = "\n- "
            )
        ),
        con = file.path(path, "README.Rmd")
    )

    lintr::use_lintr(path = path)
    writeLines(
        text = "linters: all_linters(
    indentation_linter = lintr::indentation_linter(indent = 4L),
    line_length_linter = lintr::line_length_linter(80L),
    return_linter = NULL,
    library_call_linter = NULL,
    undesirable_function_linter = NULL
    )
encoding: \"UTF-8\"
exclusions: list(\"renv\", \"packrat\")
",
        con = file.path(path, ".lintr")
    )

    dir.create(file.path(path, "data"))
    dir.create(file.path(path, "Workspaces"))
    dir.create(file.path(path, "Workspaces", "workspace_N_1"))
    dir.create(file.path(path, "Workspaces", "workspace_ref"))
    dir.create(file.path(path, "Workspaces", "workspace_auto"))
    dir.create(file.path(path, "Workspaces", "workspace_travail"))
    dir.create(file.path(path, "Workspaces", "workspace_final"))
    dir.create(file.path(path, "output"))
    dir.create(file.path(path, "specs"))
    dir.create(file.path(path, "BQ"))

    file.create(file.path(path, ".Renviron"))
    file.create(file.path(path, ".Rprofile"))

    usethis::use_description(
        fields = list(
            Imports = "rjd3toolkit, rjd3x13, rjd3providers, rjd3workspace, rjd3production",
            Suggests = "devtools, usethis, remotes, cyclocomp, lintr, rmarkdown"
        ),
        check_name = FALSE
    )

    system(paste("git -C", normalizePath(path), "init"))
    # usethis::use_git(message = "Nouveau projet de d\U0E9saisonnalisation !")

    return(invisible(path))
}
