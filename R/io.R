#' @importFrom tools file_path_sans_ext
#' @importFrom tools file_ext
prepare_path <- function(path = NULL, object = "outliers") {
    if (is.null(path)) {
        path_dir <- file.path(tempdir(), "regression")
        if (!dir.exists(path_dir)) {
            dir.create(path_dir, showWarnings = FALSE)
        }
        path <- tempfile(
            pattern = object,
            tmpdir = path_dir,
            fileext = ".yaml"
        )
        warning("The path is missing. ", "The table will be written at ", path)
    } else if (dir.exists(path)) {
        path <- tempfile(
            pattern = "td_",
            tmpdir = path,
            fileext = ".yaml"
        )
    } else if (file.exists(path)) {
        path <- normalizePath(path)
        if (!tools::file_ext(path) %in% c("yml", "yaml")) {
            new_file_name <- path |>
                basename() |>
                tools::file_path_sans_ext() |>
                paste0(... = _, ".yaml")
            path <- file.path(dirname(path), new_file_name)
            warning(
                "Only .yml and .yaml files are accepted.",
                "The table will be written at ",
                path
            )
        }
    } else if (nzchar(tools::file_ext(path))) {
        if (!dir.exists(dirname(path))) {
            dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
        }
        if (!tools::file_ext(path) %in% c("yml", "yaml")) {
            new_file_name <- path |>
                basename() |>
                tools::file_path_sans_ext() |>
                paste0(... = _, ".yaml")
            path <- file.path(dirname(path), new_file_name)
            warning(
                "Only .yml and .yaml files are accepted.",
                "The table will be written at ",
                path
            )
        }
    } else {
        dir.create(path, showWarnings = FALSE, recursive = TRUE)
        path <- tempfile(
            pattern = object,
            tmpdir = path,
            fileext = ".yaml"
        )
    }
    return(path)
}

#' @importFrom yaml write_yaml
#' @family regression tools
#' @rdname regression_tools
#' @export
export_outliers <- function(outliers, path = NULL, verbose = TRUE) {
    path <- prepare_path(path, "outliers")
    if (verbose) {
        cat("The outliers table will be written at ", path, "\n")
    }
    yaml::write_yaml(x = outliers, file = path)
    return(invisible(path))
}

#' @importFrom yaml read_yaml
#' @importFrom tools file_ext
#' @family regression tools
#' @rdname regression_tools
#' @export
import_outliers <- function(path, verbose = TRUE) {
    if (!file.exists(path)) {
        stop("The file", path, "doesn't exist.")
    }
    if (!tools::file_ext(path) %in% c("yml", "yaml")) {
        stop("Only .yml and .yaml files are accepted.")
    }
    if (verbose) {
        cat("The outliers table will be read at ", path, "\n")
    }
    outliers <- as.data.frame(yaml::read_yaml(file = path))
    return(outliers)
}

#' @importFrom yaml write_yaml
#' @family regression tools
#' @rdname regression_tools
#' @export
export_td <- function(td, path = NULL, verbose = TRUE) {
    path <- prepare_path(path, "td")
    if (verbose) {
        cat("The td table will be written at ", path, "\n")
    }
    yaml::write_yaml(x = td, file = path)
    return(invisible(path))
}

#' @importFrom yaml read_yaml
#' @importFrom tools file_ext
#' @family regression tools
#' @rdname regression_tools
#' @export
import_td <- function(path, verbose = TRUE) {
    if (!file.exists(path)) {
        stop("The file", path, "doesn't exist.")
    }
    if (!tools::file_ext(path) %in% c("yml", "yaml")) {
        stop("Only .yml and .yaml files are accepted.")
    }
    if (verbose) {
        cat("The td table will be read at ", path, "\n")
    }
    td <- as.data.frame(yaml::read_yaml(file = path))
    return(td)
}
