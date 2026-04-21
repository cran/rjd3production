merge_lists <- function(list1, list2, verbose = TRUE) {
    intersect_elts <- intersect(names(list1), names(list2))
    if (length(intersect_elts) > 0L && verbose) {
        message(
            intersect_elts,
            " are present in the 2 objects and it won't be merged."
        )
    }
    setdiff_elts <- setdiff(names(list2), names(list1))
    if (length(setdiff_elts) > 0L && verbose) {
        message(
            setdiff_elts,
            " are present in the second object and will be added to the first one."
        )
    }
    return(c(list1, list2[setdiff_elts]))
}

#' @importFrom rjd3toolkit modelling_context
merge_contexts <- function(context1 = NULL, context2 = NULL, verbose = TRUE) {
    if (is.null(context2)) {
        return(context1)
    } else if (is.null(context1)) {
        return(context2)
    }

    new_context <- rjd3toolkit::modelling_context(
        calendars = merge_lists(
            context1$calendars,
            context2$calendars,
            verbose
        ),
        variables = merge_lists(context1$variables, context2$variables, verbose)
    )
    return(new_context)
}

#' @importFrom rjd3workspace jws_sap sap_sai_count jsap_sai sai_name read_sai
#' @importFrom rjd3workspace set_specification set_domain_specification set_name
#' @importFrom rjd3toolkit add_outlier
#' @family regression tools
#' @rdname regression_tools
#' @export
assign_outliers <- function(jws, outliers, verbose = TRUE) {
    jsap <- rjd3workspace::jws_sap(jws, 1L)

    for (id_sai in seq_len(rjd3workspace::sap_sai_count(jsap))) {
        jsai <- rjd3workspace::jsap_sai(jsap, idx = id_sai)
        series_name <- rjd3workspace::sai_name(jsai)
        if (verbose) {
            cat(paste0(
                "S\u00e9rie ",
                series_name,
                ", ",
                id_sai,
                "/",
                rjd3workspace::sap_sai_count(jsap),
                "\n"
            ))
        }

        # Outliers
        outliers_series <- outliers[outliers$series == "RF1011", , drop = FALSE]

        if (nrow(outliers_series) > 0L) {
            # Création de la spec
            sai <- rjd3workspace::read_sai(jsai)
            new_estimationSpec <- estimationSpec <- sai$estimationSpec
            new_domainSpec <- domainSpec <- sai$domainSpec

            new_domainSpec <- rjd3toolkit::add_outlier(
                x = domainSpec,
                type = outliers_series$type,
                date = outliers_series$date
            )
            new_estimationSpec <- rjd3toolkit::add_outlier(
                x = estimationSpec,
                type = outliers_series$type,
                date = outliers_series$date
            )

            rjd3workspace::set_specification(
                jsap = jsap,
                idx = id_sai,
                spec = new_estimationSpec
            )
            rjd3workspace::set_domain_specification(
                jsap = jsap,
                idx = id_sai,
                spec = new_domainSpec
            )
            rjd3workspace::set_name(jsap, idx = id_sai, name = series_name)
        }
    }
    return(invisible(jws))
}

#' @importFrom rjd3workspace jws_sap sap_sai_count jsap_sai sai_name read_sai
#' @importFrom rjd3workspace set_specification set_domain_specification set_name
#' @importFrom rjd3workspace get_context
#' @importFrom rjd3toolkit set_tradingdays
#' @family regression tools
#' @rdname regression_tools
#' @export
assign_td <- function(jws, td, verbose = TRUE) {
    if (nrow(td) == 0L) {
        return(invisible(jws))
    }

    context <- rjd3workspace::get_context(jws)
    var_names <- get_named_variables(context)
    if (!all(td$regs %in% c("No_TD", names(var_names)))) {
        stop(
            setdiff(td$regs, c("No_TD", names(var_names))),
            " variables are not present in the WS.",
            " Please use the function `merge_contexts()` ",
            "to update your modelling context."
        )
    }
    jsap <- rjd3workspace::jws_sap(jws, 1L)

    for (id_sai in seq_len(rjd3workspace::sap_sai_count(jsap))) {
        jsai <- rjd3workspace::jsap_sai(jsap, idx = id_sai)
        series_name <- rjd3workspace::sai_name(jsai)
        if (verbose) {
            cat(paste0(
                "S\u00e9rie ",
                series_name,
                ", ",
                id_sai,
                "/",
                rjd3workspace::sap_sai_count(jsap),
                "\n"
            ))
        }
        chosen_set <- td[td$series == series_name, "regs"]
        if (length(chosen_set) == 1L && chosen_set != "No_TD") {
            td_variables <- var_names[[chosen_set]]

            sai <- rjd3workspace::read_sai(jsai)
            new_estimationSpec <- estimationSpec <- sai$estimationSpec
            new_domainSpec <- domainSpec <- sai$domainSpec
            new_domainSpec <- rjd3toolkit::set_tradingdays(
                x = domainSpec,
                option = "UserDefined",
                uservariable = td_variables,
                test = "None"
            )
            new_estimationSpec <- rjd3toolkit::set_tradingdays(
                x = estimationSpec,
                option = "UserDefined",
                uservariable = td_variables,
                test = "None"
            )
            rjd3workspace::set_specification(
                jsap = jsap,
                idx = id_sai,
                spec = new_estimationSpec
            )
            rjd3workspace::set_domain_specification(
                jsap = jsap,
                idx = id_sai,
                spec = new_domainSpec
            )
            rjd3workspace::set_name(jsap, idx = id_sai, name = series_name)
        }
    }

    return(invisible(jws))
}
