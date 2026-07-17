#' @title Deprecated functions
#'
#' @param ws_path,threshold,reference,estimation,verbose Parameters.
#'
#' @returns
#' The same value as returned by the corresponding non-deprecated function.
#' The returned object represents an encoded identifier for a spreadsheet
#' series or collection.
#'
#' @name deprecated-rjd3production
NULL

#' @rdname deprecated-rjd3production
#' @export
remove_non_significative_outliers <- function(
    ws_path,
    threshold = 0.3,
    reference = FALSE,
    estimation = FALSE,
    verbose = TRUE
) {
    .Deprecated("remove_non_significant_outliers")
    remove_non_significant_outliers(
        ws_path,
        threshold,
        reference,
        estimation,
        verbose
    )
}
