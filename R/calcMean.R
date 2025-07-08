#' Calculate mean for duplicated samples
#'
#' This function computes means of numeric columns for duplicated `sampleID`s
#' up to `max.dup` and combines them with non-duplicated samples.
#'
#' @param data A list of data.frames, each containing at least `sampleID`, `Gene`, `Task`.
#' @param duplicated Logical; whether to process duplicated samples.
#' @param max.dup Integer; maximum number of duplications to consider.
#'
#' @return A list of data.frames with averaged values for duplicates.
#' @importFrom dplyr where arrange filter mutate group_by summarise first across bind_rows select
#' @importFrom stats ave na.omit
#' @importFrom magrittr %>%
#' @export
calcMean <- function(data, duplicated = TRUE, max.dup = 2) {

  MeanListInt <- vector("list", length(data))

  for (i in seq_along(data)) {

    df <- data[[i]] %>% dplyr::arrange(sampleID)

    if (duplicated) {

      # No duplicados
      notdup <- df %>%
        dplyr::filter(!sampleID %in% sampleID[duplicated(sampleID)])

      for (j in 2:max.dup) {

        # Seleccionar duplicados exactamente j veces
        dup <- df %>%
          dplyr::filter(ave(sampleID, sampleID, FUN = length) == j)

        if (!is.null(dup) && nrow(dup) > 0) {
          dup <- dup %>%
            dplyr::mutate(pair_index = rep(1:ceiling(dplyr::n() / j), each = j)[1:dplyr::n()]) %>%
            dplyr::group_by(pair_index) %>%
            dplyr::summarise(
              sampleID = dplyr::first(sampleID),
              Gene     = dplyr::first(Gene),
              Task     = dplyr::first(Task),
              dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
              .groups = "drop"
            ) %>%
            dplyr::select(-pair_index)

          notdup <- dplyr::bind_rows(notdup, dup)
        }
      }
    } else {
      notdup <- df
    }

    MeanAll <- notdup %>% dplyr::arrange(sampleID)
    MeanListInt[[i]] <- MeanAll
  }

  return(MeanListInt)
}
