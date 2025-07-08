#' Check and remove NA/unknown samples from qPCR data
#'
#' This function checks a list of data.frames for missing or invalid qPCR values,
#' removes samples with NA or invalid `CT`/`Quantity`, and returns a list of cleaned data.
#'
#' @param data A list of data.frames to process.
#' @param CT.name Character vector of length 1 or 2. Column name(s) for CT values.
#' @param Quantity.name Character. Column name for Quantity.
#' @param Well.name Character vector of length 1 or 2. Column name(s) for Well position.
#' @param sampleID Character. Column name for the sample identifier.
#' @param Detector.name Character vector of length 1 or 2. Column name(s) for the gene detector.
#' @param dir.name Character. Path to the main output directory.
#'
#' @return A list of cleaned data.frames, and writes a CSV with removed samples.\
#' @importFrom utils write.table
#' @export
check_NA <- function(data,
                     CT.name,
                     Quantity.name,
                     Well.name,
                     sampleID,
                     Detector.name,
                     dir.name) {

  RemovedSamples <- data.frame()
  ConservedList <- list()

  for (i in seq_along(data)) {

    df <- data[[i]]

    # Helper: pick first available column name from a vector of candidates
    pick_col <- function(candidates, df) {
      for (c in candidates) {
        if (c %in% colnames(df)) return(df[[c]])
      }
      stop("None of the specified columns found: ", paste(candidates, collapse = ", "))
    }

    # Extract required columns
    well.def     <- pick_col(Well.name, df)
    detector.def <- pick_col(Detector.name, df)
    CT.def       <- pick_col(CT.name, df)

    clean.data <- data.frame(
      WellPosition = well.def,
      sampleID = df[[sampleID]],
      Gene = detector.def,
      Task = df$Task,
      CT = suppressWarnings(as.numeric(CT.def)),
      Quantity = suppressWarnings(as.numeric(df[[Quantity.name]]))
    )

    # Keep rows with at least one of CT or Quantity not NA, and Task is "unknown"
    nonas.data <- clean.data[
      (!is.na(clean.data$CT) | !is.na(clean.data$Quantity)) &
        tolower(clean.data$Task) == "unknown",
    ]

    # Identify removed samples: those not in nonas
    removed <- clean.data[!clean.data$sampleID %in% nonas.data$sampleID, ]

    if (nrow(removed) > 0) {
      RemovedSamples <- rbind(RemovedSamples, removed)
    }

    if (nrow(nonas.data) > 0) {
      ConservedList[[i]] <- nonas.data
    }
  }

  # Write removed samples to CSV
  output_basename <- paste0(basename(normalizePath(dir.name)), "_Removed_Samples.csv")
  output_file <- file.path(tempdir(), output_basename)  # lo escribe primero en tmp

  utils::write.table(RemovedSamples, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE)

  # mover al directorio destino
  dest_dir <- file.path(dir.name, "4.Results")
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  dest_file <- file.path(dest_dir, output_basename)

  if (file.exists(output_file)) {
    file.copy(output_file, dest_file, overwrite = TRUE)
    message("Removed samples results saved to ", dest_file)
    file.remove(output_file)
  }

  return(ConservedList)
}
