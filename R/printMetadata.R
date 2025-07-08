#' Generate and export summarized metadata tables
#'
#' This function generates metadata tables in three modes:
#' - MeanData: merges mean data of all genes.
#' - RatioData: merges ratio data of all genes relative to control.
#' - Consensus: combines mean and ratio data into a single table and writes it to a CSV.
#'
#' @param data A list of data.frames, each representing a gene or control.
#' @param MeanData Logical. If TRUE, returns merged mean data.
#' @param RatioData Logical. If TRUE, returns merged ratio data.
#' @param consensus Logical. If TRUE, combines mean and ratio data and writes CSV.
#' @param data.mean Data.frame with mean data (used if consensus = TRUE).
#' @param data.ratio Data.frame with ratio data (used if consensus = TRUE).
#' @param genes Character vector with gene names (used if consensus = TRUE).
#' @param dir.name Character. Path to the main output directory.
#'
#' @return A data.frame with the requested table, or writes CSV in consensus mode.
#' @importFrom dplyr rename_with select matches relocate all_of
#' @importFrom utils write.table
#' @export
printMetadata <- function(data,
                          dir.name,
                          MeanData = FALSE,
                          RatioData = FALSE,
                          consensus = FALSE,
                          data.mean = NULL,
                          data.ratio = NULL,
                          genes = NULL) {

  if (MeanData) {
    # Start with control
    MeanControl <- data[[1]] %>%
      dplyr::rename_with(~paste0(data[[1]]$Gene[1], '_CT_Mean'), .cols = CT) %>%
      dplyr::rename_with(~paste0(data[[1]]$Gene[1], '_Quantity_Mean'), .cols = Quantity) %>%
      dplyr::select(-dplyr::matches("Well"), -dplyr::matches("Task"), -dplyr::matches("Gene"))

    # Loop over genes
    for (i in 2:length(data)) {
      geneName <- unique(data[[i]]$Gene)
      MeanControl <- merge(MeanControl, data[[i]], by = 'sampleID', all = TRUE) %>%
        dplyr::rename_with(~paste0(geneName, '_CT_Mean'), .cols = CT) %>%
        dplyr::rename_with(~paste0(geneName, '_Quantity_Mean'), .cols = Quantity) %>%
        dplyr::select(-dplyr::matches("Well"), -dplyr::matches("Task"), -dplyr::matches("Gene"))
    }

    return(MeanControl)
  }

  if (RatioData) {
    RatioList <- list()

    for (i in 2:length(data)) {
      geneName <- unique(na.omit(data[[i]]$Gene))
      data.modified <- data[[i]] %>%
        dplyr::rename_with(~paste0(geneName, '/Bactin_CT_Mean'), .cols = CT_MeanRatio) %>%
        dplyr::rename_with(~paste0(geneName, '/Bactin_Quantity_Mean'), .cols = QuantityRatio) %>%
        dplyr::select(-dplyr::matches("Control"), -dplyr::matches("Gene"))
      RatioList[[i]] <- data.modified
    }

    mergedRatios <- RatioList[[2]]
    for (i in 3:length(data)) {
      mergedRatios <- merge(mergedRatios, RatioList[[i]], by = 'sampleID', all.x = TRUE)
    }

    return(mergedRatios)
  }

  if (consensus) {
    stopifnot(!is.null(data.mean), !is.null(data.ratio), !is.null(genes))

    ConsensusData <- merge(data.mean, data.ratio, by = 'sampleID', all.x = TRUE)

    # hacer coincidir nombres en minúsculas
    colnames_lower <- tolower(colnames(ConsensusData))
    genes_lower <- tolower(genes)

    for (gene in genes_lower) {
      # buscar las columnas que empiezan con ese gen (en minúsculas)
      JustMeans <- colnames(ConsensusData)[
        grepl(paste0("^", gene), colnames_lower)
      ]

      if (length(JustMeans) > 0) {
        ConsensusData <- ConsensusData %>%
          dplyr::relocate(all_of(JustMeans), .after = dplyr::matches("sampleID"))
      }
    }

    # write file
    output_basename <- paste0(basename(normalizePath(dir.name)), "_qMSP_results.csv")
    output_file <- file.path(tempdir(), output_basename)  # lo escribe primero en tmp

    utils::write.table(ConsensusData, file = output_file, sep = ",", row.names = FALSE, col.names = TRUE)

    # mover al directorio destino
    dest_dir <- file.path(dir.name, "4.Results")
    if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

    dest_file <- file.path(dest_dir, output_basename)

    if (file.exists(output_file)) {
      file.copy(output_file, dest_file, overwrite = TRUE)
      message("Consensus results saved to ", dest_file)
      file.remove(output_file)
    }
    return(ConsensusData)
  }
}
