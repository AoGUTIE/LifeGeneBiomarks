#' Concatenate rows by gene of interest
#'
#' This function filters and concatenates rows from a list of data.frames
#' for each specified gene of interest.
#'
#' @param data A list of data.frames, each containing at least a `Gene` column.
#' @param genes A character vector of gene names to filter by.
#'
#' @return A list of data.frames, each corresponding to one gene in `genes`.
#' @export
geneConcat <- function(data,
                       genes,
                       separate = "FALSE",
                       identificator) {

  GeneListInt <- vector("list", length(genes))
  names(GeneListInt) <- genes

  for (j in seq_along(genes)) {

    gene <- tolower(genes[j])
    gene_files <- data.frame()

    for (k in seq_along(data)) {

      df <- data[[k]]

      # Skip if NULL or empty
      if (!is.null(df) && nrow(df) > 0) {

        # Filter rows where Gene matches (case-insensitive)
        gene_subset <- df[tolower(df$Gene) == gene, ]

        # Concatenate if not empty
        if (nrow(gene_subset) > 0) {
          gene_files <- rbind(gene_files, gene_subset)
        }
      }
    }

    if (separate) {

      # Create a logic vector with all rows that match your identificator
      matches <- grepl(paste0("[", identificator, "]$"), gene_files$sampleID)
      # Filter the data according to matches vector.
      GeneListInt[[j]] <- gene_files[matches, ]

    } else {

      GeneListInt[[j]] <- gene_files
    }
  }

  return(GeneListInt)
}
