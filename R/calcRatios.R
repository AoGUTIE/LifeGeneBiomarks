#' Calculate ratios between control and genes
#'
#' This function calculates the CT and Quantity ratios (%) of each gene
#' relative to the control (assumed to be the first element in `data`).
#'
#' @param data A list of data.frames. The first element is the control, others are genes.
#'             Each data.frame must have columns: `sampleID`, `Gene`, `CT`, `Quantity`.
#'
#' @return A list of data.frames, each containing the ratios for one gene.
#' @export
calcRatios <- function(data) {

  RatioListInt <- vector("list", length(data) - 1)  # ya que el primero es control

  control <- data[[1]]

  for (i in 2:length(data)) {

    gene <- data[[i]]

    # merge control & gene by sampleID
    matched <- merge(
      control, gene, by = "sampleID", all.x = TRUE, suffixes = c(".control", ".gene")
    )

    # calculate ratios
    calc.ratio <- data.frame(
      sampleID       = matched$sampleID,
      Control        = matched$Gene.control,
      Gene           = matched$Gene.gene,
      CT_MeanRatio   = (matched$CT.gene / matched$CT.control) * 100,
      QuantityRatio  = (matched$Quantity.gene / matched$Quantity.control) * 100
    )

    RatioListInt[[i]] <- calc.ratio
  }

  return(RatioListInt)
}
