#' Plot boxplots of gene quantities
#'
#' This function generates boxplots (and jitter) for either mean quantities or ratio quantities
#' across genes. It also saves the plot as a TIFF file.
#'
#' @param data A data.frame with sampleID and gene quantities.
#' @param colors Character vector. Colors to use for the gene fill.
#' @param genes Character vector. Names of the genes to include in the plot.
#' @param ylimit Numeric vector of length 2. Y-axis limits.
#' @param MeanQuantity Logical. If TRUE, plots mean quantity per gene.
#' @param RatioQuantity Logical. If TRUE, plots ratio quantity per gene.
#' @param dir.name Character. Path to the main output directory.
#'
#' @return A ggplot2 object with the boxplot.
#' @importFrom dplyr filter select mutate matches
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_jitter scale_y_continuous labs theme element_blank element_text scale_fill_manual
#' @importFrom grDevices tiff dev.off
#' @export
plotBar <- function(data,
                    colors,
                    genes,
                    ylimit,
                    MeanQuantity = FALSE,
                    RatioQuantity = FALSE,
                    dir.name = ".") {

  stopifnot(MeanQuantity || RatioQuantity) # at least one must be TRUE

  # Check that number of colors matches number of genes
  if (length(colors) < length(genes)) {
    stop("You provided fewer colors (", length(colors),
         ") than genes to plot (", length(genes), "). Please provide at least as many colors as genes.")
  }

  # Create output folder if needed
  figures_path <- file.path(dir.name, "5.Figures")
  if (!dir.exists(figures_path)) dir.create(figures_path, recursive = TRUE)

  # Preprocess for MeanQuantity
  if (MeanQuantity) {

    Qty_Long <- data %>%
      dplyr::select(sampleID = dplyr::matches("sampleID"),
                    dplyr::matches("Quantity"),
                    -dplyr::matches(tolower("bactin"))) %>%
      tidyr::pivot_longer(
        cols = -sampleID,
        names_to = "Gene",
        values_to = "Quantity"
      ) %>%
      dplyr::mutate(
        Gene = gsub("_Quantity_Mean", "", Gene)
      ) %>%
      dplyr::filter(!is.na(Quantity)) %>%
      dplyr::filter(tolower(Gene) %in% tolower(genes))

    ylabel <- "Mean Quantity"
  }

  # Preprocess for RatioQuantity
  if (RatioQuantity) {

    Qty_Long <- data %>%
      dplyr::select(sampleID = dplyr::matches("sampleID"),
                    dplyr::matches("Quantity")) %>%
      tidyr::pivot_longer(
        cols = -sampleID,
        names_to = "Gene",
        values_to = "Quantity"
      ) %>%
      dplyr::mutate(
        Gene = gsub("/Bactin_Quantity_Mean", "", Gene)
      ) %>%
      dplyr::filter(!is.na(Quantity)) %>%
      dplyr::mutate(Quantity = log2(Quantity)) %>%
      dplyr::filter(tolower(Gene) %in% tolower(genes))

    ylabel <- "log2(Ratio Quantity)"
  }

  # Plot
  QuantityPlot <- ggplot2::ggplot(Qty_Long, ggplot2::aes(x = Gene, y = Quantity, fill = Gene)) +
    ggplot2::geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 1, notch = FALSE) +
    ggplot2::geom_jitter(width = 0.1) +
    ggplot2::scale_y_continuous(limits = ylimit) +
    ggplot2::labs(title = "", x = "", y = ylabel) +
    ggplot2::theme(
      legend.position = "right",
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks.x = ggplot2::element_line()
    ) +
    ggplot2::scale_fill_manual(values = colors[seq_along(genes)])

  # Save plot
  tiff_path <- file.path(figures_path, "QuantityMean_BoxPlot.tiff")
  grDevices::tiff(tiff_path, width = 1200, height = 1200, units = "px", res = 200)
  print(QuantityPlot)
  grDevices::dev.off()

  message("Plot saved to: ", tiff_path)

  return(QuantityPlot)
  #return(Qty_Long)
}
