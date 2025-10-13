#' Load and organize data files in a project
#'
#' This function creates a directory with subfolders, moves files matching a pattern,
#' and loads `.xls` or `.csv` files into a list of data.frames.
#'
#' @param skipping.rows Integer. Number of rows to skip when reading the files.
#' @param separator Character. Separator for CSV files (default `","`).
#' @param filePattern Character. Regex pattern to search for data files.
#' @param xlsx Logical. Whether to read `.xlsx` files.
#' @param csv Logical. Whether to read `.csv` files.
#' @param dir.name Character. Name of the main directory to create.
#'
#' @return A list of data.frames with the imported data, named after the files.
#' @importFrom readxl read_excel
#' @importFrom utils read.csv
#' @export
loadData <- function(skipping.rows = 0,
                     separator = ",",
                     filePattern,
                     fileFolder = ".",
                     xlsx = FALSE,
                     csv = FALSE,
                     dir.name) {

  if (!any(xlsx, csv)) stop("You must specify at least one format: xlsx=TRUE or csv=TRUE.")
  if (!nzchar(filePattern)) stop("You must specify a file pattern.")
  if (dir.exists(dir.name)) {
    warning("The directory already exists. Files can be overwritten.")
  } else {
    dir.create(dir.name, recursive = TRUE)
  }

  message("Creating subfolders...")
  subfolders <- c("1.Code", "2.Data", "3.Documents", "4.Results", "5.Figures", "6.Tables")
  subfolder.paths <- file.path(dir.name, subfolders)
  lapply(subfolder.paths, dir.create, showWarnings = FALSE)

  data.folder <- file.path(dir.name, "2.Data")
  code.folder <- file.path(dir.name, "1.Code")

  message("Moving data files and Rmd...")
  data.files <- list.files(fileFolder, pattern = filePattern, full.names = TRUE)
  rmd.files  <- list.files(".", pattern = "\\.Rmd$")
  file.copy(data.files, data.folder)
  file.copy(rmd.files, code.folder)

  # Listar archivos a cargar
  list.files.data <- list.files(data.folder, pattern = filePattern, full.names = FALSE)
  if (length(list.files.data) == 0) stop("No files with the specified pattern were found.")

  message("Importing data...")
  imported_data <- list()

  for (i in seq_along(list.files.data)) {
    file.path.i <- file.path(data.folder, list.files.data[i])

    if (xlsx && grepl("\\.xls[x]?$", list.files.data[i], ignore.case = TRUE)) {
      data.sample <- readxl::read_excel(file.path.i, skip = skipping.rows)
    } else if (csv && grepl("\\.csv$", list.files.data[i], ignore.case = TRUE)) {
      data.sample <- utils::read.csv(file.path.i, header = TRUE, skip = skipping.rows, sep = separator)
    } else {
      message(sprintf("Ignored file (not .xls or .csv): %s", list.files.data[i]))
      next
    }

    imported_data[[i]] <- data.sample
  }

  names(imported_data) <- list.files.data
  message("Data imported correctly.")

  return(imported_data)
}
