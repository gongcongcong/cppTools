#' Delete files and folders with a specified interval
#'
#' This function deletes files and folders from a specified directory
#' with a specified number and interval.
#'
#' @param folder_path The path to the folder to delete contents from.
#' @param num_to_delete The number of files/folders to delete at each interval.
#' @param interval_sec The interval in seconds between each delete operation.
#' @param show_more show more information of the deleted files.
#' @return None.
#' @export
delete_files <- function(folder_path, num_to_delete, interval_sec, show_more) {
  .Call("R_delete_files", folder_path, num_to_delete, interval_sec, show_more)
  invisible()
}


#' @title Analyze File Types in a Directory
#' @description
#' `file_type_analysis` analyzes the types of files within a specified directory
#' and generates a bar plot showing the percentage distribution of each file type.
#'
#' @param directory Character; the path to the directory to analyze.
#' @return A list (invisibly) containing:
#' \itemize{
#'   \item \code{files_type_freq}: A data frame summarizing file types and their frequency.
#'   \item \code{files_name}: A list of file names grouped by their types.
#' }
#'
#' @examples
#' # Example: Analyze file types in a specified directory
#'
#' file_type_analysis(R.home())
#'
#' @export
file_type_analysis <- function(directory) {
  # Get all files in the directory
  files <- list.files(directory, recursive = TRUE, full.names = TRUE)

  # Extract file extensions
  file_types <- tools::file_ext(files)
  idx <- file_types != ""
  file_types <- file_types[idx]
  files <- split(files[idx], file_types)

  # Count the frequency of each file type
  file_type_counts <- table(file_types)

  # Convert to data frame
  file_type_df <- as.data.frame(file_type_counts)

  # Add percentage column
  file_type_df$Percentage <- (file_type_df$Freq / sum(file_type_df$Freq)) * 100
  file_type_df <- file_type_df[order(file_type_df$Percentage, decreasing = TRUE), ]

  # Plot bar chart
  with(head(file_type_df), {
    barplot(Percentage,
      names.arg = file_types,
      col = rainbow(length(file_types)),
      main = "File Types Distribution",
      xlab = "",
      ylab = "Percentage (%)",
      ylim = c(0, min(100, 1.5 * ceiling(max(Percentage))))
    ) -> midpoints
    text(midpoints,
      Percentage,
      labels = paste0(round(Percentage, 1), "%"),
      pos = 3
    )
  })

  # Return results invisibly
  invisible(list(
    files_type_freq = file_type_df,
    files_name = files
  ))
}
