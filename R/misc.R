
#'
#' A list of file names for every output that is created during the analysis.
#'
#' @param output_dir Path to the location where result files are to be stored.
#' @param r_data_out Boolean; Should the read-in Nosa result data, as well as all executed metrics, be saved as an R object?
#'
#'
getOutputFilenames = function(output_dir, r_data_out = FALSE)
{
  out_files = list()
  if (r_data_out)
  {
    out_files = c(out_files, rData = paste0(output_dir, "/result.rds"))
  }
}
