#' @title Load MATLAB structs as R data frames
#'
#' @description A simple wrapper for readMat for structs that additionally
#'   converts the resulting list into a data frame.
#'
#' @details \code{struct2dataframe} is basically just a replacement for a few
#'   lines of code, but saves some time and ensures that the struct you clearly
#'   meant as a data frame for processing or plotting in R actually shows up as
#'   a data frame.
#'
#' @param fn A full (character) filename path pointing to the desired .mat file
#'   created by save(). The file must be saved in v6 or before (since R.matlab
#'   can't deal with HDF5-like files yet).
#' @param var A character variable name, if a specific variable is desired from
#'   the .mat file. If no variable is specified, \code{struct2datable} will attempt
#'   to process the first variable listed.
#'
#' @return A data frame will be returned with the struct fields as variables and
#'   the struct elements as observations
#'
#' @examples
#' \dontrun{struct2dataframe("~/code/some_struct_v6.mat")}
#'
#' @export

struct2dataframe <-
  function(fn,var=c()) {
    # Load matlab file
    mat.data <- readMat(fn)

    # Choose which variable to process
    if (length(mat.data)>1) {
      if (length(var)==0) {
        warning(paste0("File has ",length(mat.data),
                       " variables. Since no desired variable is specified, ",
                       "the first variable ('",names(mat.data)[[1]],"') will be processed."))
        mat.data <- mat.data[[names(mat.data)[[1]]]]
      } else {
        mat.data <- mat.data[[var]]
      }
    } else {
      # Isolate that one variable
      mat.data <- mat.data[[1]]
    }

    # Transform into an actually usable data frame
    mat.data <- as.data.frame(t(as.data.frame(mat.data)))

    # Get which are numeric
    num.idxs <- unlist(lapply(mat.data[1,],function(x) class(unlist(x))=="numeric"))

    # Unlist created data frame elements
    mat.data <- as.data.frame(sapply(mat.data,unlist))

    # Switch back to numerical those switched to character through unlist above
    mat.data[,num.idxs] <- sapply(mat.data[,num.idxs],function(x) as.numeric(as.character(x)))

    # SHOULD ADD SOMETHING ON FACTOR CHARACTERS, A STRINGS.AS.FACTORS THING. OR SPECIFY WHICH ARE SUPPOSED TO BE FACTORS.


    return(mat.data)
}



