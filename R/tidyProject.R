#' tidyProject
#'
#' Tidies up image files exported from imaging platform and makes thumbnails of
#' wells and plates.
#'
#' @param project_dir the full path to the project directory. This folder should
#'   contain images exported from the imaging platform.
#' @param rm_other logical, remove unwanted files and folders after the image
#'   files are moved to the raw_images subfolder? Default is \code{FALSE}. Set to
#'   \code{TRUE} if you are certain you want to remove unwanted files and folders
#'   in the \code{project_dir} after the desired images are moved.
#' @param plates a vector with plate numbers to make thumbs for. This vector is
#'   used to match plate patterns in the filelist.\cr \code{"all"} will make
#'   thumbs for all plates.\cr \code{13:72} will make thumbs for wells plates
#'   p013 - p072.\cr \code{c(1, 3, 106)} will make thumbs for plates p001, p003, and
#'   p106.
#' @param max_dim The maximum dimension of the resized images in pixels. The
#'   default value is 512, which scales a 2048 pixel image to 6.25 percent of
#'   its original resolution.
#' @param file_ext the file extension of your raw images. The default is \code{".TIF"},
#'    this argument is case sensitive. This means .tif files will not be found by default.
#' @param profile the profile is matched to the imager. Currently there are two
#'    supported imagers, "moldev" for the Molecular Devices ImageXpress Nano microscope and
#'    "keyence" for the Keyence BZ-X800 microscope. The default is \code{"moldev"}.
#' @return Folders named raw_images, raw_image_thumbs, and raw_plate_thumbs are
#'   created under the directory specified by \code{project_dir}. These folders
#'   match the directory structure needed to process the images with
#'   \href{https://github.com/AndersenLab/CellProfiler}{AndersenLab/CellProfiler}.
#' @export
#'

tidyProject <- function(project_dir, rm_other = FALSE, plates = "all", max_dim = 512, file_ext = ".TIF", profile = "moldev") {
  # tidy the images
  easyXpress::tidyImages(project_dir, rm_other, file_ext, profile)

  # make well thumbs
  easyXpress::wellThumbs(project_dir, plates, max_dim, file_ext)

  # make plate thumbs
  easyXpress::plateThumbs(project_dir, plates)
}
