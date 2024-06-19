#' tidyImages
#'
#' Organize image files exported from an imager platform by creating a raw_images
#' subdirectory in the project directory, moving all image files into it, and
#' removing unwanted files and folders if desired.
#'
#' @param project_dir the full path to the project directory. This folder should
#'   contain images exported from the imaging platform.
#' @param rm_other logical, remove unwanted files and folders after the files
#'    matching the \code{file_ext} are moved to raw_images subfolder? Default is
#'    \code{FALSE}. Set to \code{TRUE} if you are certain you want to remove other
#'    files and folders in the \code{project_dir} after the files matching \code{file_ext}
#'    are moved.
#' @param file_ext the file extension of your raw images. The default is \code{".TIF"},
#'    this argument is case sensitive. This means .tif files will not be found by default.
#' @param profile the profile is matched to the imager. Currently there are two
#'    supported imagers, "moldev" for the Molecular Devices ImageXpress Nano microscope and
#'    "keyence" for the Keyence BZ-X800 microscope. The default is \code{"moldev"}.
#' @return A folder named raw_images under the directory specified with
#'    \code{project_dir}, which contains all image files matching \code{file_ext}
#'    for the project.
#' @importFrom dplyr %>%
#' @export
#'

tidyImages <- function(project_dir, rm_other = FALSE, file_ext = ".TIF", profile = "moldev") {
  # make the raw_images directory
  message(glue::glue("making directory {project_dir}/raw_images"))
  system2(command = "mkdir", args = glue::glue("{project_dir}/raw_images"))

  # move all files matching the file_ext exported from imager to raw_images.
  permission = readline(prompt = glue::glue("Do you want to tidy all {file_ext} files in {project_dir} (y/n):  "))

  if(permission %in% c("Y", "yes", "y") & profile == "moldev") {
    message(glue::glue("moving {file_ext}s to {project_dir}/raw_images"))
    system2(command = "find", args = glue::glue("{project_dir} -type f -name \"*{file_ext}\" -exec mv '{{}}' {project_dir}/raw_images \\;"),
            stdout = FALSE, stderr = FALSE)
    if(rm_other == T) {
      # remove old directories (careful!)
      #get list of all directories not expected to be needed
      dirs <- tibble::tibble(dir = dir(project_dir)) %>%
        dplyr::filter(!(dir %in% c("raw_images", "raw_plate_thumbs", "raw_image_thumbs"))) %>%
        dplyr::mutate(dir_path = glue::glue("{project_dir}/{dir}")) %>%
        dplyr::pull(dir_path)
      message(glue::glue("Removing unwanted files and other directories:"))
      message(paste(dirs, .sep = "\n"))
      unlink(dirs, recursive = T) # delete files listed in dirs
      message(glue::glue("tidyImage DONE"))
    }
    else{
      message(glue::glue("Not removing unwanted files and other directories"))
      message(glue::glue("tidyImage DONE"))
    }
  }
  if(permission %in% c("Y", "yes", "y") & profile == "keyence") {
    message(glue::glue("moving {file_ext}s to {project_dir}/raw_images"))
    # Get plate directory names
    plate_paths <- list.dirs(path = project_dir, recursive = F)

    # Get plate numbers
    plate_numbers <- sub(".*_p(\\d+)$", "\\1", plate_paths)
    plate_numbers2 <- as.numeric(plate_numbers)

    # Check for duplicate or missing plates
    if (any(duplicated(plate_numbers2))) {
      warning(glue::glue("Duplicated plate numbers are detected in the project directory.\nSee plates {paste(plate_numbers2[duplicated(plate_numbers2)], collapse = ', ')}"))
      permission_dup = readline(prompt = glue::glue("Do you want to proceed (y/n):  "))
      if(!permission_dup %in% c("Y", "yes", "y")) {
        stop(glue::glue("QUIT tidyImage - no files moved"))
      }
    } else if (!all(diff(sort(plate_numbers2)) == 1)) {
      warning("The plate numbers do not form a consecutive sequence.")
      permission_cs = readline(prompt = glue::glue("Do you want to proceed (y/n):  "))
      if(!permission_cs %in% c("Y", "yes", "y")) {
        stop(glue::glue("QUIT tidyImage - no files moved"))
      }
    } else {
      message("Great! The plate numbers in the project directory are unique and form a consecutive sequence.")
    }

    # get the full paths to each .lnk file within the project dir
    files <- list.files(path = project_dir, full.names = T, recursive = T, include.dirs = T)
    files_at_depth <- files[stringr::str_count(files, stringr::fixed("/")) == min(stringr::str_count(files, stringr::fixed("/"))) + 1]
    lnk_files <- files_at_depth[grepl(files_at_depth, pattern = "/[^/]{3}\\.lnk$")]

    # setup progress bar
    filelist_pb <- progress::progress_bar$new(total = length(unique(lnk_files)),
                                              format = "Tidying files [:bar] :percent eta: :eta",
                                              clear = FALSE)

    # For each link file, get the path to the actual image file. Move and rename the actual image file within raw_images.
    for(i in unique(lnk_files)) {
      # read it in
      lnk <- R.utils::readWindowsShortcut(i)

      # build data
      well_name <- sub(".*/([^/]+)\\.lnk$", "\\1", i)
      well_id <- sub(x = lnk$relativePath, pattern = "\\.\\\\", replacement = "")
      well_dir <- sub(x = lnk$relativePath, pattern = "\\.\\\\", replacement = "/")
      img_dir <- stringr::str_replace(i, pattern = "/[^/]{3}\\.lnk$", replacement = well_dir)
      orig_file_path <- list.files(path = img_dir, pattern = file_ext, recursive = T, full.names = T)
      orig_file = sub(x = orig_file_path, pattern = ".*/([^/]+)$", replacement = "\\1")
      new_file <- sub(x = orig_file, pattern = paste0("_", well_id, "_.*"), replacement = paste0("_", well_name, file_ext))
      new_file_path <- paste0(project_dir,"/raw_images/", new_file)

      # move them
      fs::file_move(orig_file_path, new_file_path)

      # mark it
      filelist_pb$tick()
    }

    if(rm_other == T) {
      # remove old directories (careful!)
      #get list of all directories not expected to be needed
      dirs <- tibble::tibble(dir = dir(project_dir)) %>%
        dplyr::filter(!(dir %in% c("raw_images", "raw_plate_thumbs", "raw_image_thumbs"))) %>%
        dplyr::mutate(dir_path = glue::glue("{project_dir}/{dir}")) %>%
        dplyr::pull(dir_path)
      message(glue::glue("Removing unwanted files and other directories:"))
      message(paste(dirs, .sep = "\n"))
      unlink(dirs, recursive = T) # delete files listed in dirs
      message(glue::glue("tidyImage DONE"))
    }
    else{
      message(glue::glue("Not removing unwanted files and other directories"))
      message(glue::glue("tidyImage DONE"))
    }
  }
  else{
    message(glue::glue("QUIT tidyImage - no files moved"))
  }
}
