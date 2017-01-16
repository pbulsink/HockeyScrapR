# helper for scrape roster testing
try_delete <- function(f) {
  if (file.exists(f)) {
    tryCatch({
      file.remove(f)
    }, error = function(e) message(paste0("Error deleting file ", f, ", Continuing...")))
  }
}
