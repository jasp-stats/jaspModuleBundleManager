generateDirectoryChecksum <- function(path) {
  checkSumSingle <- function(path) {
    openssl::sha256(paste0(openssl::sha256(dir(path, recursive= TRUE)), collapse=''))
  }
  sapply(path, checkSumSingle)
}

createLink <- function(from, to) {
  if (.Platform$OS.type == "windows") { 
    from <- fs::path_abs(fs::path_norm(fs::path(fs::path_dir(to[[1]]), from))) #on windows there are no nice relative symlinks :( so we create abs path
    Sys.junction(from, to)
  }
  else {
    file.symlink(from, to)
  }
}

parseManifest <- function(manifestPath) {
  parse <- function(path) {
    manifest <- rjson::fromJSON(file=path)
    mapping <- stringr::str_split(manifest$mapping, pattern=' => ', simplify=TRUE)
    list(name=manifest$name, from=mapping[,1], to=mapping[,2])
  }
  sapply(manifestPath, parse)
} 
