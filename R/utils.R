generateDirectoryChecksum <- function(path) {
  checkSumSingle <- function(path) {
    openssl::sha256(paste0(openssl::sha256(dir(path, recursive= TRUE)), collapse=''))
  }
  sapply(path, checkSumSingle)
}

createLink <- function(from, to) {
  if (.Platform$OS.type == "windows") { 
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