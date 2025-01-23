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

gatherPkgsFromRepo <- function(hashes, targetDir = './', additionalRepoURLs = NULL) {
  download <- function(file, repoURL, targetDir) {
    compressed <- fs::path_ext_set(fs::path(tempdir(), file), 'zip')
    req <- tryCatch({
      curl::curl_fetch_disk(paste0(repoURL, '/', file), compressed)
    }, error = function(e) { list(status_code=404) })
    if(req$status_code != 200)
      return(FALSE)
    unzip(compressed, overwrite=TRUE, exdir=targetDir)
    unlink(compressed)
    TRUE
  }

  repos <- c('https://static.jasp-stats.org/JASP_BINARY_REPO/', 'http://0.0.0.0:8000/', additionalRepoURLs)
  hashesNeeded <- hashes
  for(repo in repos) {
    if(length(hashesNeeded) <= 0) break
    res <- sapply(hashesNeeded, download, repo, targetDir)
    hashesNeeded <- hashesNeeded[!res]
  }

  if(length(hashesNeeded) > 0) {
    print('Couldnt Gather:')
    print(hashesNeeded)
    return(-1)
  }

  return(0)
}