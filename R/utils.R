getModuleInfo <- function(modulePkg) {
  return(read.dcf(fs::path(modulePkg, "DESCRIPTION"))[1, ])
}

getOS <- function() {
  os <- Sys.info()[['sysname']]
  if(os == 'Darwin')
    os <- 'MacOS'
  return(os)
}

hashDir <- function(path) {
  hash <- function(x) {stringr::str_replace(openssl::sha256(x), ':', '')}
  hashFile <- function(x) {hash(file(x))}
  hashes <- fs::dir_map(path, all = TRUE, recurse = TRUE, type='file', fun = hashFile)
  hash(paste0(hashes, collapse = ""))
}

getRemoteCellarURLs <- function(baseURLs, repoNames) {
  func <- function(a,b) {paste(a,b, 'PKGS', sep='/')}
  outer(baseURLs, repoNames, FUN=func)
}

createL0TarAchive <- function(inputDir, outputPath) { #using TAR in R is so damn annoying
  inputDir <- fs::path_abs(inputDir)
  outputPath <- fs::path_abs(outputPath)
  old_workdir <- setwd(inputDir)
  on.exit(setwd(old_workdir)) #if error
  tar(outputPath, compression='gzip', tar='internal', compression_level=9)
  setwd(old_workdir)
}

extractL0TarAchive <- function(tarfile, exdir) { #using TAR in R is so damn annoying
  exdir <- fs::path_abs(exdir)
  tarfile <- fs::path_abs(tarfile)
  fs::dir_create(exdir)
  old_workdir <- setwd(exdir)
  on.exit(setwd(old_workdir)) #if error
  untar(tarfile, tar='internal')
  setwd(old_workdir)
}

createLink <- function(from, to, forceSymlink=FALSE) {
  fs::link_delete(to[fs::link_exists(to)])
  if (.Platform$OS.type == "windows") {
    from <- fs::path_abs(fs::path_norm(fs::path(fs::path_dir(to[[1]]), from))) #on windows there are no nice relative symlinks :( so we create abs path
    if(forceSymlink)
      file.symlink(from, to)
    else
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
    manifest$pkgs <- mapping[,2]
    manifest$from <- mapping[,1]
    manifest$to   <- stringr::str_split(mapping[,2], pattern='_', simplify=TRUE)[,1]
    manifest
  }
  sapply(manifestPath, parse)
}

gatherPkgsFromRepo <- function(hashes, targetDir = './', repoNames = c('development'), additionalRepoURLs = NULL) {
  download <- function(file, repoURL, targetDir) {
    compressed <- fs::path(tempdir(), file)
    on.exit(if(fs::dir_exists(compressed)) fs::dir_delete(compressed))
    req <- tryCatch({
      curl::curl_fetch_disk(paste0(repoURL, '/', file), compressed)
    }, error = function(e) { list(status_code=404) })
    if(req$status_code != 200)
      return(FALSE)
    if(!fs::dir_exists(fs::path(targetDir, file)))
      extractL0TarAchive(compressed, fs::path(targetDir, file))
    if(hashDir(fs::path(targetDir, file)) !=  file) 
      stop(paste0("Hash mismatch for remote cellar file: ", file))
      
    TRUE
  }

  repos <- getRemoteCellarURLs(c('https://repo.jasp-stats.org', additionalRepoURLs), repoNames)
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
