getModuleInfo <- function(modulePkg) {
  return(read.dcf(fs::path(modulePkg, "DESCRIPTION"))[1, ])
}

getOS <- function() {
  os <- Sys.info()[['sysname']]
  if(os == 'Darwin')
    os <- 'MacOS'
  if(Sys.getenv('FLATPAK_ID') != "")
    os <- 'Flatpak'
  return(os)
}

hashDir <- function(path) {
  hash <- function(x) {stringr::str_replace(openssl::sha256(x), ':', '')}
  hashFile <- function(x) {hash(file(x))}
  hashes <- fs::dir_map(path, all = TRUE, recurse = TRUE, type='file', fun = hashFile)
  hash(paste0(hashes, collapse = ""))
}

getRemoteCellarURLs <- function(baseURLs, repoNames) {
  if(length(repoNames) == 0) return(c())
  func <- function(a,b) {paste(a,b, 'PKGS', sep='/')}
  outer(baseURLs, repoNames, FUN=func)
}

createL0TarAchive <- function(inputDir, outputPath) {
  inputDir <- fs::path_abs(inputDir)
  outputPath <- fs::path_abs(outputPath)

  if (.Platform$OS.type != "windows") {
    archive::archive_write_dir(outputPath, inputDir, format = "tar", filter = "zstd")
  }
  else { #on windows archive pkg seems to just be broken beyond compressing text files. Any binary ops tweak out archive :'(
    old_workdir <- setwd(inputDir)
    on.exit(setwd(old_workdir)) #if error
    tar(outputPath, compression='zstd', tar='internal', compression_level=3)
    setwd(old_workdir)
  }
}

extractL0TarAchive <- function(tarfile, exdir) {
  exdir <- fs::path_abs(exdir)
  tarfile <- fs::path_abs(tarfile)
  archive::archive_extract(tarfile, exdir)
}

nestBinaryPkgIfNeeded <- function(hashDir, pkgName) {
  #Turn a flat binary_pkgs/<hash> (the package at its root, the old layout) into a micro-library
  #binary_pkgs/<hash>/<pkgname>/, so that the hash dir itself is a valid R library path. Idempotent:
  #already-nested and missing dirs are left alone. Windows-only concern, but safe anywhere.
  if(!fs::file_exists(fs::path(hashDir, 'DESCRIPTION')))
    return(invisible(FALSE))

  staging <- fs::path(fs::path_dir(hashDir), paste0(fs::path_file(hashDir), '_nesting'))
  fs::dir_create(staging)
  on.exit(if(fs::dir_exists(staging)) fs::dir_delete(staging), add = TRUE)
  fs::file_move(hashDir, fs::path(staging, pkgName)) #fs has no dir_move; file_move moves dirs too
  fs::file_move(staging, hashDir)
  invisible(TRUE)
}

#Packages that start with "jasp" but are shared infrastructure rather than modules. They carry no QML
#of their own, so they never need a name-keyed entry inside another module's module_libs dir.
jaspInfraPkgs <- c('jaspBase', 'jaspGraphs', 'jaspTools', 'jaspResults', 'jaspWorkarounds')

isJaspModulePkg <- function(pkgName) {
  startsWith(pkgName, 'jasp') & !(pkgName %in% jaspInfraPkgs) #vectorized: pkgName is manifest$to
}

createWindowsModuleLibEntry <- function(installPath, manifest) {
  #Windows module_libs entries contain real directory copies instead of links. Plain R deps are
  #served straight from their binary_pkgs micro-libraries through .libPaths() (JASP derives those
  #from the manifest in AppDirs::moduleExtraLibPaths), so only packages that must exist under
  #their own name inside this dir are copied: the module package itself and every dependency that
  #is itself a JASP module — modules import each other's QML through relative paths that resolve
  #positionally through the importer's entry dir. A few MB per cross-module edge (~34 in total)
  #buys us the end of the appData junction farm (jasp-issues #4586).
  entryPath <- fs::path(installPath, 'module_libs', manifest$name)
  if(fs::dir_exists(entryPath)) #may hold stale entries from an interrupted or pre-copy-rule install
    fs::dir_delete(entryPath)
  fs::dir_create(entryPath)

  needsEntry <- manifest$to == manifest$name | isJaspModulePkg(manifest$to)
  copyPkg <- function(hash, pkg) {
    from <- fs::path(installPath, 'binary_pkgs', hash, pkg)
    if(!fs::dir_exists(from)) {
      warning(paste0('Missing micro-library binary_pkgs/', hash, '/', pkg, ', not creating a module_libs entry for it'))
      return(invisible(FALSE))
    }
    fs::dir_copy(from, fs::path(entryPath, pkg))
    invisible(TRUE)
  }
  invisible(mapply(copyPkg, manifest$from[needsEntry], manifest$to[needsEntry]))
  entryPath
}

nestAndHealSharedHashes <- function(binaryPkgsPath, modulesLibPaths, manifestPath, ownManifestFile, manifest) {
  #Nest every hash of `manifest` (see nestBinaryPkgIfNeeded) and, when a hash that was still FLAT
  #is also referenced by another installed manifest, re-point that module's module_libs junction
  #one level deeper. Without this, nesting moves the package out from under the older (junction
  #layout) module's entry: R loading self-heals through the micro-library, but the name-keyed
  #entries — needed for cross-module QML imports — would dangle. A junction is the entry's native
  #idiom, is created instantly and dedups perfectly; if creation is blocked (AV & friends,
  #jasp-issues#4586) we fall back to a real dir copy. Healthy real-copy entries are left alone.
  otherManifests <- setdiff(fs::dir_ls(manifestPath, type = 'file', glob = '*_manifest.json'), ownManifestFile)
  others <- if (length(otherManifests) > 0) parseManifest(otherManifests) else list()

  nestOne <- function(hash, pkg) {
    hashDir <- fs::path(binaryPkgsPath, hash)
    wasFlat <- fs::file_exists(fs::path(hashDir, 'DESCRIPTION'))
    nestBinaryPkgIfNeeded(hashDir, pkg)

    if (wasFlat && length(others) > 0)
      for (m in others) {
        i <- match(hash, m$from)
        if (is.na(i)) next
        target <- fs::path(hashDir, m$to[i])
        link <- fs::path(modulesLibPaths, m$name, m$to[i])
        if (fs::dir_exists(link) && !fs::link_exists(link))
          next #a healthy real copy already serves this entry
        tryCatch({
          if (fs::link_exists(link))
            fs::link_delete(link)
          Sys.junction(target, link)
        }, error = function(e) {
          warning(paste0('Could not re-point junction ', link, ' (', conditionMessage(e), '), falling back to a real copy'))
          fs::dir_copy(target, link)
        })
      }
    invisible(TRUE)
  }
  invisible(mapply(nestOne, manifest$from, manifest$to))
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
  lapply(manifestPath, parse)
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
    res <- unlist(lapply(hashesNeeded, download, repo, targetDir))
    hashesNeeded <- hashesNeeded[!res]
  }

  if(length(hashesNeeded) > 0) {
    print('Couldnt Gather:')
    print(hashesNeeded)
    return(-1)
  }

  return(0)
}


getUnavailableHashes <- function(hashes, repoNames = c('development'), additionalRepoURLs = NULL) {
  check <- function(file, repoURL, targetDir) {
    h <- curl::new_handle()
    curl::handle_setopt(h, customrequest = "PUT")
    req <- tryCatch({
      curl::curl_fetch_memory(paste0(repoURL, '/', file), handle=h)
    }, error = function(e) { list(status_code=404) })
    req$status_code == 405
  }

  repos <- getRemoteCellarURLs(c('https://repo.jasp-stats.org', additionalRepoURLs), repoNames)
  hashesNeeded <- hashes
  for(repo in repos) {
    if(length(hashesNeeded) <= 0) break
    res <- unlist(lapply(hashesNeeded, check, repo, targetDir))
    hashesNeeded <- hashesNeeded[!res]
  }
  hashesNeeded
}
