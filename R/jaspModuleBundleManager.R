#' Install Module Bundle
#'
#' @description Install Module Bundle into the relevant dirs. See JaspModuleManagement.pdf for an overview
#' @param installPath Path to the root of the module install folder
#' @param bundlePath Path to bundle to install
#' @export
installJaspModuleBundle <- function(installPath, bundlePath, repoNames=c('development'), additionalRepos=NULL) {
  binaryPkgsPath <- fs::path(installPath, 'binary_pkgs')
  modulesLibPaths <- fs::path(installPath, 'module_libs')
  manifestPath <- fs::path(installPath, 'manifests')

  #create the necessary directories in installPath if they dont exist
  fs::dir_create(binaryPkgsPath)
  fs::dir_create(modulesLibPaths)
  fs::dir_create(manifestPath)

  #copy all binary pkgs we do not yet have.
  stagingDir <- fs::dir_create(tempdir(), 'bundleManagerInstall')
  untarBundleDir <- fs::dir_create(stagingDir, 'bundleUntar')
  on.exit(if(fs::dir_exists(stagingDir)) fs::dir_delete(stagingDir))
  extractL0TarAchive(bundlePath, untarBundleDir)

  pkgs <- fs::dir_ls(untarBundleDir, type='file', glob='*_manifest.json', invert=TRUE)
  untarPkgtoBinDir <- function(pkg) {
    hash <- fs::path_file(pkg)
    if(!fs::dir_exists(fs::path(binaryPkgsPath, hash))){
      extractL0TarAchive(pkg, fs::path(binaryPkgsPath, hash))
    }
  }
  sapply(pkgs, untarPkgtoBinDir)

  #Move manifest to manifest folder. But first check if a different version is already installed if so uninstall
  manifest <- fs::dir_ls(untarBundleDir, type='file', glob='*_manifest.json')
  manifestDestinationPath <- fs::path(manifestPath, fs::path_file(manifest[[1]]))
  if(fs::file_exists(manifestDestinationPath)) uninstallJaspModuleBundleByManifest(installPath, manifestDestinationPath, newManifest=manifest[[1]]) #If this module is already present delete it
  manifestFile <- fs::file_copy(manifest[[1]], manifestDestinationPath, overwrite=TRUE)
  fs::file_delete(manifest)
  manifest <- parseManifest(manifestFile)[, 1]

  #download and extract any missing pkgs that were not included in the bundle from the online repo
  if(!manifest$complete == TRUE)
    repairJaspModuleBundleByManifest(installPath, manifestFile, repoNames)

  #create moduleLib entry (folder with symlinks to actual pkgs) from manifest mapping
  entryPath <- fs::path(modulesLibPaths, manifest$name)
  fs::dir_create(entryPath)
  from <- fs::path(fs::path_rel(binaryPkgsPath, start=entryPath), manifest$from)
  to <- fs::path(entryPath, manifest$to)
  createLink(from, to)
}

# Todo expand with version check
#' @export
checkIfJaspModuleBundleInstalled <- function(installPath, name, version = NULL) {
  manifests <- fs::dir_ls(fs::path(installPath, 'manifests'), type='file', regexp=name)
  length(manifests) > 0
}

#' @export
uninstallJaspModuleBundle <- function(installPath, name) {
  manifests <- fs::dir_ls(fs::path(installPath, 'manifests'), type='file', regexp=name)
  uninstallJaspModuleBundleByManifest(installPath, manifests)
}

#remove the binarys related to oldManifst, that are unused in the other manifest in manifest folder or newManifest
#remove the library folder entry and the manifest
#' @export
uninstallJaspModuleBundleByManifest <- function(installPath, oldManifest, newManifest=NULL) {
  #figure out which binaries to delete and delete them
  manifests <- fs::dir_ls(fs::path(installPath, 'manifests'), type='file', glob='*_manifest.json')
  manifests <- manifests[fs::path_file(manifests) != fs::path_file(oldManifest)]
  manifests <- c(manifests, newManifest)
  manifests <- parseManifest(manifests)

  keep <- list()
  if(length(manifests) > 0)
    keep <- unique(unlist(manifests['from',]))

  rmManifest <- parseManifest(oldManifest)
  rm <- unique(unlist(rmManifest['from',]))
  rm <- rm[!(rm %in% keep)]
  rmPaths <- fs::path(installPath, 'binary_pkgs', rm)
  fs::dir_delete(rmPaths)

  #delete lib entry
  entry <- fs::path(installPath, 'module_libs', rmManifest['name', ])
  if(fs::dir_exists(entry))
    fs::dir_delete(entry)

  #delete manifest
  fs::file_delete(oldManifest)

}


#' @export
repairJaspModuleBundle <- function(installPath, name, repoNames=c('development'), additionalRepos=NULL) {
  manifest <- fs::dir_ls(fs::path(installPath, 'manifests'), type='file', regexp=name)[[1]]
  repairJaspModuleBundleByManifest(installPath, manifest, repoNames, additionalRepos)
}


#' @export
repairJaspModuleBundleByManifest <- function(installPath, manifest, repoNames=c('development'), additionalRepos=NULL) {
  #get the needed pkg hashes from the manifest, subtract those we already have in the binary_pkg folder
  binaryPkgsPath <- fs::path(installPath, 'binary_pkgs')
  manifest <- parseManifest(manifest)[, 1]
  hashesNeeded <- manifest$from
  hashesPresent <- fs::path_file(fs::dir_ls(binaryPkgsPath, type='directory'))
  hashesNeeded <- hashesNeeded[!(hashesNeeded %in% hashesPresent)]

  #attempt to download them all from the repo and extract them to binary_pkg folder
  gatherPkgsFromRepo(hashesNeeded, binaryPkgsPath, repoNames, additionalRepos)

  #check if we now have all. if not print something and return non null
  hashesPresent <- fs::path_file(fs::dir_ls(binaryPkgsPath, type='directory'))
  hashesleft <- hashesNeeded[!(hashesNeeded %in% hashesPresent)]
  if(length(hashesleft) > 0) {
    print('Failed to find and gather the following Packages:')
    print(hashesleft)
    return(-1)
  }

  return(0)
}

#' @export
createJaspModuleBundle <- function(moduleLib, resultdir = './', packageAll = TRUE, mustPackage=NULL, includeInManifest=NULL) {
  moduleName <- fs::path_file(moduleLib)
  stagingDir <- fs::dir_create(tempdir(), moduleName)
  on.exit(if(fs::dir_exists(stagingDir)) fs::dir_delete(stagingDir))
  tarDir <- fs::dir_create(stagingDir, 'tarDir')
  preCompressionBundleDir <- fs::dir_create(stagingDir, 'uncompressed')

  #gather final 'pkgName_version' for later manifest mapping
  pkgDirs <- fs::dir_ls(moduleLib, type = 'any')
  gatherNameVersionNum <- function(pkgDir) {
    info <- getModuleInfo(pkgDir) 
    version <- if('RemoteSha' %in% names(info)) substr(info[['RemoteSha']], 1, 8) else info[['Version']]
    name <- fs::path_file(pkgDir)
    paste(name, version, sep='_') #_ is not allowed in R pkg names
  }
  mappingNames <- sapply(pkgDirs, gatherNameVersionNum)
  
  #get all the pkgs and tar them. rename to hash of tar itself. Gives back a list of hashes with the original pkg-Name in names() (so a map)
  makeTar <- function(dir) {
    hash <- hashDir(dir)
    createL0TarAchive(dir, fs::path(tarDir, hash))
    hash
  }
  pkgToArchiveMap <- sapply(pkgDirs, makeTar)
  names(pkgToArchiveMap) <- fs::path_file(names(pkgToArchiveMap)) #strip rest of path in names() to make indexing on pkg name easy

  #copy the packages we are instructed to pack into the bundle
  if(packageAll)
    fs::file_copy(fs::dir_ls(tarDir), preCompressionBundleDir)
  else {
    if(stringr::str_detect(mustPackage[[1]], '_')) mustPackage <- stringr::str_split(mustPackage, pattern='_', simplify=TRUE)[,1] #delete the version number as we do not need it
    hashesToPack <- pkgToArchiveMap[mustPackage]
    fs::file_copy(fs::path(tarDir, hashesToPack), preCompressionBundleDir)
  }

  #write a little manifest with hash => name_version mappings
  version <- packageVersion(moduleName, moduleLib)
  packDate <- format(Sys.time(), "%a %b %d %X %Y")
  hash <- pkgToArchiveMap[moduleName]
  RVersion <- paste0('R-', paste(R.Version()$major, substring(R.Version()$minor, 1, 1), sep = '.'))
  os <- getOS()
  arch <- unname(Sys.info()['machine'])
  manifestList <- list(name=moduleName, version=version, complete=packageAll, checksum=hash, pack_date=packDate,
                  RVersion=RVersion, os=os, architecture=arch,
                  mapping=paste(pkgToArchiveMap, mappingNames, sep=" => "))
  json <- rjson::toJSON(c(manifestList, includeInManifest), indent=1)
  manifest <- fs::file_create(fs::path(preCompressionBundleDir, paste0(moduleName, '_manifest.json')))
  write(json, file=manifest)

  #archive all into one bundle and clean up
  resultPath = fs::path_ext_set(fs::path(resultdir, moduleName), 'JASPModule')
  createL0TarAchive(preCompressionBundleDir, resultPath)
}


#' @export
extractBundleIntoRemoteCellarRepo <- function(repoRoot, bundlePath, repoName='development', RVersion, os, architecture) {
  #extract bundle and parse manifest
  stagingDir <- fs::dir_create(tempdir(), 'bundleManagerRepoExtract') #cant use tempdir because of R tar..
  untarBundleDir <- fs::dir_create(stagingDir, 'bundleUntar')
  on.exit(if(fs::dir_exists(stagingDir))fs::dir_delete(stagingDir))
  extractL0TarAchive(bundlePath, untarBundleDir)
  manifestPath <- fs::dir_ls(untarBundleDir, type='file', glob='*_manifest.json')[[1]]
  manifest <- parseManifest(manifestPath)[, 1]

  if(missing(RVersion)) RVersion <- manifest$RVersion
  if(missing(os)) os <- manifest$os
  if(missing(architecture)) architecture <- manifest$architecture

  #create the necessary directories if they dont exist
  pkgsArchivePath <- fs::path(repoRoot, repoName, 'PKGS')
  cellarPath <- fs::path(repoRoot, repoName, RVersion, os, architecture)
  fs::dir_create(pkgsArchivePath)
  fs::dir_create(cellarPath)

  #write pkgs hash archives to pkgArchive
  pkgs <- fs::dir_ls(untarBundleDir, type='file', glob='*_manifest.json', invert=TRUE)
  copyPkgtoBinDir <- function(pkg) {
    hash <- fs::path_file(pkg)
    outPath <- fs::path(pkgsArchivePath, hash)
    if(fs::dir_exists(outPath))
      print('Hash conflict! We are overriding. You might have meant to do this, so dont worry?')
    fs::file_copy(pkg, outPath, overwrite = TRUE)
  }
  sapply(pkgs, copyPkgtoBinDir)

  #add symlinks to the cellarPath for those archives that we just extracted
  hashesPresent <- fs::path_file(pkgs)
  from <- fs::path(fs::path_rel(pkgsArchivePath, start=cellarPath), manifest$from)
  extractMask <- fs::path_file(from) %in% hashesPresent
  to <- paste0(fs::path(cellarPath, manifest$pkgs), '.tar.gz')
  createLink(from[extractMask], to[extractMask], forceSymlink = TRUE)
}





