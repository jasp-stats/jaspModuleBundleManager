#' @export
installJaspModuleBundle <- function(bundlePath, installPath) {
  binaryPkgsPath <- fs::path(installPath, 'binary_pkgs') 
  modulesLibPaths <- fs::path(installPath, 'module_libs')
  manifestPath <- fs::path(installPath, 'manifests')

  #create the necessary directories in installPath if they dont exist
  fs::dir_create(binaryPkgsPath)
  fs::dir_create(modulesLibPaths)
  fs::dir_create(manifestPath)

  #copy all binary pkgs we do not yet have. 
  unzip(bundlePath, overwrite=FALSE, exdir=binaryPkgsPath)

  #Move manifest to manifest folder. But first check if a different version is already installed if so uninstall
  manifest <- fs::dir_ls(binaryPkgsPath, type='file', glob='*_manifest.json')
  manifestDestinationPath <- fs::path(manifestPath, fs::path_file(manifest[[1]]))
  if(fs::file_exists(manifestDestinationPath)) deleteJaspModuleBundleByManifest(installPath, manifestDestinationPath, newManifest=manifest[[1]]) #If this module is already present delete it
  tmp <- fs::file_copy(manifest[[1]], manifestDestinationPath, overwrite=TRUE)
  fs::file_delete(manifest)
  manifest <- tmp

  #create moduleLib entry (folder with symlinks to actual pkgs) from manifest mapping
  manifest <- parseManifest(manifest)[, 1]
  entryPath <- fs::path(modulesLibPaths, manifest$name)
  fs::dir_create(entryPath)
  from <- fs::path(fs::path_rel(binaryPkgsPath, start=entryPath), manifest$from)
  to <- fs::path(entryPath, manifest$to)
  createLink(from, to)
}

#' @export
deleteJaspModuleBundle <- function(name, installPath) {
  manifests <- fs::dir_ls(fs::path(installPath, 'manifests'), type='file', regexp=name)
  deleteJaspModuleBundleByManifest(installPath, manifests)
}

#' @export
createJaspModuleBundle <- function(moduledir, resultdir) {
  name <- fs::path_file(moduledir)

  #copy all the (dependency) rpkg folders in the moduledir but change the name of their roots to their sha256 hash
  content <- fs::path(moduledir, dir(moduledir))
  checkSum <- generateDirectoryChecksum(content)
  stagingDir <- fs::dir_create(tempdir(), name)
  newDir <- fs::dir_create(stagingDir, checkSum)  
  fs::dir_copy(content, newDir, overwrite=TRUE)
  
  #write a little manifest with hash => name mappings
  version <- packageVersion(name, moduledir)
  packDate <- format(Sys.time(), "%a %b %d %X %Y")
  hash <- generateDirectoryChecksum(fs::path(moduledir, name))[[1]]
  json <- rjson::toJSON(list(name=name, version=version, checksum=hash, pack_date=packDate, mapping=paste(checkSum, fs::path_file(content), sep=" => ")), indent=1)
  manifest <- fs::file_create(fs::path(stagingDir, paste0(name, '_manifest.json')))
  write(json, file=manifest)
  
  #zip and clean up
  resultPath = fs::path_ext_set(fs::path(resultdir, name), 'JASPModule')
  zip::zipr(resultPath, fs::dir_ls(stagingDir))
  unlink(stagingDir, recursive=TRUE)
}

#remove the binarys related to oldManifst, that are unused in the other manifest in manifest folder or newManifest
#remove the library folder entry and the manifest
#' @export
deleteJaspModuleBundleByManifest <- function(installPath, oldManifest, newManifest=NULL) {
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
  fs::dir_delete(entry)

  #delete manifest
  fs::file_delete(oldManifest)

}
