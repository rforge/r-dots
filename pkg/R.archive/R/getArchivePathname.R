#########################################################################/**
# @RdocDefault getArchivePathname
#
# @title "Generates the pathname in the archive for a file"
#
# \description{
#  @get "title" based on its filename.
# }
#
# @synopsis
#
# \arguments{
#   \item{filename}{A @character string specifying the filename/pathname
#      of the file to be archived, and on which the archived filename
#      will be based on.}
#   \item{...}{Additional arguments passed to @see "getArchivePath"
#      and @see "getArchiveFilename".}
#   \item{mustNotExist}{If @TRUE and there already exists a file with
#      the same archive pathname, then an exception is thrown.}
# }
#
# \value{
#   Returns the archive pathname as a @character string.
# }
#
# @author
#
# \seealso{
#  @see "archiveFile".
# }
#
# @keyword "programming"
# @keyword "IO"
# @keyword "internal"
#*/#########################################################################  
setMethodS3("getArchivePathname", "default", function(filename, ..., mustNotExist=TRUE) {
  pathA <- getArchivePath(...);

  filenameA <- getArchiveFilename(filename, ...);

  pathnameA <- Arguments$getWritablePathname(filenameA, path=pathA, mustNotExist=mustNotExist);

  pathnameA;
}) # getArchivePathname()



############################################################################
# HISTORY:
# 2011-03-18
# o In order to make it more unique, the timestamp added to files now
#   includes milliseconds as well, e.g. 041359.032.
# 2011-03-09
# o Added getArchiveFilename() and getArchivePathname().
# o Created.
############################################################################
