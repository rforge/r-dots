library("R.utils")
warnifnot <- egsub("stop", "warning", stopifnot, value=FALSE)


# Current directory
stopifnot(identical(getAbsolutePath("."), getwd()))
stopifnot(identical(getRelativePath("."), "."))

# Tilde expansion
pathH <- normalizePath("~")
print(pathH)
pathHA <- getAbsolutePath(pathH)
print(pathHA)
pathA <- getAbsolutePath("~", expandTilde=TRUE)
print(pathA)
pathR <- getRelativePath("~")
print(pathR)
warnifnot(identical(tolower(pathA), tolower(pathH)))
warnifnot(identical(tolower(getAbsolutePath(pathR)), tolower(pathH)))

# Microsoft Windows UNC paths
stopifnot(identical(getAbsolutePath("//vinata/biomed"), "//vinata/biomed"))
stopifnot(identical(getAbsolutePath("//vinata///biomed"), "//vinata/biomed"))

# Vector of files
paths <- c(".", "..", getwd())
print(paths)
pathsA <- getAbsolutePath(paths)
print(pathsA)
pathsR <- getRelativePath(paths)
print(pathsR)
pathsAR <- getRelativePath(pathsA)
print(pathsAR)
pathsRA <- getAbsolutePath(pathsR)
print(pathsRA)

# Sanity checks
stopifnot(all(isAbsolutePath(pathsA)))
stopifnot(all(!isAbsolutePath(pathsR)))
stopifnot(all(pathsRA == pathsA))
stopifnot(all(pathsAR == pathsR))

# Paths relative to give directories
stopifnot(getRelativePath("foo", "foo") == ".")
stopifnot(getRelativePath("foo/bar", "foo") == "bar")
stopifnot(getRelativePath("foo/bar", "foo/bar/yah") == "..")
stopifnot(getRelativePath("foo/bar/cool", "foo/bar/yah/sub/") == "../../cool")
stopifnot(getRelativePath("/foo/bar/", "/bar/foo/") == "../../foo/bar")
stopifnot(getRelativePath("C:/foo/bar/", "C:/bar/") == "../foo/bar")
stopifnot(getRelativePath("C:/foo/bar/", "D:/bar/") == "C:/foo/bar")
