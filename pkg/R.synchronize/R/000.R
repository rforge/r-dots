############################################################################
# This code has to come first in a library. To do this make sure this file
# is named "000.R" (zeros).
############################################################################

# Is autoload() allowed in R v2.0.0 and higher? According to the help one
# should not use require().
autoload("appendVarArgs", package="R.methodsS3")
autoload("hasVarArgs", package="R.methodsS3")
autoload("setMethodS3", package="R.methodsS3")
autoload("setConstructorS3", package="R.oo")
