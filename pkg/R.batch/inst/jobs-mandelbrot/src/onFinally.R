####################################################################
# There is a risk, although small, that the job is interrupted
# or that an error occurs while creating the image. Regardless of
# if the job was successful, an interrupt or an error occured, the
# onFinally() function will always be called.
####################################################################
onFinally <- function(...) {
  # Add an finalizing code here, e.g. closing open connections.
}
