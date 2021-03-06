library("R.utils")

# - - - - - - - - - - - - - - - - - - - - - - - - -
# Function that takes "a long" time to run
# - - - - - - - - - - - - - - - - - - - - - - - - -
foo <- function() {
  print("Tic");
  for (kk in 1:20) {
    print(kk);
    Sys.sleep(0.1);
  }
  print("Tac");
}


# - - - - - - - - - - - - - - - - - - - - - - - - -
# Evaluate code, if it takes too long, generate
# a timeout by throwing a TimeoutException.
# - - - - - - - - - - - - - - - - - - - - - - - - -
res <- NULL;
tryCatch({
  res <- withTimeout({
    foo();
  }, timeout=1.08);
}, TimeoutException=function(ex) {
  cat("Timeout (", getMessage(ex), "). Skipping.\n", sep="");
})


# - - - - - - - - - - - - - - - - - - - - - - - - -
# Evaluate code, if it takes too long, generate
# a timeout, and return silently NULL.
# - - - - - - - - - - - - - - - - - - - - - - - - -
res <- withTimeout({
  foo();
}, timeout=1.08, onTimeout="silent");


# - - - - - - - - - - - - - - - - - - - - - - - - -
# Evaluate code, that does not timeout, then
# evaluate code that takes long, but should not
# timeout.
# - - - - - - - - - - - - - - - - - - - - - - - - -
res <- withTimeout({
  cat("Hello world!\n")
}, timeout=1.08);
foo();
