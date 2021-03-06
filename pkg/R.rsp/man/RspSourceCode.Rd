%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  RspSourceCode.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{RspSourceCode}
\docType{class}
\alias{RspSourceCode}


\title{The RspSourceCode class}

\description{
 Package:  R.rsp \cr
\bold{Class RspSourceCode}\cr

\code{character}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.rsp]{RspObject}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.rsp]{RspProduct}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\emph{\code{RspSourceCode}}\cr

\bold{Directly known subclasses:}\cr
\emph{\link[R.rsp]{RspRSourceCode}}, \emph{\link[R.rsp]{RspShSourceCode}}\cr

public abstract class \bold{RspSourceCode}\cr
extends \emph{\link[R.rsp]{RspProduct}}\cr



 An RspSourceCode object is a \code{\link[base]{character}} \code{\link[base]{vector}} holding RSP generated
 source code for a particular programming language.
}

\usage{
RspSourceCode(code=character(), ...)
}

\arguments{
  \item{code}{\code{\link[base]{character}} \code{\link[base]{vector}}.}
  \item{...}{Additional arguments passed to the \code{\link{RspProduct}}
    constructor.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.rsp:evaluate.RspSourceCode]{evaluate}} \tab Evaluates the source code.\cr
 \tab \code{print} \tab  -\cr
 \tab \code{rstring} \tab  -\cr
 \tab \code{\link[R.rsp:tangle.RspSourceCode]{tangle}} \tab Drops all text-outputting calls from the source code.\cr
 \tab \code{\link[R.rsp:tidy.RspSourceCode]{tidy}} \tab Tidy up the RSP source code.\cr
}


 \bold{Methods inherited from RspProduct}:\cr
!, findProcessor, getMetadata, getType, hasProcessor, print, process, setMetadata, view

 \bold{Methods inherited from RspObject}:\cr
getAttribute, getAttributes, hasAttribute, print, setAttribute, setAttributes

 \bold{Methods inherited from character}:\cr
all.equal, as.Date, as.POSIXlt, as.data.frame, as.raster, downloadFile, formula, getDLLRegisteredRoutines, isOpen, toAsciiRegExprPattern, toFileListTree, toLatex, uses


}

\author{Henrik Bengtsson}


\keyword{classes}
\keyword{internal}
