\name{getS4SlotGSetterGenericAndMethods}
\Rdversion{1.1}
\alias{getS4SlotGSetterGenericAndMethods}
\alias{getS4AllSlotsGSetterGenericAndMethods}
\alias{writeS4SlotGSetterGenericAndMethods}
\alias{writeS4AllSlotsGSetterGenericAndMethods}
%% private functions
%\alias{printGSetterGenericWithFormatString}
%\alias{printGetterGenericWithFormatString}
%\alias{printGetterMethodWithFormatString}
%\alias{printSetterGenericWithFormatString}
%\alias{printSetterMethodWithFormatString}
\title{AUTOMATIC WRAPPED GETTER AND SETTER GENERICS AND METHODS FOR S4 CLASSES}
\description{
  Automatic wrapping getter and setter (GSetter) generics and methods for S4 classes,
  like the functionality implemented in many IDEs for other programming
  languagues like Java or C#. 
}
\usage{
getS4SlotGSetterGenericAndMethods(class, slot)
getS4AllSlotsGSetterGenericAndMethods(class)

writeS4SlotGSetterGenericAndMethods(class, slot, con=stdout(), ...)
writeS4AllSlotsGSetterGenericAndMethods(class, con=stdout(),...)
}
\arguments{
  \item{class}{Name of a S4 class}
  \item{slot}{Slot name of a S4 class, must be of length one}
  \item{con}{Connection to write the output to, for example stardard
    output of R (\code{stdout()}) or a file name.}
  \item{...}{Other parameters passed to \code{writeLines}.}
}
\details{
  We use the abbreviation \emph{GSetter} to refer to the \emph{getter}
  (aka \emph{accessor}) and the \emph{setter} (aka \emph{mutator}) of S4
  class slots. The former gets the value of the slot, while the later specify the
  value of the slot.
  
  \code{getS4SlotGSetterGenericAndMethods} returns a string containing
  both generics and methods to get and set the value of the given slot
  in the specified class. It only receives one slot at a time. The
  results can be written into plain text file with the wrapper
  \code{writeS4SlotGSetterGenericAndMethods}. The user could comment out
  some of the generics or methods in the output file in case they do not
  need them.

  \code{getS4AllSlotsGSetterGenericAndMethods} returns the generics
  and methods of getters and setters of all slots in the speicified
  class in the form of a vector of character strings. User could use
  \code{writeS4AllSlotsGSetterGenericAndMethods} to write them into
  plain text files, which can be moved later to the R package.
  
}
\value{
  One or a vector of character strings containing the text form of the
  generics and methods.
}
%%\references{
%%%% ~put references to the literature/web site here ~
%%}
%%\author{
%%%%  ~~who you are~~
%%}
%%\note{
%%%%  ~~further notes~~
%%}
%%
%%%% ~Make other sections like Warning with \section{Warning }{....} ~
%%
%%\seealso{
%%%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
setClass("track", representation(xx="numeric", yy="numeric"))
## the trailing "\n" is just for the beauty of output
cat(getS4SlotGSetterGenericAndMethods("track", "xx"), "\n")
## it equals to
writeS4SlotGSetterGenericAndMethods("track", "xx", con=stdout())


cat(getS4AllSlotsGSetterGenericAndMethods("track"), "\n")
## one could also call
writeS4AllSlotsGSetterGenericAndMethods("track", con=stdout())
}
