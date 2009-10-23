## TODO: Accessor should be Accessor and Mutator

##------------------------------------------------------------##
## Internal functions
##------------------------------------------------------------##
printGSetterGenericWithFormatString <- function(formatString, slot) {
  sprintf(formatString, slot, slot)
}

printGetterGenericWithFormatString <- function(slot) {
  G_genericGetterFormatString <- "setGeneric(\"%s\", function(object) standardGeneric(\"%s\"))"
  printGSetterGenericWithFormatString(formatString=G_genericGetterFormatString,
                                       slot=slot)
}

printGetterMethodWithFormatString <- function(class, slot) {
  G_getterMethodFormatString <- "setMethod(\"%s\", signature=\"%s\", function(object) {object@%s})"
  sprintf(G_getterMethodFormatString, slot, class, slot)
}

printSetterGenericWithFormatString <- function(slot) {
  G_genericSetterFormatString <- "setGeneric(\"%s<-\", function(object,value) standardGeneric(\"%s<-\"))"
  printGSetterGenericWithFormatString(formatString=G_genericSetterFormatString,
                                       slot=slot)
}

printSetterMethodWithFormatString <- function(class, slot) {
  G_setterMethodFormatString <- "setReplaceMethod(\"%s\", signature=c(\"%s\", \"%s\"), function(object, value) {object@%s <- value; object})"
  slotValueType <- getSlots(class)[slot]
  sprintf(G_setterMethodFormatString, slot, class, slotValueType, slot)
}

##------------------------------------------------------------##
## user visible functions:
##------------------------------------------------------------##
getS4SlotGSetterGenericAndMethods <- function(class, slot) {
  stopifnot(length(slot)==1)
  getterGeneric <- printGetterGenericWithFormatString(slot=slot)
  setterGeneric <- printSetterGenericWithFormatString(slot=slot)
  getterMethod <- printGetterMethodWithFormatString(class=class, slot=slot)
  setterMethod <- printSetterMethodWithFormatString(class=class, slot=slot)
  funcs <- paste(getterGeneric,
                 setterGeneric,
                 getterMethod,
                 setterMethod, sep="\n")
  return(funcs)
}

getS4AllSlotsGSetterGenericAndMethods <- function(class) {
  slots <- slotNames(class)
  slotGenericAndMethods <- sapply(slots, getS4SlotGSetterGenericAndMethods, class=class)
  return(slotGenericAndMethods)

}

writeS4SlotGSetterGenericAndMethods <- function(class, slot, con=stdout(),...) {
  funcs <- getS4SlotGSetterGenericAndMethods(class=class, slot=slot)
  writeLines(text=funcs, con=con,...)
}

writeS4AllSlotsGSetterGenericAndMethods <- function(class, con=stdout(), ...) {
  funcs <- getS4AllSlotsGSetterGenericAndMethods(class=class)
  writeLines(text=funcs, con=con, ...)
}
