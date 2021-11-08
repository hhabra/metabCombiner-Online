library(metabCombiner)

validateRTbounds <- function(rtmin, rtmax){
    if(rtmin != "min")
        rtmin = as.numeric(rtmin)

    if(rtmax != "max")
        rtmax = as.numeric(rtmax)

    return(list(rtmin = rtmin, rtmax = rtmax))
}

##because regular ifelse doesn't work for some reason with NULL outcomes
if_else <- function(expression, value1, value2){
    if(expression)
        value1
    else
        value2
}

updatePlot <- function(state, anchors = FALSE, fit = FALSE)
{
    state$plotAnchors <- anchors
    state$plotFit <- fit

    return(state)
}

attempt <- function(f){
    tryCatch(
        f, error = function(e)
           showNotification(paste("Error: ", e$message), type = "error")
    )
}

attCatch <- function(f, object)
{
    res <- attempt(f)

    if(!is.character(res))
        return(res)
    else
        return(object)
}




