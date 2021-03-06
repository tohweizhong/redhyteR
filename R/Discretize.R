
# Discretize.R (internal)

# function to convert all numeric variables by the dataset into categorical variables
# except Atgt and Acmp

Discretize <- function(df, Atgt, Acmp, by = "mean"){

    print(colnames(df))
    tgt <- df[,Atgt]
    cmp <- df[,Acmp]

    df <- df[, !(colnames(df) %in% c(eval(Atgt), eval(Acmp)))]
    ncols <- ncol(df)

    classes <- unlist(sapply(c(1:ncols), FUN = function(c){
        return(class(df[,c]))
    }))

    which_are_num <- union(which(classes == "integer"), which(classes == "numeric"))

    # v: vector, t: numeric threshold
    BinBy <- function(v, t){

        returnMe <- NULL
        for(i in seq(length(v))){
            if(v[i] >= t) returnMe <- c(returnMe, paste0("above", t))
            else if(v[i] < t) returnMe <- c(returnMe, paste0("below", t))
        }
        return(returnMe)
    }

    for(ii in which_are_num){

        m <- round(mean(df[,ii]), digits = 1)
        df[,ii] <- factor(BinBy(df[,ii], m))

    }

    df <- cbind(df, tgt, cmp)
    colnames(df)[c((ncol(df) - 1) : ncol(df))] <- c(Atgt, Acmp)

    return(df)
}
