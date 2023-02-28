library(dplyr)
library(readr)
library(ggplot2)
library(GGally)
library(vcd)
library(data.table)
library(gtools)
library(rpart)
library(rpart.plot)
library(caret)
library(formattable)
library(FFTrees)
train=read.csv("train.csv",header=T)
test<-read.csv("test.csv",header=T)
full<-bind_rows(train,test)

glimpse(full)
summary(full)
names(full)


g1=ggplot(full, aes(x = as.factor(Survived),fill=as.factor(Survived))) + 
  geom_bar()+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base Completa")

g2=ggplot(train, aes(x = as.factor(Survived),fill=as.factor(Survived))) + 
  geom_bar()+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")

gridExtra::grid.arrange(g1,g2,ncol=2)



ggplot(train, aes(x = as.factor(Pclass),fill=as.factor(Survived))) + 
  geom_bar(position = "dodge")+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")


ggplot(train, aes(x = as.factor(Pclass),fill=as.factor(Survived))) + 
  geom_bar(position = "dodge")+scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100))+theme_bw()+
  labs(x="Sobrevivente",title="Base de Treino")


full$SupFare=ifelse(full$Fare>50,1,0)
full$Survived=as.factor(full$Survived)
full$Pclass=as.factor(full$Pclass)
full$Sex=as.factor(full$Sex)
full$Embarked=as.factor(full$Embarked)
full$Title=as.factor(full$Title)
full$SupFare=as.factor(full$SupFare)

full=full%>%
  select(Survived, Pclass, Sex, SepAge, Embarked, Fsize, SupFare,Title)

train <- full[1:891,]
test <- full[892:1309,]


tableplot <- function(dat, select, subset=NULL, sortCol=1,  decreasing=TRUE, 
                      nBins=100, from=0, to=100, nCols=ncol(dat),
                      sample=FALSE,
                      sampleBinSize=1e3,
                      scales="auto", 
                      numMode="mb-sdb-ml",
                      max_levels=50, 
                      pals=list("Set1", "Set2", "Set3", "Set4"), 
                      change_palette_type_at = 20,
                      rev_legend=FALSE,
                      colorNA = "#FF1414", 
                      colorNA_num = "gray75",
                      numPals = "OrBu", 
                      limitsX = NULL,
                      bias_brokenX=0.8, IQR_bias=5, 
                      select_string = NULL,
                      subset_string=NULL, colNames=NULL, filter=NULL, 
                      plot=TRUE, ...) {
  
  
  #require(ffbase)
  
  ##################################
  ## prepare data if necessary
  ##################################
  
  p <- dat
  is_prepared <- inherits(dat, "prepared")
  
  if (!is_prepared) {
    datName <- deparse(substitute(dat))
    p <- tablePrepare(dat, name=datName)
  } else datName <- attr(p, "name")
  
  dat <- p$data
  
  ##################################
  ## check select and subset arguments
  ##################################
  
  ## discourage colNames and filter arguments
  if (!missing(colNames)) {
    warning("The argument colNames will not be supported 
				anymore in the future versions of tabplot. 
				Use select or select_string instead")
    select_string <- colNames
  }
  
  if (!missing(filter)) {
    warning("The argument filter will not be supported 
				anymore in the future versions of tabplot. 
				Use subset or subset_string instead")  
    subset_string <- filter
  }
  
  ## argument subset(string): complement subset and subset_string
  if (!missing(subset)) {
    subset_string <- deparse(substitute(subset))
  } else if (!missing(subset_string)) {
    subset <- parse(text=subset_string)
  }
  
  ## argument select(_string) argument
  if (!missing(select)) {
    select_string <- as.character(substitute(select))
    if (any(sapply(select_string, function(x)substr(x,1,1) %in% as.character(0:9)))) {
      # evaluate indices directly (not working for subtractions)
      nl <- as.list(seq_along(dat))
      names(nl) <- names(dat)
      select_string <- eval(substitute(select), nl, parent.frame())
      select_string <- names(dat)[select_string]
      
    } else {
      select_call <- deparse(substitute(select))
      if ((substr(select_call[1], 1, 2))=="c(") select_string <- select_string[-1]
    }
    
  } 
  
  if (!is.null(select_string)) {
    #colNames <- select_string
    
    allColNames <- strsplit(select_string, "[ ]?-[ ]?")
    colNames1 <- sapply(allColNames, function(x)x[1])
    colNames2 <- sapply(allColNames, function(x)x[2])
    colNames <- unique(c(colNames1, na.omit(colNames2)))
    
    if (!all(colNames %in% names(dat))) 
      stop("select(_string) contains wrong column names")
  } else {
    colNames <- names(dat)
    
    colNames1 <- colNames
    colNames2 <- rep(NA, length(colNames))
  }
  
  colNames_string <- mapply(function(x,y) ifelse(is.na(y), x, paste(x,y,sep="-")), colNames1, colNames2, SIMPLIFY = TRUE)
  
  ## argument sortCol
  sortCol <- tableplot_checkCols(substitute(sortCol), sortCol, colNames)
  sortColName <- colNames[sortCol]
  if (!(sortColName %in% colNames_string)) stop("sortCol should be plotted")
  
  
  
  ##################################
  ## subset data
  ##################################
  if (!missing(subset_string)) {
    # split by one variable
    if (subset_string %in% names(dat)) {
      lvls <- levels(dat[[subset_string]])
      
      if ((class(dat[[subset_string]])[1]=="logical") || (class(dat)[1]=="ffdf" &&
                                                          vmode(dat[[subset_string]]) %in% c("boolean", "logical"))) {
        isLogical <- TRUE
        lvls <- c("TRUE", "FALSE")
      } else {
        isLogical <- FALSE
      }
      if (is.null(lvls)) stop("subset variable is not categorical")
      
      subsets_string <- paste(subset_string, " == ", 
                              ifelse(isLogical, "", "\""), lvls,
                              ifelse(isLogical, "", "\""), sep="")
      tabs <- lapply(subsets_string, FUN=function(subs_string){
        tableplot(p, select_string=colNames_string, sortCol=sortCol, 
                  decreasing=decreasing, scales=scales, max_levels=max_levels, 
                  pals=pals, nBins=nBins,
                  from=from, to=to, subset_string=subs_string,
                  change_palette_type_at=change_palette_type_at,
                  rev_legend = rev_legend,
                  colorNA = colorNA, colorNA_num = colorNA_num, numPals = numPals, limitsX=limitsX,
                  bias_brokenX=bias_brokenX, IQR_bias=IQR_bias, plot=plot, ...)
      })
      return(invisible(tabs))
    }		
    p <- subset_data(p, cols=colNames, subset_string=subset_string, sortCol=sortCol)
    dat <- p$data
  }	
  
  ##################################
  ## other checks
  ##################################
  isNumber <- sapply(physical(dat)[colNames], function(col)!is.factor.ff(col) && !vmode(col)=="logical")
  
  if (nrow(dat)==0) stop("<dat> doesn't have any rows")
  if (nrow(dat)==1) stop("<dat> has only one row")
  decreasing <- tableplot_checkDecreasing(decreasing, sortCol)
  nCols <- tableplot_checkNcols(nCols, colNames, sortCol)
  scales <- tableplot_checkScales(scales, colNames, isNumber)
  pals <- tableplot_checkPals(pals, colNames, !isNumber)
  if (any(!isNumber))
    change_palette_type_at <- tableplot_checkChangePalType(change_palette_type_at, 
                                                           max(sapply(pals[!isNumber], function(pal)length(pal$palette))))
  if (class(try(col2rgb(colorNA), silent=TRUE))=="try-error") 
    stop("<colorNA> is not correct")
  if (class(try(col2rgb(colorNA_num), silent=TRUE))=="try-error") 
    stop("<colorNA_num> is not correct")
  
  rev_legend <- tableplot_checkRevLeg(rev_legend, colNames)
  
  
  numPals <- tableplot_checkNumPals(numPals, colNames, isNumber)
  limitsX <- if (missing(limitsX)) list() else tableplot_checkLimitsX(limitsX, colNames, isNumber)
  tableplot_checkFromTo(from, to)
  
  #browser()
  N <- as.integer(length(p$ordered[[1]]) * (to-from)/100)
  nBins <- tableplot_checkBins(nBins, max(N,2))
  n <- ifelse(sample, min(nBins * sampleBinSize, N), N)
  
  numMode <- strsplit(numMode, "-", fixed=TRUE)[[1]]
  
  
  
  ##################################
  ## bin data
  ##################################
  
  bd <- bin_data( p, sortCol=sortCol, cols=colNames, from=from/100, to=to/100
                  , nbins=nBins, decreasing=decreasing, sample, sampleBinSize=sampleBinSize)
  
  bd <- bin_hcc_data(bd, max_levels)
  #print(list(bd=bd))
  tab <- columnTable( bd, datName, colNames=colNames, subset_string=subset_string, 
                      sortCol=sortCol, decreasing=decreasing, scales=scales, 
                      pals=pals, 
                      change_palette_type_at=change_palette_type_at,
                      rev_legend=rev_legend,
                      colorNA=colorNA, colorNA_num=colorNA_num, numPals=numPals, nBins=nBins, from=from, 
                      to=to, N=N, n=n)
  
  ##################################
  ## Clean temp ff objects
  ##################################
  if (!is_prepared) lapply(p, ff::close.ffdf)
  
  ##################################
  ## Grammar of Graphics, and create difference columns
  ##################################
  tab <- tableplot_processCols(tab, colNames1, colNames2, IQR_bias, bias_brokenX, limitsX, nBins, sortColName, numMode)
  
  
  
  
  ##################################
  ## output
  ##################################
  
  ### multiple tableplots if nCols < length(colNames)
  if (nCols < length(colNames)) {
    tabs <- splitTab(tab, nCols)
    if (plot) sapply(tabs, plot, ...)
    invisible(tabs)
  } else {
    if (plot) plot(tab, ...)
    invisible(tab)
  }
}

tableplot(train, sortCol = Survived)

ggpairs(train, mapping = aes(color = Survived),binwidth=.5)



