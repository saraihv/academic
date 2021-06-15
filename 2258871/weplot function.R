
weplot <- function(x = NULL, y = NULL, data = NULL, group = FALSE, group.type = "color",
                   ylab = NULL, xlab = NULL, group.lab = NULL, type = "point",
                   color = NULL, edge.color = "black", transparency = 0, xlim = NULL, ylim = NULL,
                   bins = NULL, log = "", title = NULL, give.data = FALSE,
                   commas = ""){
  
  
  # # # Yes, the code below is UGLY, but it works...  # # #
  
  
  blue <- rgb(14, 40, 121, maxColorValue = 255)
  
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(scales))
  
  #gets input arguments
  args = as.list(match.call()[-1])
  
  arg.names <- names(args)
  
  #keeps the arguments separate
  args <- as.character(args)
  
  if (is.element("x", commas)){
    num.format.x <- comma
  } else {
    num.format.x <- function(x) format(x, big.mark = "",
                                       scientific = FALSE)
  }
  
  if (is.element("y", commas)){
    num.format.y <- comma
  } else {
    num.format.y <- function(x) format(x, big.mark = "",
                                       scientific = FALSE)
  }
  
  
  # print(args)
  # print(arg.names)
  
  y.mat <- FALSE
  
  
  if (!is.null(data)){
    
    # d <- as_tibble(data)
    # 
    # #SIMPLY RENAME COLUMNS!
    # #Creates character objects
    # X <- gsub("\"", "", deparse(substitute(x)))
    # Y <- gsub("\"", "", deparse(substitute(y)))
    # Group <- gsub("\"", "", deparse(substitute(group)))
    # 
    # print(Group)
    # 
    # names(d)[names(d) == X] <- "X"
    # names(d)[names(d) == Y] <- "Y"
    # 
    # if (Group != "FALSE"){
    #   group <- TRUE
    #   names(d)[names(d) == Group] <- "Group"
    #   
    #   if(is.null(group.lab)) group.lab <- Group
    #   print(group.lab)
    #   
    # } else {
    #   group <- FALSE
    # }
    # 
    
    #applies any input argument functions to column
    get.data <- function(data, arg.char){
      
      vars <- names(data)
      
      for (i in 1:length(vars)){
        assign(vars[i], data[[i]])
      }
      
      return(eval(parse(text = args[arg.names == arg.char])))
      
    }
    
    
    if(is.element("x", arg.names)){
      # message("x!")
      x <- list(get.data(data, "x"))
      names(x) <- args[arg.names == "x"]
      # message("x done!")
    } else {
      x <- NULL
    }
    
    if(is.element("y", arg.names)){
      # message("y!")
      y <- list(get.data(data, "y"))
      names(y) <- args[arg.names == "y"]
    } else {
      y <- NULL
    }
    
    
    # print(x)
    # print(y)  
    
    #x is null, switch y and x
    if(is.null(x) & !is.null(y)){
      # message("no x!")
      
      x <- y
      y <- NULL
      xlim <- ylim
      
      arg.names[arg.names == "y"] <- "x"
    }
    
    
    if(is.null(y)){
      
      if (type == "hist"){
        
        d <- tibble(X = x[[1]])
        # if (is.null(ylab)) ylab <- args[arg.names == "x"]
        if (is.null(xlab)) xlab <- args[arg.names == "x"]
        
        
        if(!missing(group)){
          if(args[arg.names == "group"] != "FALSE"){
            
            d <- bind_cols(d, tibble(Group = get.data(data, "group")))
            # print(d)
            if(is.numeric(d$Group)) stop("Grouping variable cannot be numeric for a histogram")
            
            if (is.null(group.lab)) group.lab <- args[arg.names == "group"]
            
            group <- TRUE
            
            
          }
        }
        
        
      } else {
        
        d <- tibble(Y = x[[1]])
        if (is.null(ylab)) ylab <- args[arg.names == "x"]
        if (is.null(xlab)) xlab <- "Index"
        
        d$X = 1:length(x[[1]])  #will be replaced if group
        
        if(!missing(group)){
          if(args[arg.names == "group"] != "FALSE"){
            
            
            d$Group <- get.data(data, "group")
            group <- TRUE
            if (is.null(group.lab)) group.lab <- args[arg.names == "group"]
            
            if (!is.numeric(d$Group)){
              
              # message("categorical")
              d$X <- numeric(nrow(d))
              
              for (u in unique(d$Group)){
                
                j <- which(d$Group == u)
                
                d$X[j] <- 1:length(j)
                
                
              }#loop
            }#group is categorical
          }#group isn't FALSE 
          
        } #if group is missing
        
        
        
      }# not histogram, but index
      
      
    } else {
      #X and Y provided
      
      
      d <- bind_cols(x, y)
      names(d) <- c("X", "Y")
      
      
      if (is.null(xlab)) xlab <- names(x)[1]
      if (is.null(ylab)) ylab <- names(y)[1]
      
      if(!missing(group)){
        if(args[arg.names == "group"] != "FALSE"){
          
          d <- bind_cols(d, tibble(Group = get.data(data, "group")))
          # print(d)
          
          if (is.null(group.lab)) group.lab <- args[arg.names == "group"]
          
          group <- TRUE
        }
      }
    }
    
    
    # print(x)
    # print(y)
    # 
    # print(d)
    
    
  } else {
    
    
    #Deals with lists and names
    list.names <- function(arg.obj, arg.char){
      
      if (is.list(arg.obj)){
        
        if (!is.null(names(arg.obj))){
          #at least some names given
          
          arg.obj.names <- names(arg.obj)
          
          i <- which(arg.obj.names == "")  #any names blank?
          
        } else {
          
          i <- 1:length(arg.obj)
          arg.obj.names <- character(length(arg.obj))
          
        }
        
        #uses alist function to get args as is
        arg.obj.args.list <- eval(parse(text = paste0("a", args[arg.names == arg.char])))
        
        arg.obj.names[i] <- as.character(arg.obj.args.list)[i]
        
        names(arg.obj) <- arg.obj.names
        
      } else {
        
        arg.obj <- list(arg.obj)
        names(arg.obj) <- args[arg.names == arg.char]
        
      }
      
      return(arg.obj)
      
    }
    
    existing.obj <- function(arg.obj, arg.char){
      if (is.numeric(arg.obj) | is.Date(arg.obj) | is.POSIXt(arg.obj)){
        
        if (is.matrix(arg.obj)){
          # message("y is a matrix!")
          # print(y)
          # y.mat <- TRUE
          
          arg.obj <- as.list(as.data.frame(arg.obj))
          
          attr(arg.obj,'matrix') <- TRUE
          attr(arg.obj,'name') <- args[arg.names == arg.char]
          # print(y)
          
        } else {
          
          arg.obj <- list.names(arg.obj, arg.char)
          
        }
        
        # is numeric
      } else if (is.list(arg.obj)){
        
        if (!is.null(names(arg.obj))){
          #at least some names given
          i <- which(names(arg.obj) == "")  #any names blank?
          
        } else {
          
          i <- 1:length(arg.obj)
          names(arg.obj) <- character(length(arg.obj))
          
        }
        
        names(arg.obj)[i] <- paste("Var",i)
        
      } #is list
      
      return(arg.obj)
      
    } # existing.obj
    
    #old way
    # if(!is.null(x)) x <- list.names(x, "x")
    # if(!is.null(y)) y <- list.names(y, "y")
    
    
    
    #new way
    if(!is.null(x)){
      
      if(exists(args[arg.names == "x"], 1)){
        
        x <- existing.obj(x, "x")
        
      } else {
        
        x <- list.names(x, "x")
        
      }
    }
    
    if(!is.null(y)){
      
      if(exists(args[arg.names == "y"], 1)){
        
        y <- existing.obj(y, "y")
        
      } else {
        
        y <- list.names(y, "y")
        
      }
    }
    
    
    # print(names(y))
    
    # return(list(x,y))
    
    
    
    #if only one var, make it x
    if(is.null(x) & !is.null(y)){
      x <- y
      y <- NULL
      xlim <- ylim
    }
    
    # message("str(x)")
    # cat(str(x))
    # message("str(y)")
    # cat(str(y))
    
    #put together
    if (length(x) == 1){
      
      if (is.null(y)){
        # message("histogram")
        
        if (type == "hist"){
          
          d <- tibble(X = x[[1]])
          
          type = "hist"
          
          if (is.null(xlab)) xlab <- names(x)[1]
          
        } else {
          
          d <- tibble(X = 1:length(x[[1]]), Y = x[[1]])
          
          if (is.null(xlab)) xlab <- "Index"
          if (is.null(ylab)) ylab <- names(x)[1]
          
        }
        
        
        if(!missing(group)){
          if(args[arg.names == "group"] != "FALSE"){
            
            group <- list.names(group, "group")
            
            d <- bind_cols(d, tibble(Group = group[[1]]))
            # print(d)
            
            if (is.null(group.lab)) group.lab <- args[arg.names == "group"]
            
            group <- TRUE
          }
        }
        
        
        
      } else {
        
        lens <- c(sapply(x, length), sapply(y, length))
        # print(lens)
        
        if (!any(mean(lens) == range(lens))) stop("x and y variables must have the same length")
        
        
        if (length(y) == 1){
          # message("single x, single y")
          
          d <- bind_cols(x, y)
          names(d) <- c("X", "Y")
          
          if(!missing(group)){
            if(args[arg.names == "group"] != "FALSE"){
              
              group <- list.names(group, "group")
              
              d <- bind_cols(d, tibble(Group = group[[1]]))
              # print(d)
              
              if (is.null(group.lab)) group.lab <- args[arg.names == "group"]
              
              group <- TRUE
            }
          }
          
          
          # print(d)
          
          if (is.null(xlab)) xlab <- names(x)[1]
          if (is.null(ylab)) ylab <- names(y)[1]
          
        } else {
          # message("single x, multiple y")
          
          
          # print(attr(y,'matrix'))
          
          d <- bind_cols(x, y)
          # names(d)[1] <- "X"  #keep y names for gather
          names(d) <- c("X", names(y))
          
          
          # print(names(y))
          # print(names(d))
          
          d <- gather(d, key = Group, value = Y, all_of(names(y)))
          
          #put in order given
          d$Group <- factor(d$Group, levels = names(y))
          
          group <- TRUE
          
          if(!is.null(attr(y,'matrix'))){
            y.mat <- TRUE
            ylab <- attr(y,'name')
          }
          # if(!is.null(attr(y,'name')) & is.null(ylab))  ylab <- attr(y,'name')
          
          if (is.null(xlab)) xlab <- names(x)[1]
          if (is.null(ylab)) ylab <- ""
          
          
          
          
          
          
        }
        
      }
      
    } else {
      
      
      if (is.null(y)){
        # message("histogram with multiple x")
        
        if (type == "hist"){
          
          d <- tibble(X = x[[1]], Group = names(x)[1])
          
          for (i in 2:length(x)) d <- bind_rows(d, tibble(X = x[[i]], Group = names(x)[i]))
          
          d$Group <- factor(d$Group, levels = names(x))
          
          type = "hist"
          
          group <- TRUE
          
          if (is.null(xlab)) xlab <- ""
          
        } else {
          
          # message("indexed single x")
          # print(attr(x,'matrix'))
          
          d <- tibble(X = 1:length(x[[1]]), Y = x[[1]], Group = names(x)[1])
          
          for (i in 2:length(x)) d <- bind_rows(d, tibble(X = 1:length(x[[i]]), Y = x[[i]], Group = names(x)[i]))
          
          d$Group <- factor(d$Group, levels = names(x))
          
          group <- TRUE
          
          if(!is.null(attr(x,'matrix'))) y.mat <- TRUE
          # if(!is.null(attr(x,'name')) & is.null(ylab))  ylab <- attr(x,'name')
          
          
          if (is.null(xlab)) xlab <- "Index"
          if (is.null(ylab)) ylab <- ""
          
        }
        
      } else {
        # message("multiple x, multiple y")
        
        if (any(sapply(x, length) != sapply(y, length))) stop("x and y variables must have the same length")
        
        d <- tibble(X = x[[1]], Y = y[[1]], Group = names(y)[1])
        
        for (i in 2:length(x)) d <- bind_rows(d, tibble(X = x[[i]], Y = y[[i]], Group = names(y)[i]))
        
        #put in order given
        d$Group <- factor(d$Group, levels = names(y))
        
        group <- TRUE
        
        if (is.null(xlab)) xlab <- ""
        if (is.null(ylab)) ylab <- ""
        
      }
      
      
    }
    
    
  }
  
  # print(X)
  # print(Y)
  # print(Group)
  
  
  if (group & !is.null(color) & !y.mat & group.type != "panels")
    if (length(color) != length(levels(d$Group))){
      warning("Number of colors provided does not match number of groups \n  Using default colors")
      color <- NULL
    }
  
  if (type == "hist"){
    
    # message("Plot Histogram")
    
    p <- ggplot(data = d, aes(x = X))
    
    if (is.null(bins)) bins <- ceiling(sqrt(sum(!is.na(d$X))))
    
    # if (is.null(color)) {
    #   p <- p + geom_histogram(alpha = 1 - transparency, bins = bins, fill = blue)
    # } else {
    #   p <- p + geom_histogram(fill = color, alpha = 1 - transparency, bins = bins)
    # }
    
    
    if (!group | group.type == "panels") {
      
      if (is.null(color)) {
        p <- p + geom_histogram(alpha = 1 - transparency, bins = bins, fill = blue, color = edge.color)
      } else {
        p <- p + geom_histogram(fill = color, alpha = 1 - transparency, bins = bins, color = edge.color)
      }
      
    } else if (group.type == "color") {
      
      # print(group.lab)
      p <- p + geom_histogram(aes(fill = Group), alpha = 1 - transparency, bins = bins, color = edge.color) +
        labs(fill = group.lab)
      
      if (!is.null(color)) p <- p + scale_fill_manual(values = color)
      # add.color.scale <- TRUE
    }
    
    
    
  } else {
    
    # options(warn = -1)
    # if (type == "default") type <- "point"
    # options(warn = 0)
    # # if (is.null(ylab)) ylab <- "Y"
    
    switch(type,
           point = {geom <- geom_point},
           line = {geom <- geom_line},
           both = {geom <- geom_line},
           path = {geom <- geom_path},
           area = {geom <- geom_area})
    
    
    if (missing(edge.color)) edge.color <- NA
    
    p <- ggplot(data = d, aes(x = X, y = Y))
    
    
    #COLOR----
    if (is.element(type, c("point", "line", "path", "both"))) {
      
      # print(group)
      # print(group.type)
      # print(names(d))
      
      if (!group | group.type == "panels") {
        
        if (is.null(color)) {
          
          p <- p + geom(alpha = 1 - transparency, color = blue)
          if (type == "both") p <- p + geom_point(alpha = 1 - transparency, color = blue)
          
        } else {
          p <- p + geom(color = color, alpha = 1 - transparency)
          if (type == "both") p <- p + geom_point(color = color, alpha = 1 - transparency)
        }
        
      } else if (y.mat) {
        
        # print(y.mat)
        if (is.null(color)) {
          
          p <- p + geom(aes(group = Group), alpha = 1 - transparency, color = blue) +
            theme(legend.position = "none")
          if (type == "both") p <- p + geom_point(aes(group = Group), alpha = 1 - transparency, color = blue)
          
        } else {
          
          p <- p + geom(aes(group = Group), alpha = 1 - transparency, color = color) +
            theme(legend.position = "none")
          if (type == "both") p <- p + geom_point(aes(group = Group), alpha = 1 - transparency, color = color)
          
          
        }
        
        
        
      } else if (group.type == "color") {
        
        # print(group.lab)
        p <- p + geom(aes(color = Group), alpha = 1 - transparency) +
          labs(color = group.lab)
        if (type == "both") p <- p + geom_point(aes(color = Group), alpha = 1 - transparency)
        
        if (!is.null(color)) p <- p + scale_color_manual(values = color)
        # add.color.scale <- TRUE
      }
      
    } 
    
    
    #FILL----
    if (is.element(type, "area")) {
      
      if (!group | group.type == "panels") {
        
        if (is.null(color)) {
          p <- p + geom(alpha = 1 - transparency, fill = blue, color = edge.color)
        } else {
          p <- p + geom(fill = color, alpha = 1 - transparency)
        }
        
      } else if (group.type == "color") {
        
        p <- p + geom(aes(fill = Group), alpha = 1 - transparency, color = edge.color)
        
        if (!is.null(color)) p <- p + scale_fill_manual(values = color)
        # add.color.scale <- TRUE
      }
      
    } 
    
    
  }
  
  
  if (group & group.type == "panels") {
    
    p <- p + facet_wrap(vars(Group))
    
  }
  
  
  # return(d)
  
  if (is.null(group.lab)) p <- p + theme(legend.title = element_blank())
  # if (!is.null(group.lab)) p <- p + theme(legend.title = element_blank())
  if (!is.null(xlab)) p <- p + xlab(xlab)
  if (!is.null(ylab)) p <- p + ylab(ylab)
  
  # if (!is.null(xlim)) p <- p + xlim(xlim)
  # if (!is.null(ylim)) p <- p + ylim(ylim)
  
  # if (!is.null(xlim)) xlim <- range(d$X, na.rm = TRUE)
  # 
  # if (!is.null(ylim)) ylim <- range(d$Y, na.rm = TRUE)
  
  
  
  if (is.element("x", log)) {
    p <- p + scale_x_log10(labels = num.format.x, limits = xlim)
  } else if (is.Date(d$X) | is.POSIXt(d$X)) { 
    p <- p 
    
  } else {
    p <- p + scale_x_continuous(labels = num.format.x, limits = xlim)
  }
  
  if (is.element("y", log)) {
    p <- p + scale_y_log10(labels = num.format.y, limits = ylim)
  } else {
    p <- p + scale_y_continuous(labels = num.format.y, limits = ylim)
  }
  
  if (!is.null(title)) p <- p + ggtitle(title)
  
  # print(p)
  
  
  if (give.data) {
    return(d)
  } else {
    print(p)
  }
  
}


weplot.Pop <- function(x = NULL, y = NULL, type = "both",
                       xlab = "Time", ylab = NULL, ...){
  
  #gets input arguments
  args = as.list(match.call()[-1])
  arg.names <- names(args) 
  args <- as.character(args)
  
  
  #single argument x = N
  if (is.null(y)){
    X <- 1:ncol(x) - 1
    y <- x
    
    if(is.null(ylab)) ylab <- args[arg.names == "x"]
    # message("no y")
    
  } else {
    X <- x
    if(is.null(ylab)) ylab <- args[arg.names == "y"]
    
  }
  
  
  
  Y <- list()
  for (i in 1:nrow(y)) Y[[i]] <- y[i,]
  if (!is.null(rownames(y))) names(Y) <- rownames(y)
  
  # message(str(Y,1))
  # message(str(X,1))
  # new.weplot <- weplot
  weplot.Pop.Y <<- Y
  weplot.Pop.X  <<- X
  
  weplot(x = weplot.Pop.X, y = weplot.Pop.Y, type = type, xlab = xlab, ylab = ylab, ...)
  
  # return(Y)
  rm("weplot.Pop.X", "weplot.Pop.Y", envir = .GlobalEnv)
  
} 



message("-- weplot loaded (version 1.11) --")






