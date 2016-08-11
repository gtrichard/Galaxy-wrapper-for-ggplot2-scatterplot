# Setup R error handling to go to stderr
options(show.error.messages=F, error=function(){cat(geterrmessage(),file=stderr());q("no",1,F)})

# We need to not crash galaxy with an UTF8 error on German LC settings.
loc <- Sys.setlocale("LC_MESSAGES", "en_US.UTF-8")


# Import library
library("getopt")
library("ggplot2")
options(stringAsfactors = FALSE, useFancyQuotes = FALSE)
# Take in trailing command line arguments
args <- commandArgs(trailingOnly = TRUE)


# get options, using the spec as defined by the enclosed list.
# we read the options from the default: commandArgs(TRUE).
option_specification = matrix(c(
  'input', 'i', 2, 'character',
  'title', 't',2, 'character',
  'size', 's', 2, 'double',
  'xlab', 'x', 2, 'character',
  'ylab', 'y', 2, 'character',
  'xplot', 'z', 2, 'integer',
  'yplot', 'j', 2, 'integer',
  'xaxismin', 'e', 2, 'integer',
  'xaxismax', 'f', 2, 'integer',
  'yaxismin', 'g', 2, 'integer',
  'yaxismax', 'h', 2, 'integer',
  'alpha', 'a', 2, 'double',
  'points', 'p', 2, 'character',
  'theme', 'l', 2, 'character',
  'scaling', 'b', 2, 'character',
  'transform', 'w', 2, 'character',
  'dim', 'k', 2, 'character',
  'woutputdim', 'c', 2, 'integer',
  'houtputdim', 'd', 2, 'integer',
  'factor', 'n', 2, 'character',
  'factorcol', 'm', 2, 'integer',
  'colors', 'q', 2, 'character', 
  'colororder', 'r', 2, 'integer', 
  'pointcolor', 'u', 2, 'character', 
  'output', 'o', 2, 'character'
  ), byrow=TRUE, ncol=4);

# Parse options
options = getopt(option_specification);



# Print options to see what is going on
cat("\n input: ",options$input)
cat("\n title: ",options$title)
cat("\n xlab: ",options$xlab)
cat("\n ylab: ",options$ylab)
cat("\n points: ",options$points)
cat("\n theme: ",options$theme)
cat("\n scaling: ",options$scaling)
cat("\n transform: ",options$transform)



#Choose between automatically scaled x and y axis or user defined
if(options$scaling == "Automatic"){
    gg_scalex = NULL
    gg_scaley = NULL
} else {
    gg_scalex = xlim(options$xaxismin,options$xaxismax)
    gg_scaley = ylim(options$yaxismin,options$yaxismax)
    cat("\n xaxismin: ",options$xaxismin)
    cat("\n xaxismax: ",options$xaxismax)
    cat("\n yaxismin: ",options$yaxismin)
    cat("\n yaxismax: ",options$yaxismax)
}

# Choose theme for plot
if(options$theme == "bw"){
    gg_theme = theme_bw()
} else {
    gg_theme = NULL
}

#Choose dimensions of output pdf
if(options$dim == "Default"){
    gg_width = 7
    gg_height = 7
} else {
    gg_width = options$woutputdim
    gg_height = options$houtputdim 
}

input <-read.table(options$input, check.names=TRUE, header=TRUE, row.names=NULL)

#renaming columns so ggplot can use them
names(input)[options$xplot] <- "xcol"
names(input)[options$yplot] <- "ycol"

#choosing whether to plot data as multiple groups on one plot(factoring) OR multiple groups on different plots
if(options$factor == "Multiple"){
     gg_facet = facet_wrap( ~ factor)
     gg_factor = NULL
     color_scale = NULL
          
          if(options$points == "Default"){
            gg_point = geom_point(size=1, alpha=1, gg_factor)
        } else {
            gg_point = geom_point(size=options$size, alpha=options$alpha, colour=options$pointcolor)
            cat("\n size: ",options$size)
            cat("\n alpha: ",options$alpha)
            cat("\n pointcolor: ",options$pointcolor)
        }

    names(input)[options$factorcol] <- "factor"
   
    cat("\n factor: ",options$factor)
    cat("\n factorcol: ",options$factorcol)
} else if(options$factor == "Single"){
    gg_facet = NULL
    gg_factor = aes(colour=factor(factor))

          if(options$points == "Default"){
            gg_point = geom_point(size=1, alpha=1, gg_factor)
        } else {
            gg_point = geom_point(size=options$size, alpha=options$alpha, gg_factor)
            cat("\n size: ",options$size)
            cat("\n alpha: ",options$alpha)
            cat("\n pointcolor: ",options$pointcolor)
        }

      if(options$colors == "Default"){
          color_scale = scale_colour_hue(direction=options$colororder)
      } else {
          color_scale = scale_color_brewer(palette=options$colors, direction=options$colororder)
      }

    names(input)[options$factorcol] <- "factor"
    
    cat("\n factor: ",options$factor)
    cat("\n factorcol: ",options$factorcol)
    cat("\n color_scale: ",options$colors)
    cat("\n color_order: ",options$colororder)
} else{
      gg_facet = NULL
      gg_factor = NULL
      color_scale = NULL

        if(options$points == "Default"){
            gg_point = geom_point(size=1, alpha=1, gg_factor)
        } else {
            gg_point = geom_point(size=options$size, alpha=options$alpha, colour=options$pointcolor)
            cat("\n size: ",options$size)
            cat("\n alpha: ",options$alpha)
            cat("\n pointcolor: ",options$pointcolor)
        }    
}


#Selecting whether or not to log transform data
if(options$transform == "log2"){
    input[, options$xplot] <- log2(input[options$xplot])
    input[, options$yplot] <- log2(input[options$yplot])
}else if(options$transform == "log2plus1"){
    input[, options$xplot] <- log2(input[options$xplot]+1)
    input[, options$yplot] <- log2(input[options$yplot]+1)
}else if(options$transform == "log10"){
    input[, options$xplot] <- log10(input[options$xplot])
    input[, options$yplot] <- log10(input[options$yplot])
}else if(options$transform == "log10plus1"){
    input[, options$xplot] <- log10(input[options$xplot]+1)
    input[, options$yplot] <- log10(input[options$yplot]+1)
    }else{
}


#this is the actual ggplot command to make the final plot(s)
ggplot(input, aes(xcol,ycol))+gg_point+gg_facet+
gg_theme+gg_scalex+gg_scaley+color_scale+ggtitle(options$title)+xlab(options$xlab)+ylab(options$ylab)

ggsave(file=options$output, width=gg_width, height=gg_height)
