#!/usr/bin/env Rscript

suppressMessages( library( library('ggplot2')))
suppressMessages( library(library('optparse')))

option_list = list(

  make_option( c( "--input","-i" ), type="character", default=NULL,
               help="", metavar="character" ),

  make_option( c( "--output", "-o" ), type="character", default=NULL,
               help="", metavar="character" ),

  make_option( c( "--col_x" ), type="character", default=NULL,
               help="which column to use for X axis.", metavar="character" ),

  make_option( c( "--col_y" ), type="character", default=NULL,
               help="which column to use for Y axis.", metavar="character" ),

  make_option( c( "--geom_boxplot"), type="character", default=NULL,
               help="Add a geom_boxplot layer", metavar="character" ),

  make_option( c( "--geom_boxplot_outlier_shape"), type="character", default=NULL,
               help="", metavar="character" )
)

opt_parser = OptionParser( description= "ggplot2 for galaxy", option_list=option_list );
opt = parse_args( opt_parser );

if ( is.null( opt$input ) ) {
  print_help( opt_parser )
  stop( "An input file must be supplied", call.=FALSE )
}

if ( is.null( opt$output ) ) {
  print_help( opt_parser )
  stop( "An output prefix must be supplied", call.=FALSE )
}

input_data <- read.table(opt$input, header=T)

p <- ggplot(data = input_data, aes(x = opt$col_x, y = opt$col_y))

if (is.null(opt$geom_boxplot)==FALSE){
  p + geom_boxplot()
}

if (is.null(opt$geom_violin)==FALSE){
  p + geom_violin()
}

pdf(opt$output)
p
dev.off()
