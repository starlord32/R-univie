

if(!("ch00_unzip_check_packages.r" %in% dir()))
  stop("These scripts were meant to be run with the",
       "working directory set to the same location as",
       "scripts")

x = require("abind") &
    require("colorspace") &
    require("coreNLP") &
    require("igraph") &
    require("jpeg") &
    require("mallet") &
    require("maptools") &
    require("RColorBrewer") &
    require("rgdal") &
    require("rgeos") &
    require("snippets") &
    require("sp")

if (x)
  print("All required packages available!")

if (!x)
  print("Some packages need to be installed (though you may do these chapter by chapter)")
