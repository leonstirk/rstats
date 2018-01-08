#!/usr/bin/env Rscript

filepath="/home/ubuntu/rstats/datasets/repairIDs2.dat"

con = file(filepath, "r")
while ( TRUE ) {
 line = readLines(con, n = 1)
 if ( length(line) == 0 ) {
  break
 }
 qldc$remove(line)
}

close(con)