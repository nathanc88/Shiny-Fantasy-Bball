
# Read Data ----
b        <- readRDS("data/b.rds")        ## Player Bios
corr     <- readRDS( "data/corr.rds")    ## Correlation dataframes
d        <- readRDS( "data/d.rds")       ## raw game logs
d2       <- readRDS("data/d2.rds")       ## cleaned game logs
f        <- readRDS("data/f.rds")        ## new draft offers
half1    <- readRDS("data/half1.rds")    ## raw first half numbers
half2    <- readRDS("data/half2.rds")    ## raw second half numbers
p        <- readRDS("data/p.rds")        ## raw fantasy draft numbers
p2       <- readRDS("data/p2.rds")       ## clean fantasy draft numbers
pct      <- readRDS("data/pct.rds")      ## percentile numbers
pct30    <- readRDS("data/pct30.rds")    ## per30 percentile numbers
pctHalf1 <- readRDS("data/pctHalf1.rds") ## first half percentile numbers
pctHalf2 <- readRDS("data/pctHalf2.rds") ## second half percentile numbers
per      <- readRDS("data/per.rds")      ## per game numbers
per30    <- readRDS("data/per30.rds")    ## per30 numbers
progress <- readRDS("data/progress.rds") ## in season progress
s        <- readRDS("data/s.rds")        ## raw depth charts
s2       <- readRDS("data/s2.rds")       ## clean depth charts
z        <- readRDS("data/z.rds")        ## z scores