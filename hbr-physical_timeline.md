# 90 Years with Harvard Business Review

I'm working on the Harvard Business Review data set (a [kaggle](http://www.kaggle.com/) [thing](https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/)). I want to make the timeline more physically intuitive and more closely linked to the pages of HBR. I'm using R with knitr (R markdown) and showing all my work, with some use of stopifnot() and commenting some things out.


```r
# set up graphics for later
suppressPackageStartupMessages(library(ggplot2))
theme_set(theme_bw())

# Read in the provided file, available at the kaggle site
# https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/data
hbr <- read.csv('HBR Citations_correct_abstracts.csv',
              strip.white=TRUE,
              as.is=TRUE)
# This was working fine for a while and then something went wrong somehow with
# some unicode characters or something, so I had to do this on the command line:
# LC_CTYPE=C tr -c -d "[:alnum:][:punct:][:space:]" \
# < 'HBR Citations_correct_abstracts.csv' > hbr.csv
# cp 'HBR Citations_correct_abstracts.csv' original.csv
# mv hbr.csv 'HBR Citations_correct_abstracts.csv'
# Your mileage may vary.

# make sure we've got the expected number of observations
# as checked against the Excel files that were originally provided
stopifnot(nrow(hbr)==12751)

# confirm that there's always at most one of
# ABSTRACT or AUTHOR.SUPPLIED.ABSTRACT
stopifnot(all(hbr$ABSTRACT=="" | hbr$AUTHOR.SUPPLIED.ABSTRACT==""))
# and put it in a more convenient place
hbr$abstract <- ifelse(hbr$ABSTRACT!="",hbr$ABSTRACT,hbr$AUTHOR.SUPPLIED.ABSTRACT)
# preserve information just in case
hbr$abstract_type <- ifelse(hbr$ABSTRACT!="","HBR",
                            ifelse(hbr$AUTHOR.SUPPLIED.ABSTRACT!="","author",
                                   "none"))

# fix up the dates
# without this you incorrectly get results like year 2068, etc...
# the substringing only works because all the dates are 8 character
stopifnot(all(nchar(hbr$SYSTEM..PUB.DATE)==8))
hbr$dm <- substr(hbr$SYSTEM..PUB.DATE,1,6)
hbr$y <- substr(hbr$SYSTEM..PUB.DATE,7,8)
hbr$dmY <- ifelse(as.numeric(hbr$y)>20,
                  paste(hbr$dm,"19",hbr$y,sep=""),
                  paste(hbr$dm,"20",hbr$y,sep=""))
hbr$date <- as.Date(hbr$dmY,format="%d-%b-%Y")
hbr$year <- as.numeric(substr(hbr$date,1,4))
```


The first issue seems to have been published `1922-10-01`. Everyone seems to agree on the year, at least. The [HBR wikipedia page](http://en.wikipedia.org/wiki/Harvard_Business_Review) adds this interesting tidbit:
> Harvard Business Review began in 1922 as a magazine for Harvard Business School. Founded under the auspices of Dean Wallace Donham, HBR was meant to be more than just a typical school publication. "The paper [HBR] is intended to be the highest type of business journal that we can make it, and for use by the student and the business man. It is not a school paper," Donham wrote.


```r
# columns 'VOLUME', 'ISSUE', and 'PUBLICATION.DATE', which has something
# like volume names, are all moderately interesting, but I'll be mostly
# concerned with publication dates as pulled out above

# How often is HBR published?
years <- sort(unique(hbr$year))
dpy <- data.frame(year = years, peryear = sapply(years, function(x) {
    length(unique(hbr$date[hbr$year == x]))
}))
qplot(year, peryear, data = dpy, main = "publication dates per year")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


There's some fluctuation from special supplements and just irregularities, but broadly it was quartlerly, then bi-monthly, then close to monthly (10, 11, or 12 times per year). I'll pull out the years that start the second and third epochs.


```r
# start of bi-monthly epoch
head(dpy[dpy$peryear == 6, ], 1)
```

```
##    year peryear
## 27 1948       6
```

```r

# start of near-monthly epoch
head(dpy[dpy$peryear == 11, ], 1)
```

```
##    year peryear
## 80 2001      11
```

```r

# let's look at page counts and word counts
hbr$pagesN <- hbr$PAGE.COUNT
# maybe it would be better to use 'START.PAGE' and 'END.PAGE' I don't
# think it's worth checking into that Let's look at page counts through
# time
pty <- data.frame(year = years, pagesthrough = sapply(years, function(x) {
    sum(hbr$pagesN[hbr$year <= x], na.rm = TRUE)
}))
ggplot(pty, aes(year, pagesthrough)) + geom_area() + opts(title = "cumulative pages") + 
    geom_vline(xintercept = c(1948, 2001), colour = I("red")) + geom_text(x = 1931, 
    y = 50000, label = paste(round(sum(hbr$pagesN[hbr$year < 1948 & hbr$year > 
        1922], na.rm = TRUE)/(1948 - 1922)), "pages/year", sep = "\n"), colour = I("blue"), 
    family = "mono") + scale_x_continuous(limits = c(1920, 2020)) + geom_text(x = 1973, 
    y = 50000, label = paste(round(sum(hbr$pagesN[hbr$year < 2001 & hbr$year >= 
        1948], na.rm = TRUE)/(2001 - 1948)), "pages/year", sep = "\n"), colour = I("blue"), 
    family = "mono") + geom_text(x = 2012, y = 50000, label = paste(round(sum(hbr$pagesN[hbr$year < 
    2012 & hbr$year >= 2001], na.rm = TRUE)/(2012 - 2001)), "pages/year", sep = "\n"), 
    colour = I("blue"), family = "mono")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


It looks like the changes in publishing frequency were not offset by too much thinning of individual issues.


```r
# too much in one line
with(hbr, plot(sort(unique(date)), tapply(pagesN, date, sum, na.rm = TRUE)[order(names(tapply(pagesN, 
    date, sum, na.rm = TRUE)))], xlim = as.Date(c("1920-01-01", "2020-01-01")), 
    xlab = "year", ylab = "pages", main = "pages per publication date"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
with(hbr, plot(sort(unique(year)), tapply(pagesN, year, sum, na.rm = TRUE)[order(names(tapply(pagesN, 
    year, sum, na.rm = TRUE)))], xlim = c(1920, 2020), xlab = "year", ylab = "pages", 
    main = "pages per publication year"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 


Well, it looks like there was some drop in issue thickness when HBR went to the near-monthly frequency in 2001. But doubling (or nearly so) the frequency gave the highest ever yearly page counts. Here's how to space out the year labels in the combined anthology of all HBR ever, which is how to space out my timeline weighted by HBR commentary.


```r
pty$mark <- c(0, pty$pagesthrough/max(pty$pagesthrough))[-(nrow(pty) + 
    1)]
print(pty)
```

```
##    year pagesthrough     mark
## 1  1922          130 0.000000
## 2  1923          679 0.001777
## 3  1924         1224 0.009282
## 4  1925         1743 0.016732
## 5  1926         2289 0.023826
## 6  1927         2853 0.031290
## 7  1928         3410 0.039000
## 8  1929         3951 0.046614
## 9  1930         4482 0.054009
## 10 1931         5006 0.061268
## 11 1932         5541 0.068431
## 12 1933         6108 0.075744
## 13 1934         6639 0.083495
## 14 1935         7181 0.090754
## 15 1936         7711 0.098163
## 16 1937         8226 0.105408
## 17 1938         8760 0.112448
## 18 1939         9306 0.119747
## 19 1940         9839 0.127211
## 20 1941        10358 0.134497
## 21 1942        10880 0.141592
## 22 1943        11466 0.148727
## 23 1944        11986 0.156738
## 24 1945        12518 0.163846
## 25 1946        13064 0.171118
## 26 1947        13581 0.178582
## 27 1948        14351 0.185649
## 28 1949        15149 0.196175
## 29 1950        15854 0.207084
## 30 1951        16573 0.216721
## 31 1952        17290 0.226549
## 32 1953        18019 0.236351
## 33 1954        18765 0.246316
## 34 1955        19524 0.256514
## 35 1956        20255 0.266889
## 36 1957        21029 0.276882
## 37 1958        21812 0.287462
## 38 1959        22597 0.298166
## 39 1960        23460 0.308896
## 40 1961        24304 0.320693
## 41 1962        25177 0.332231
## 42 1963        26055 0.344164
## 43 1964        26928 0.356166
## 44 1965        27778 0.368100
## 45 1966        28617 0.379719
## 46 1967        29500 0.391188
## 47 1968        30352 0.403259
## 48 1969        31200 0.414906
## 49 1970        32068 0.426498
## 50 1971        32907 0.438363
## 51 1972        33716 0.449832
## 52 1973        34531 0.460891
## 53 1974        35324 0.472032
## 54 1975        36147 0.482872
## 55 1976        36978 0.494122
## 56 1977        37822 0.505482
## 57 1978        38695 0.517019
## 58 1979        39644 0.528953
## 59 1980        40568 0.541925
## 60 1981        41590 0.554556
## 61 1982        42440 0.568527
## 62 1983        43326 0.580146
## 63 1984        44311 0.592257
## 64 1985        45339 0.605722
## 65 1986        46168 0.619775
## 66 1987        46987 0.631107
## 67 1988        47778 0.642303
## 68 1989        48815 0.653115
## 69 1990        49935 0.667291
## 70 1991        50942 0.682601
## 71 1992        51850 0.696367
## 72 1993        52769 0.708779
## 73 1994        53692 0.721341
## 74 1995        54645 0.733958
## 75 1996        55589 0.746986
## 76 1997        56470 0.759890
## 77 1998        57299 0.771933
## 78 1999        58069 0.783265
## 79 2000        58862 0.793791
## 80 2001        60123 0.804631
## 81 2002        61296 0.821869
## 82 2003        62569 0.837904
## 83 2004        63956 0.855305
## 84 2005        65406 0.874265
## 85 2006        66883 0.894086
## 86 2007        68285 0.914277
## 87 2008        69560 0.933442
## 88 2009        70759 0.950871
## 89 2010        71898 0.967261
## 90 2011        73028 0.982831
## 91 2012        73154 0.998278
```


I would like to get a total word count.


```r
hbr$wordsN <- hbr$FULL.TEXT.WORD.COUNT
```


There are `12751` entries total. Of these, `12549` have a page count, but only `5654` have a word count. (They started counting in `1990`.) All but `13` of the entries with word counts have page counts. Based on those entries, there's an average of `541.4908` words per page. At least [some people online](http://www.writersservices.com/wps/p_word_count.htm) think that's reasonable.

> At one extreme you get large print books with 250 words on the page. Academic books might put 600 words on a page with works of reference squeezing in 1000 words.

So we can get some sort of estimate of the total words ever published, adding in the word counts for the `13` entries with word count but no page count, and giving the `189` entries with neither page count nor word count the average word count per entry with both of `2210.3115`.


```r
# calculate an estimated total word count for the history of HBR (not very
# readable...)
with(hbr[!is.na(hbr$pagesN) & !is.na(hbr$wordsN), ], sum(wordsN)/sum(pagesN)) * 
    with(hbr[!is.na(hbr$pagesN), ], sum(pagesN)) + with(hbr[is.na(hbr$pagesN) & 
    !is.na(hbr$wordsN), ], sum(wordsN)) + with(hbr[!is.na(hbr$pagesN) & !is.na(hbr$wordsN), 
    ], sum(wordsN)/length(pagesN)) * nrow(hbr[is.na(hbr$pagesN) & is.na(hbr$wordsN), 
    ])
```

```
## [1] 40085102
```


I'm prepared to call that forty million words. It might be possible to do better using the "DOCUMENT.TYPE" column, but perhaps not much better. (There are four "Image" entries; three of them have word counts, all four have page counts.) Anyway, `0.9842` of entries have page counts, for a total of `73154` pages. I'll use page count as a estimate of quantity of content.

Here's some evidence that 


```r




# there are too many columns that I don't care about separately there has
# GOT to be a better way to do this:
hbr$naics <- ""
for (j in grep("DESC", names(hbr))) {
    hbr$naics <- paste(hbr$naics, hbr[[j]])
}
hbr$subjects <- ""
for (j in grep("TERM", names(hbr))) {
    hbr$subjects <- paste(hbr$subjects, hbr[[j]])
}
hbr$keywords <- ""
for (j in grep("KEYWORD", names(hbr))) {
    hbr$keywords <- paste(hbr$keywords, hbr[[j]])
}
hbr$affils <- ""
for (j in grep("AFFIL", names(hbr))) {
    hbr$affils <- paste(hbr$affils, hbr[[j]])
}
# not sure how/if I want to completely deal with names yet this just does
# most frequent authors and adds a count of authors per article thing
first_names <- c()
last_names <- c()
names <- c()
affils <- c()
hbr$authorsN <- 0
for (i in 1:20) {
    last <- paste("AUTHOR.", i, ".LAST.NAME", sep = "")
    last_names <- c(last_names, hbr[[last]])
    first <- paste("AUTHOR.", i, ".FIRST.NAME", sep = "")
    first_names <- c(first_names, hbr[[first]])
    affil <- paste("AUTHOR.", i, ".AFFILIATION", sep = "")
    affils <- c(affils, hbr[[affil]])
    # if there's anything, that's an author
    hbr$authorsN <- hbr$authorsN + ifelse(hbr[[last]] == "" & hbr[[first]] == 
        "" & hbr[[affil]] == "", 0, 1)
}
names <- paste(first_names, last_names)
# meet the most prolific HBR authors tail(sort(table(names)),20)

# authors per article over time
with(hbr, smoothScatter(date, authorsN))
```

```
## KernSmooth 2.23 loaded Copyright M. P. Wand 1997-2009
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

```r

# look at more stuff about number authors
with(hbr, tapply(authorsN, DOCUMENT.TYPE, mean, na.rm = T))
```

```
##             Abstract              Article         Bibliography 
##               1.3547               1.2902               0.4000 
##            Biography         Book Chapter          Book Review 
##               1.3333               1.0000               0.7498 
##           Case Study        Course Review            Editorial 
##               2.1126               0.0000               0.8110 
## Entertainment Review              Erratum                Essay 
##               1.0000               0.1132               1.0000 
##              Excerpt                Image            Interview 
##               1.0638               0.5000               1.0478 
##               Letter             Obituary              Opinion 
##               2.1756               0.7500               0.9500 
##                Other                 Poem           Proceeding 
##               0.1538               1.0000               0.9412 
##       Product Review          Short Story               Speech 
##               1.6667               1.7500               1.0000
```

```r




# could be useful
Sys.setenv(NOAWT = TRUE)
library(tm)
data("crude")
crude[[1]]
```

```
## Diamond Shamrock Corp said that
## effective today it had cut its contract prices for crude oil by
## 1.50 dlrs a barrel.
##     The reduction brings its posted price for West Texas
## Intermediate to 16.00 dlrs a barrel, the copany said.
##     "The price reduction today was made in the light of falling
## oil product prices and a weak crude oil market," a company
## spokeswoman said.
##     Diamond is the latest in a line of U.S. oil companies that
## have cut its contract, or posted, prices over the last two days
## citing weak oil markets.
##  Reuter
```

```r
stemDocument(crude[[1]])
```

```
## Diamond Shamrock Corp said that
## effect today it had cut it contract price for crude oil by
## 1.50 dlrs a barrel.
##     The reduct bring it post price for West Texas
## Intermedi to 16.00 dlrs a barrel, the copani said.
##     "The price reduct today was made in the light of falling
## oil product price and a weak crude oil market," a company
## spokeswoman said.
##     Diamond is the latest in a line of U.S. oil compani that
## hav cut it contract, or posted, price over the last two days
## cit weak oil markets.
##  Reuter
```

```r
# and these are all vectorized
temp <- removeNumbers(crude[[1]])
temp <- removePunctuation(temp)
temp <- tolower(temp)
temp <- gsub("\n", " ", temp)
stemDocument(temp)
```

```
## diamond shamrock corp said that effect today it had cut it contract price for crude oil by  dlrs a barrel     the reduct bring it post price for west texa intermedi to  dlrs a barrel the copani said     the price reduct today was made in the light of fall oil product price and a weak crude oil market a compani spokeswoman said     diamond is the latest in a line of us oil compani that have cut it contract or post price over the last two day cite weak oil market  reuter
```

