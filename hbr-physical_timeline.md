





# 90 Years of Harvard Business Review

> Bare lists of words are found suggestive to an imaginative and excited mind.
-Ralph Waldo Emerson, writer and philosopher (1803-1882) (thanks to [a.w.a.d](http://wordsmith.org/awad/))

I'm working on the Harvard Business Review data set (a [kaggle](http://www.kaggle.com/) [thing](https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/)). I want to make the timeline more physically intuitive and more closely linked to the pages of HBR. I'm using R with knitr (R markdown) to show my work. I'm [Aaron Schumacher](mailto:ajschumacher@gmail.com) and I welcome any comments, corrections, suggestions, etc.


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
# That filtering only took out two bytes. Not an issue.
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
# columns "VOLUME", "ISSUE", and "PUBLICATION.DATE", which has something like
# volume names, are all moderately interesting,
# but I'll be mostly concerned with publication dates as pulled out above

# How often is HBR published?
years <- sort(unique(hbr$year))
dpy <- data.frame("year"=years,
                  "peryear"=sapply(years,function(x) {
                    length(unique(hbr$date[hbr$year==x]))
                    }))
qplot(year, peryear, data=dpy, main="publication dates per year")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


There's some fluctuation from special supplements and just irregularities, but broadly it was quartlerly, then bi-monthly, then close to monthly (10, 11, or 12 times per year). I'll pull out the years that start the second and third epochs.


```r
# start of bi-monthly epoch
head(dpy[dpy$peryear==6,],1)
```

```
##    year peryear
## 27 1948       6
```

```r

# start of near-monthly epoch
head(dpy[dpy$peryear==11,],1)
```

```
##    year peryear
## 80 2001      11
```

```r

# let's look at page counts and word counts
hbr$pagesN <- hbr$PAGE.COUNT
# maybe it would be better to use "START.PAGE" and "END.PAGE"
# I don't think it's worth checking into that
# Let's look at page counts through time
pty <- data.frame("year"=years,
                  "pagesthrough"=sapply(years,function(x) {
                    sum(hbr$pagesN[hbr$year<=x], na.rm=TRUE)
                    }))
ggplot(pty, aes(year, pagesthrough)) + 
  geom_area() + 
  opts(title="cumulative pages") + 
  geom_vline(xintercept=c(1948,2001), colour=I("red")) + 
  geom_text(x=1931, y=50000, 
            label=paste(round(sum(hbr$pagesN[hbr$year<1948&hbr$year>1922],na.rm=TRUE)/
              (1948-1922)),"pages/year",sep="\n"),
            colour=I("blue"), family="mono") + 
  scale_x_continuous(limits=c(1920,2020)) + 
  geom_text(x=1973, y=50000, 
            label=paste(round(sum(hbr$pagesN[hbr$year<2001&hbr$year>=1948],na.rm=TRUE)/
              (2001-1948)), "pages/year",sep="\n"),
            colour=I("blue"), family="mono") +
  geom_text(x=2012, y=50000,
            label=paste(round(sum(hbr$pagesN[hbr$year<2012&hbr$year>=2001],na.rm=TRUE)/
              (2012-2001)), "pages/year",sep="\n"),
            colour=I("blue"), family="mono")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 


It looks like the changes in publishing frequency were not offset by too much thinning of individual issues. (I won't try to dig into words per year.)


```r
# ugly, but it works; look at the output
with(hbr, plot(sort(unique(date)),tapply(pagesN,date,sum,na.rm=TRUE)[order(names(tapply(pagesN,date,sum,na.rm=TRUE)))],xlim=as.Date(c("1920-01-01","2020-01-01")),xlab="year",ylab="pages",main="pages per publication date"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-41.png) 

```r
with(hbr, plot(sort(unique(year)),tapply(pagesN,year,sum,na.rm=TRUE)[order(names(tapply(pagesN,year,sum,na.rm=TRUE)))],xlim=c(1920,2020),xlab="year",ylab="pages",main="pages per publication year"))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-42.png) 


Well, it looks like there was some drop in issue thickness when HBR went to the near-monthly frequency in 2001. But doubling (or nearly so) the frequency gave the highest ever yearly page counts. Here's how to space out the year labels in the combined anthology of all HBR ever, which is how to space out my timeline weighted by HBR commentary.


```r
pty$mark <- c(0, pty$pagesthrough / max(pty$pagesthrough))[-(nrow(pty)+1)]
print(pty,row.names=FALSE)
```

```
##  year pagesthrough     mark
##  1922          130 0.000000
##  1923          679 0.001777
##  1924         1224 0.009282
##  1925         1743 0.016732
##  1926         2289 0.023826
##  1927         2853 0.031290
##  1928         3410 0.039000
##  1929         3951 0.046614
##  1930         4482 0.054009
##  1931         5006 0.061268
##  1932         5541 0.068431
##  1933         6108 0.075744
##  1934         6639 0.083495
##  1935         7181 0.090754
##  1936         7711 0.098163
##  1937         8226 0.105408
##  1938         8760 0.112448
##  1939         9306 0.119747
##  1940         9839 0.127211
##  1941        10358 0.134497
##  1942        10880 0.141592
##  1943        11466 0.148727
##  1944        11986 0.156738
##  1945        12518 0.163846
##  1946        13064 0.171118
##  1947        13581 0.178582
##  1948        14351 0.185649
##  1949        15149 0.196175
##  1950        15854 0.207084
##  1951        16573 0.216721
##  1952        17290 0.226549
##  1953        18019 0.236351
##  1954        18765 0.246316
##  1955        19524 0.256514
##  1956        20255 0.266889
##  1957        21029 0.276882
##  1958        21812 0.287462
##  1959        22597 0.298166
##  1960        23460 0.308896
##  1961        24304 0.320693
##  1962        25177 0.332231
##  1963        26055 0.344164
##  1964        26928 0.356166
##  1965        27778 0.368100
##  1966        28617 0.379719
##  1967        29500 0.391188
##  1968        30352 0.403259
##  1969        31200 0.414906
##  1970        32068 0.426498
##  1971        32907 0.438363
##  1972        33716 0.449832
##  1973        34531 0.460891
##  1974        35324 0.472032
##  1975        36147 0.482872
##  1976        36978 0.494122
##  1977        37822 0.505482
##  1978        38695 0.517019
##  1979        39644 0.528953
##  1980        40568 0.541925
##  1981        41590 0.554556
##  1982        42440 0.568527
##  1983        43326 0.580146
##  1984        44311 0.592257
##  1985        45339 0.605722
##  1986        46168 0.619775
##  1987        46987 0.631107
##  1988        47778 0.642303
##  1989        48815 0.653115
##  1990        49935 0.667291
##  1991        50942 0.682601
##  1992        51850 0.696367
##  1993        52769 0.708779
##  1994        53692 0.721341
##  1995        54645 0.733958
##  1996        55589 0.746986
##  1997        56470 0.759890
##  1998        57299 0.771933
##  1999        58069 0.783265
##  2000        58862 0.793791
##  2001        60123 0.804631
##  2002        61296 0.821869
##  2003        62569 0.837904
##  2004        63956 0.855305
##  2005        65406 0.874265
##  2006        66883 0.894086
##  2007        68285 0.914277
##  2008        69560 0.933442
##  2009        70759 0.950871
##  2010        71898 0.967261
##  2011        73028 0.982831
##  2012        73154 0.998278
```


I can't say for sure how many words were on pages before 1990, but I would like to estimate a total word count. Here's an attempt:


```r
hbr$wordsN <- hbr$FULL.TEXT.WORD.COUNT
```


There are `12751` entries total. Of these, `12549` have a page count, but only `5654` have a word count. (They started counting in `1990`.) All but `13` of the entries with word counts have page counts. Based on those entries, there's an average of `541.4908` words per page. At least [some people online](http://www.writersservices.com/wps/p_word_count.htm) think that's reasonable.

> At one extreme you get large print books with 250 words on the page. Academic books might put 600 words on a page with works of reference squeezing in 1000 words.

So we can get some sort of estimate of the total words ever published, adding in the word counts for the `13` entries with word count but no page count, and giving the `189` entries with neither page count nor word count the average word count per entry with both of `2210.3115`.


```r
# calculate an estimated total word count for the history of HBR
# (not very readable...)
with(hbr[!is.na(hbr$pagesN) & !is.na(hbr$wordsN),], 
     sum(wordsN)/sum(pagesN)) * 
       with(hbr[!is.na(hbr$pagesN),], sum(pagesN)) +
       with(hbr[is.na(hbr$pagesN)&!is.na(hbr$wordsN),], sum(wordsN)) +
       with(hbr[!is.na(hbr$pagesN) & !is.na(hbr$wordsN),], 
            sum(wordsN)/length(pagesN)) *
       nrow(hbr[is.na(hbr$pagesN)&is.na(hbr$wordsN),])
```

```
## [1] 40085102
```


I'm prepared to call that forty million words. It might be possible to do better using the "DOCUMENT.TYPE" column, but perhaps not much better. (There are four "Image" entries; three of them have word counts, all four have page counts.) Anyway, `0.9842` of entries have page counts, for a total of `73154` pages. I'll use page count as a estimate of quantity of content.


```r
# There are too many columns that I don't care about separately.
# (there has GOT to be a better way to do this...)
hbr$naics <- ""
for (j in grep("DESC",names(hbr))) {
  hbr$naics <- paste(hbr$naics, hbr[[j]])
}
hbr$subjects <- ""
for (j in grep("TERM",names(hbr))) {
  hbr$subjects <- paste(hbr$subjects, hbr[[j]])
}
hbr$keywords <- ""
for (j in grep("KEYWORD",names(hbr))) {
  hbr$keywords <- paste(hbr$keywords, hbr[[j]])
}
hbr$affils <- ""
for (j in grep("AFFIL",names(hbr))) {
  hbr$affils <- paste(hbr$affils, hbr[[j]])
}
# not sure how/if I want to completely deal with names yet
# this just does most frequent authors
# and adds a count of authors per article thing
first_names <- c()
last_names <- c()
names <- c()
affils <- c()
hbr$authorsN <- 0
for (i in 1:20) {
  last <- paste("AUTHOR.",i,".LAST.NAME",sep="")
  last_names <- c(last_names, hbr[[last]])
  first <- paste("AUTHOR.",i,".FIRST.NAME",sep="")
  first_names <- c(first_names, hbr[[first]])
  affil <- paste("AUTHOR.",i,".AFFILIATION",sep="")
  affils <- c(affils, hbr[[affil]]) 
  # if there's anything, that's an author
  hbr$authorsN <- hbr$authorsN + ifelse(hbr[[last]]=="" &
                                        hbr[[first]]=="" &
                                        hbr[[affil]]=="",
                                        0,1)
}
names <- paste(first_names,last_names)
hbr$authorsN[hbr$authorsN==0] <- NA
# meet the eleven most prolific HBR authors
# people might have different name spellings
# but there's a three-way tie for tenth in my method, so...
tail(sort(table(names)),13)
```

```
## names
##      Anne G. Perkins     David E. Gumpert    Michael E. Porter 
##                   34                   34                   34 
##            Nan Stone       Gardiner Morse    Mary V. Chatfield 
##                   35                   41                   42 
## Rosabeth Moss Kanter    Lorna M. Daniells     Peter F. Drucker 
##                   43                   49                   51 
##            Don Moyer    Thomas A. Stewart       John T. Landry 
##                   63                   79                  255 
##                      
##               237185
```


Okay, I'm almost ready to look at the text data, but first, something about collaboration:


```r
# excuse the numeric dates
with(hbr, smoothScatter(date,authorsN,main="authors per article over time",ylim=c(0,20)))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-91.png) 

```r
with(hbr, plot(sort(unique(year)),
               tapply(authorsN,year,mean,na.rm=T),
               xlab="year", ylab="mean number of authors",
               main="mean authors per article per year"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-92.png) 

```r
# overall trend looks good, but weird outliers are a problem
# especially around the 70's
# What's going on with document types? Check average authors per document by type:
with(hbr, tapply(authorsN,DOCUMENT.TYPE,mean,na.rm=TRUE))
```

```
##             Abstract              Article         Bibliography 
##                7.237                1.418                1.000 
##            Biography         Book Chapter          Book Review 
##                1.333                1.000                1.051 
##           Case Study        Course Review            Editorial 
##                2.615                  NaN                1.073 
## Entertainment Review              Erratum                Essay 
##                1.000                1.200                1.000 
##              Excerpt                Image            Interview 
##                1.183                1.000                1.199 
##               Letter             Obituary              Opinion 
##                2.230                1.000                1.000 
##                Other                 Poem           Proceeding 
##                1.000                1.000                1.143 
##       Product Review          Short Story               Speech 
##                1.667                1.750                1.000
```

```r
# How many of each of these are there?
with(hbr, tapply(authorsN,DOCUMENT.TYPE,length))
```

```
##             Abstract              Article         Bibliography 
##                  203                 8044                   10 
##            Biography         Book Chapter          Book Review 
##                    3                    1                 1067 
##           Case Study        Course Review            Editorial 
##                  302                    1                  127 
## Entertainment Review              Erratum                Essay 
##                    2                   53                    2 
##              Excerpt                Image            Interview 
##                  298                    4                  293 
##               Letter             Obituary              Opinion 
##                 2266                    4                   20 
##                Other                 Poem           Proceeding 
##                   13                    4                   17 
##       Product Review          Short Story               Speech 
##                    3                    4                   10
```

```r
# Letters are one thing, but abstracts are an issue too.
# So, looking at just Articles:
with(hbr[hbr$DOCUMENT.TYPE=="Article",],
     plot(sort(unique(year)),
          tapply(authorsN,year,mean,na.rm=T)))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-93.png) 

```r
# Okay, I like this story. Let's do it by decades defined as 1922-1931 etc.
with(hbr[hbr$DOCUMENT.TYPE=="Article"&hbr$year<2012,],
     barplot(tapply(authorsN,substr(year-2,1,3),mean,na.rm=T),
             ylim=c(1,1.8), main="authors per Article"))
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-94.png) 

```r
# Looks good. Pull the numbers.
with(hbr[hbr$DOCUMENT.TYPE=="Article"&hbr$year<2012,],
     tapply(authorsN,substr(year-2,1,3),mean,na.rm=T))
```

```
##   192   193   194   195   196   197   198   199   200 
## 1.060 1.039 1.154 1.164 1.232 1.368 1.448 1.536 1.738
```


At this point I already have a few interesting stories. Time to look at the text - later.



