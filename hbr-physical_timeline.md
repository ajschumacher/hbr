# 90 Years with Harvard Business Review

I'm working on the Harvard Business Review data set (a [kaggle](http://www.kaggle.com/) [thing](https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/)). I want to make the timeline more physically intuitive and more closely linked to the pages of HBR. I'm using R with knitr (R markdown) and showing all my work, with some use of stopifnot() and commenting some things out.


```r
# set up graphics for later
library(ggplot2)
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
# think it's worth checking into that; but I do want word count
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

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

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

