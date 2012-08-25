# 90 Years with Harvard Business Review

I'm working on the Harvard Business Review data set (a [kaggle](http://www.kaggle.com/) [thing](https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/)). I want to make the timeline more physically intuitive and more closely linked to the pages of HBR. I'm using R with knitr (R markdown) and showing all my work, with some use of stopifnot() and commenting some things out.


```r
# read in the provided file, available on the kaggle site
# https://www.kaggle.com/c/harvard-business-review-vision-statement-prospect/data
hbr <- read.csv("HBR Citations_correct_abstracts.csv", strip.white = TRUE, 
    as.is = TRUE)

# make sure we've got the expected number of observations as checked
# against the Excel files that were originally provided
stopifnot(nrow(hbr) == 12751)

# confirm that there's always at most one of ABSTRACT or
# AUTHOR.SUPPLIED.ABSTRACT
stopifnot(all(hbr$ABSTRACT == "" | hbr$AUTHOR.SUPPLIED.ABSTRACT == 
    ""))
# and put it in a more convenient place
hbr$abstract <- ifelse(hbr$ABSTRACT != "", hbr$ABSTRACT, hbr$AUTHOR.SUPPLIED.ABSTRACT)
# preserve information just in case
hbr$abstract_type <- ifelse(hbr$ABSTRACT != "", "HBR", ifelse(hbr$AUTHOR.SUPPLIED.ABSTRACT != 
    "", "author", "none"))
hbr$ABSTRACT <- NULL
hbr$AUTHOR.SUPPLIED.ABSTRACT <- NULL

# fix up the dates without this you incorrectly get results like year
# 2068, etc...
hbr$dm <- substr(hbr$SYSTEM..PUB.DATE, 1, 6)
hbr$y <- substr(hbr$SYSTEM..PUB.DATE, 8, 9)
hbr$dmY <- ifelse(as.numeric(hbr$y) > 20, paste(hbr$dm, "-19", hbr$y, 
    sep = ""), paste(hbr$dm, "-20", hbr$y, sep = ""))
hbr$date <- as.Date(hbr$dmY, format = "%d-%b-%Y")
hbr$dm <- NULL
hbr$y <- NULL
hbr$dmY <- NULL
hbr$SYSTEM..PUB.DATE <- NULL


first_names <- c()
last_names <- c()
names <- c()
affils <- c()
hbr$n_authors <- 0
for (i in 1:20) {
    last <- paste("AUTHOR.", i, ".LAST.NAME", sep = "")
    last_names <- c(last_names, hbr[[last]])
    # if there's a last name, that's an author
    hbr$n_authors <- hbr$n_authors + ifelse(hbr[[last]] == "", 0, 1)
    first <- paste("AUTHOR.", i, ".FIRST.NAME", sep = "")
    first_names <- c(first_names, hbr[[first]])
    affil <- paste("AUTHOR.", i, ".AFFILIATION", sep = "")
    affils <- c(affils, hbr[[affil]])
}
names <- paste(first_names, last_names)

# meet the most prolific HBR authors
tail(sort(table(names)), 100)
```

```
## names
##        Robert B. Reich       Sumantra Ghoshal     Sumner H. Slichter 
##                     12                     12                     12 
##       T. George Harris           Tarun Khanna       Thomas V. Bonoma 
##                     12                     12                     12 
##      Wallace B. Donham       Warren G. Bennis               K. R. A. 
##                     12                     12                     13 
##        Alan M. Kantrow           Alison Beard             Dan Ariely 
##                     13                     13                     13 
##     F. Warren McFarlan        Herminia Ibarra        Jeffrey Pfeffer 
##                     13                     13                     13 
##  Katherine Zoe Andrews     Melvin T. Copeland       Nicholas G. Carr 
##                     13                     13                     13 
##        Pankaj Ghemawat   Rita Gunther McGrath        Boris Groysberg 
##                     13                     13                     14 
##       Dan Throop Smith            Diane Coutu    Eliza G. C. Collins 
##                     14                     14                     14 
##        Henry Mintzberg      James Brian Quinn             Ram Charan 
##                     14                     14                     14 
##            W. Chan Kim        Wickham Skinner         John P. Kotter 
##                     14                     14                     15 
##         Katherine Bell          Philip Kotler      Richard F. Vancil 
##                     15                     15                     15 
##     Stephen A. Greyser             Ted Levitt       Abram T. Collier 
##                     15                     15                     16 
##       Alfred Rappaport         Karen B. Tracy     Sylvia Ann Hewlett 
##                     16                     16                     16 
##        Andrew Campbell      Arlyne A. Jackson         Diane L. Coutu 
##                     17                     17                     17 
##       Ian C. MacMillan       John Seely Brown        Joseph L. Bower 
##                     17                     17                     17 
##           Kim B. Clark        Lynn M. Salerno         William Taylor 
##                     17                     17                     17 
##         David Champion        Edward C. Bursk         John A. Quelch 
##                     18                     18                     18 
##     Kenneth R. Andrews              Paul Hemp  Steven C. Wheelwright 
##                     18                     18                     18 
##         C. K. Prahalad          Chris Argyris Clayton M. Christensen 
##                     19                     19                     20 
##        David A. Garvin          Jay W. Lorsch          Myles L. Mace 
##                     20                     20                     20 
##      Robert N. Anthony   Benjamin M. Selekman      Malcolm P. McNair 
##                     20                     21                     21 
##   Regina E. Herzlinger       Richard L. Nolan          Bronwyn Fryer 
##                     21                     21                     22 
##         Leigh Buchanan      Benson P. Shapiro    Regina Fazio Maruca 
##                     23                     24                     24 
##         Alan M. Webber             Gary Hamel           John Dearden 
##                     25                     25                     25 
##    Timothy B. Blodgett         David W. Ewing            Julia Kirby 
##                     25                     26                     26 
##           Nitin Nohria        Robert H. Hayes             Maren Judd 
##                     26                     26                     27 
##         Suzy Wetlaufer            Arch Patton    Nancy Comfort Bowen 
##                     28                     30                     31 
##        Theodore Levitt           Adi Ignatius       Andrew O'Connell 
##                     31                     32                     32 
##         Harry Levinson       Robert S. Kaplan    Thomas H. Davenport 
##                     32                     32                     32 
##        Anne G. Perkins       David E. Gumpert      Michael E. Porter 
##                     34                     34                     34 
##              Nan Stone         Gardiner Morse      Mary V. Chatfield 
##                     35                     41                     42 
##   Rosabeth Moss Kanter      Lorna M. Daniells       Peter F. Drucker 
##                     43                     49                     51 
##              Don Moyer      Thomas A. Stewart         John T. Landry 
##                     63                     79                    255 
##                        
##                 237185
```

```r

# look at more stuff about number authors
with(hbr, tapply(n_authors, DOCUMENT.TYPE, mean, na.rm = T))
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

# look at total words total entries
nrow(hbr)
```

```
## [1] 12751
```

```r
# with word count
nrow(hbr[!is.na(hbr$FULL.TEXT.WORD.COUNT), ])
```

```
## [1] 5654
```

```r
# most of these also have page counts
nrow(hbr[!is.na(hbr$PAGE.COUNT) & !is.na(hbr$FULL.TEXT.WORD.COUNT), 
    ])
```

```
## [1] 5641
```

```r
# but nearly everything has page count
nrow(hbr[!is.na(hbr$PAGE.COUNT), ])
```

```
## [1] 12549
```

```r
# so use the average of entries with both to get an estimated total words
# for all entries
with(hbr[!is.na(hbr$PAGE.COUNT) & !is.na(hbr$FULL.TEXT.WORD.COUNT), 
    ], sum(FULL.TEXT.WORD.COUNT)/sum(PAGE.COUNT)) * with(hbr[!is.na(hbr$PAGE.COUNT), 
    ], sum(PAGE.COUNT))
```

```
## [1] 39612217
```

```r

# load graphics
library(ggplot2)
theme_set(theme_bw())
```

