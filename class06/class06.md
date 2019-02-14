Class 06 R Functions
================
STEPHANIE CURTIS
01/25/2019

Let's read our file again:
--------------------------

New header and new intro text. Here we will try to use **read.table()** and friends.

``` r
read.table("https://bioboot.github.io/bggn213_S18/class-material/test1.txt", header=T, sep=",")
```

    ##   Col1 Col2 Col3
    ## 1    1    2    3
    ## 2    4    5    6
    ## 3    7    8    9
    ## 4    a    b    c

``` r
file1 <- "https://bioboot.github.io/bggn213_S18/class-material/test1.txt"
data1 <- read.csv(file1)
```

``` r
file2 <- "https://bioboot.github.io/bggn213_S18/class-material/test2.txt"
data2 <- read.table(file2,header=T,sep="$")
```

``` r
file3 <- "https://bioboot.github.io/bggn213_S18/class-material/test3.txt"
data3 <- read.table(file3)
```

R Functions
-----------

Function 1 ˆ

``` r
add <- function(x, y=1) {
 # Sum the input x and y
 x + y}
```

Using the add function

``` r
add(1)
```

    ## [1] 2

``` r
#specifying a y value overrides default y value
add(1,2)
```

    ## [1] 3

``` r
#if x is a vector it adds y to every value in the vector
add(c(1,2,3,4))
```

    ## [1] 2 3 4 5

``` r
#same if y is a vector and x is a specfied number
add(6,c(1,2,3,4))
```

    ## [1]  7  8  9 10

``` r
add(c(1,2,3,4,c(1,2,3,4)))
```

    ## [1] 2 3 4 5 2 3 4 5

``` r
rescale <- function(x){rng <- range(x)
(x-rng[1]/rng[2]-rng[1])}
```

``` r
rescale(c(1,2,NA,3,10))
```

    ## [1] NA NA NA NA NA

``` r
rescale2 <- function(x){rng <- range(x,na.rm=T)
(x-rng[1]/rng[2]-rng[1])}
```

``` r
rescale2(c(1,2,NA,3,10))
```

    ## [1] -0.1  0.9   NA  1.9  8.9

``` r
rescale3 <- function(x, na.rm=TRUE, plot=FALSE) {rng <-range(x, na.rm=TRUE)
 print("Hello")
 answer <- (x - rng[1]) / (rng[2] - rng[1])
 return(answer)
 print("is it me you are looking for?")
 if(plot) {
 plot(answer, typ="b", lwd=4)
 }
 print("I can see it in ...")}
```

``` r
rescale3(1:10)
```

    ## [1] "Hello"

    ##  [1] 0.0000000 0.1111111 0.2222222 0.3333333 0.4444444 0.5555556 0.6666667
    ##  [8] 0.7777778 0.8888889 1.0000000
