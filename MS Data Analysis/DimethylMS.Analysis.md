Dimethyl MS Data
================

Preparatory code - Packages
---------------------------

### Load necessary packages to use on your data.

For data manipulation:

``` r
library(dplyr)
```

For gene list annotation:

``` r
library("AnnotationDbi")
```

For mouse (Mm) database information from entrez gene (eg). Keys that you can use with this data are listed below, which you will want to know for later as you use functions from the AnnotationDbi packages

\[1\] "ACCNUM" "ALIAS" "ENSEMBL" "ENSEMBLPROT" \[5\] "ENSEMBLTRANS" "ENTREZID" "ENZYME" "EVIDENCE"
\[9\] "EVIDENCEALL" "GENENAME" "GO" "GOALL"
\[13\] "IPI" "MAP" "OMIM" "ONTOLOGY"
\[17\] "ONTOLOGYALL" "PATH" "PFAM" "PMID"
\[21\] "PROSITE" "REFSEQ" "SYMBOL" "UCSCKG"
\[25\] "UNIGENE" "UNIPROT"

``` r
library("org.Mm.eg.db")
```

``` r
#gage has kegg and go in it
library(pathview)
library(gage)
library(gageData)

#data(kegg.sets.hs)
#data(sigmet.idx.hs)
```

``` r
# Notes from bggn213 c15 that may be useful to you:
#Focus on signaling and metabolic pathways only
#what is this
#kegg.sets.hs = kegg.sets.hs[sigmet.idx.hs]
# Examine the first 3 pathways.
#head(kegg.sets.hs, 3)
```

### Practicing dplyr verbs on sample data:

Sample data includes just two columns: An "ID" column with 12 unique random alpha-numeric assigments and a description column correlating to these IDs which frequently contain the word "gene", "sup", and/or "thing" to practice creating groups based on words contained in gene names.

``` r
dprac<-read.csv("Dplyrpractice.csv")
dpract<-as.tbl(dprac)
```

The following 2 chunks are not to be used, but good for you to know that you tried. The first code chunk is limited to 1 & 0 values and single columns per call of function. Would require the same number of calls as categories desired **PLUS** an additional step of summing and accounting for any possible duplicate assigments. Named this function grepscore1 for version 1 of a possible scoring function based on using grep. The second chunk generates index (positional info) of TRUE grepl outcomes but how you would create values in a new column for only 3 rows is unclear and maybe not possible.

``` r
grepscore1<- function(z, x) {
  mutate(z, GrepScore.y = as.numeric(grepl(x, z$Description)))}
grepscore1(dpract, "ge")
```

    ## # A tibble: 12 x 3
    ##    PID   Description GrepScore.y
    ##    <fct> <fct>             <dbl>
    ##  1 P34   Genie                 0
    ##  2 P21   Yes-gene              1
    ##  3 O39   heygene               1
    ##  4 Q09   higene                1
    ##  5 P45   hellog                0
    ##  6 P91   supg                  0
    ##  7 P12   gegee                 1
    ##  8 P10   sup thing             0
    ##  9 O33   thing (sup)           0
    ## 10 O77   thinggene             1
    ## 11 P78   hithere               0
    ## 12 P90   Yes indeed            0

``` r
ginds<-which(grepl("sup", dpract$Description))
ginds
```

    ## [1] 6 8 9

Let's improve this code by **nesting ifelse() into mutate()**. Online example about classroom letter grades where "gradebook"" was the df and "letter"" was the column to-be-added using the mutate function, as defined by info in the "grade" column used the following code to do something similar to my aim: mutate(gradebook, letter = ifelse(grade %in% 60:69, "D", ifelse(grade %in% 70:79, "C", ifelse(grade %in% 80:89, "B", ifelse(grade %in% 90:99, "A", "F")))))

``` r
#This uses dataframe as your only argument, z
#Note the inclusion of an OR operator inside tge second grepl call to classify multiple things as 1 subtype
grepcatp<- function(z) {
  mutate(z, ProtCat = ifelse(grepl("sup|thing", z$Description), "Super", 
                               ifelse(grepl("ge|eni", z$Description), "Genesies", "No Class")))}
```

Note that when you call this function on your practice dataset that the observation "thinggene" gets classified by the first category it falls into, not the second. The first category (Super) is assigned based on the word "thing" being present in the description column, and the second category you created (Genesies) is based on "gene" being there. Since "thinggene" contains both, it is **important to note** which category it was ultimately assigned.

``` r
dpract.cat<-grepcatp(dpract)
dpract.cat
```

    ## # A tibble: 12 x 3
    ##    PID   Description ProtCat 
    ##    <fct> <fct>       <chr>   
    ##  1 P34   Genie       Genesies
    ##  2 P21   Yes-gene    Genesies
    ##  3 O39   heygene     Genesies
    ##  4 Q09   higene      Genesies
    ##  5 P45   hellog      No Class
    ##  6 P91   supg        Super   
    ##  7 P12   gegee       Genesies
    ##  8 P10   sup thing   Super   
    ##  9 O33   thing (sup) Super   
    ## 10 O77   thinggene   Super   
    ## 11 P78   hithere     No Class
    ## 12 P90   Yes indeed  No Class

``` r
dpract$Description<-gsub("g.*","", dpract$Description)
```

``` r
dpract
```

    ## # A tibble: 12 x 2
    ##    PID   Description
    ##    <fct> <chr>      
    ##  1 P34   Genie      
    ##  2 P21   Yes-       
    ##  3 O39   hey        
    ##  4 Q09   hi         
    ##  5 P45   hello      
    ##  6 P91   sup        
    ##  7 P12   ""         
    ##  8 P10   sup thin   
    ##  9 O33   thin       
    ## 10 O77   thin       
    ## 11 P78   hithere    
    ## 12 P90   Yes indeed

``` r
#library(tidyr)
#strsplit
#dpract%>%separate()
```

Data Analysis
-------------

One of the first thing you may want to do is add your classification column, as practced above. There is an annotation dbs that can do an ~equivalent form of this for you, but your own simplification of perhaps a smaller number of categories may be preferred? Add column for protein category named "ProtCat" using **mutate()** to classify proteins based on the content of their $DESCRIPTION, a column from IP2 that includes both the protein name and gene symbol.

Notes: -This you should use on your data but watch out upper/lower case letters for your grep... Right now you have omitted the first letter of several words in order to capture them when they occur with lower case first letter and upper case first letter. May want to return to this and learn how to call both.

-Input to the function grepcat is simply your dataframe. (which is read in as **MSData**)

-Write in compartmental designations (ifelse statements) toward the **beginning** of the code because you want them to supercede other categories like "tf" or "kinase" if they are compartment specific. You have not addressed the potential for dual classification in your code directly, although all manual pre-screening of excel workbook looked good. (if you use "actin" it will pick up that sequence of letters in "interacting")

Start by getting your data:

``` r
MSData<-as.tbl(read.csv("0302WT2.csv"))
```

Have a look:

``` r
head(MSData)
```

    ## # A tibble: 6 x 16
    ##   PLINE LOCUS AVERAGE_RATIO AVERAGE_RATIO_R… STANDARD_DEVIAT…
    ##   <fct> <fct>         <dbl>            <dbl> <fct>           
    ## 1 P     A0A0…         0.466            0.562 <NA>            
    ## 2 P     A0A2…         0.779            0.733 <NA>            
    ## 3 P     A0A2…         0.673            0.853 <NA>            
    ## 4 P     Q9WV…         0.433            0.631 <NA>            
    ## 5 P     B7ZC…         0.972            0.970 0.02176         
    ## 6 P     O885…         0.555            0.676 <NA>            
    ## # … with 11 more variables: STANDARD_DEVIATION_REV <fct>,
    ## #   COMPOSITE_RATIO <dbl>, COMPOSITE_RATIO_STANDARD_DEVIATION <dbl>,
    ## #   WEIGHTED_AVERAGE <dbl>, LOG_INV_AVERAGE <dbl>,
    ## #   LOG_INV_AVERAGE_REV <dbl>, PEPTIDE_NUM <int>, TOTAL_PEPTIDE_NUM <int>,
    ## #   SPEC_COUNT <int>, AREA_RATIO <dbl>, DESCRIPTION <fct>

``` r
colnames(MSData)
```

    ##  [1] "PLINE"                             
    ##  [2] "LOCUS"                             
    ##  [3] "AVERAGE_RATIO"                     
    ##  [4] "AVERAGE_RATIO_REV"                 
    ##  [5] "STANDARD_DEVIATION"                
    ##  [6] "STANDARD_DEVIATION_REV"            
    ##  [7] "COMPOSITE_RATIO"                   
    ##  [8] "COMPOSITE_RATIO_STANDARD_DEVIATION"
    ##  [9] "WEIGHTED_AVERAGE"                  
    ## [10] "LOG_INV_AVERAGE"                   
    ## [11] "LOG_INV_AVERAGE_REV"               
    ## [12] "PEPTIDE_NUM"                       
    ## [13] "TOTAL_PEPTIDE_NUM"                 
    ## [14] "SPEC_COUNT"                        
    ## [15] "AREA_RATIO"                        
    ## [16] "DESCRIPTION"

Clean up data: Overwrite variable MSData after removing extra stuff from strings in the Description column.

Overwrite column:

``` r
MSData$DESCRIPTION<-gsub("PE=.*","", MSData$DESCRIPTION)
```

``` r
grepcat<- function(z) {
  mutate(z, ProtCat = ifelse(grepl("itochondria|ubiquinone|ytochrome", z$DESCRIPTION), "Mitochondria", 
                               ifelse(grepl("kinase|phosphatase" , z$DESCRIPTION), "Kinase/Phosphatase",
                                      ifelse(grepl("istone-lysine N|istone deacetylase|istone demethy|istone-binding protein|istone methyl", z$DESCRIPTION), "Histone Modifiers",
                                             ifelse(grepl("ibonucleoprot", z$DESCRIPTION), "Ribonucleoproteins",
                                                    ifelse(grepl("T-complex|Heat shock", z$DESCRIPTION), "Chaperone complexes",
                                                           ifelse(grepl("40S ribosomal prot|60S ribosomal prot", z$DESCRIPTION), "Ribosome",
                                                                  ifelse(grepl("olgi|esicle|xosome|endoplasmic", z$DESCRIPTION), "ER-Golgi-Vesicular-Exosome",
                                                                        ifelse(grepl("inc finger|PHD finger|orkhead box|PDZ and|nhibitor of growth protein", z$DESCRIPTION), "Transcription Factor",
                                                                               ifelse(grepl("rueppel-like factor|obox|B-cell CLL|our and a half LIM", z$DESCRIPTION), "Transcription Factor",
                                                                                      ifelse(grepl("RNA polymerase II|ranscription initiation|ranscription elongation|General transcription factor", z$DESCRIPTION), "General Transcription",
                                                                                             ifelse(grepl("Histone", z$DESCRIPTION), "Histones",
                                                                                                      ifelse(grepl("portin|uclear pore|ucleoporin", z$DESCRIPTION), "Nuclear Transport Machinery",
                                                                                                             ifelse(grepl("plicing factor|RNA helicase|mRNA-processing|THO complex", z$DESCRIPTION), "mRNA processing",
                                                                                                                    ifelse(grepl("biquitin|SUMO", z$DESCRIPTION), "Ub-SUMO-related",
                                                                                                                           ifelse(grepl("actinin|Actin-|Actin,|ubulin|yosin|oronin|ollagen|ransgelin|Septin-|dynein|ytoskeleton", z$DESCRIPTION), "Cytoskeleton",
                                                                                                                                  ifelse(grepl("ukaryotic translation initiation|RNA-binding protein|RNA binding protein", z$DESCRIPTION), "Translation Regulation",
                               ifelse(grepl("ranscription factor", z$DESCRIPTION), "Transcription Factor",
                                      "Other"))))))))))))))))))}
```

Apply classification function **grepcat()** to your data:

``` r
MSData.cat<-grepcat(MSData)
```

Verify your new column "ProtCat" is there:

``` r
colnames(MSData.cat)
```

    ##  [1] "PLINE"                             
    ##  [2] "LOCUS"                             
    ##  [3] "AVERAGE_RATIO"                     
    ##  [4] "AVERAGE_RATIO_REV"                 
    ##  [5] "STANDARD_DEVIATION"                
    ##  [6] "STANDARD_DEVIATION_REV"            
    ##  [7] "COMPOSITE_RATIO"                   
    ##  [8] "COMPOSITE_RATIO_STANDARD_DEVIATION"
    ##  [9] "WEIGHTED_AVERAGE"                  
    ## [10] "LOG_INV_AVERAGE"                   
    ## [11] "LOG_INV_AVERAGE_REV"               
    ## [12] "PEPTIDE_NUM"                       
    ## [13] "TOTAL_PEPTIDE_NUM"                 
    ## [14] "SPEC_COUNT"                        
    ## [15] "AREA_RATIO"                        
    ## [16] "DESCRIPTION"                       
    ## [17] "ProtCat"

There are 16 categories:

``` r
print(unique(MSData.cat$ProtCat))
```

    ##  [1] "Other"                       "Cytoskeleton"               
    ##  [3] "mRNA processing"             "Ribonucleoproteins"         
    ##  [5] "Ribosome"                    "Kinase/Phosphatase"         
    ##  [7] "Translation Regulation"      "Chaperone complexes"        
    ##  [9] "Transcription Factor"        "Ub-SUMO-related"            
    ## [11] "Nuclear Transport Machinery" "Mitochondria"               
    ## [13] "Histone Modifiers"           "General Transcription"      
    ## [15] "ER-Golgi-Vesicular-Exosome"  "Histones"

``` r
glimpse(MSData.cat)
```

    ## Observations: 12,406
    ## Variables: 17
    ## $ PLINE                              <fct> P, P, P, P, P, P, P, P, P, P,…
    ## $ LOCUS                              <fct> A0A0G2JEP8, A0A2R8VHF3, A0A2R…
    ## $ AVERAGE_RATIO                      <dbl> 0.46628, 0.77947, 0.67316, 0.…
    ## $ AVERAGE_RATIO_REV                  <dbl> 0.56194, 0.73306, 0.85318, 0.…
    ## $ STANDARD_DEVIATION                 <fct> NA, NA, NA, NA, 0.02176, NA, …
    ## $ STANDARD_DEVIATION_REV             <fct> NA, NA, NA, NA, 0.00068, NA, …
    ## $ COMPOSITE_RATIO                    <dbl> 0.92457, 1.05775, 0.89153, 0.…
    ## $ COMPOSITE_RATIO_STANDARD_DEVIATION <dbl> 1.44598, 1.29149, 1.28296, 1.…
    ## $ WEIGHTED_AVERAGE                   <dbl> 0.69632, 0.96963, 0.79564, 0.…
    ## $ LOG_INV_AVERAGE                    <dbl> 0.46628, 0.77947, 0.67316, 0.…
    ## $ LOG_INV_AVERAGE_REV                <dbl> 0.56194, 0.73306, 0.85318, 0.…
    ## $ PEPTIDE_NUM                        <int> 24, 87, 64, 183, 3, 266, 132,…
    ## $ TOTAL_PEPTIDE_NUM                  <int> 24, 87, 64, 183, 3, 266, 132,…
    ## $ SPEC_COUNT                         <int> 47, 359, 259, 649, 3, 1553, 4…
    ## $ AREA_RATIO                         <dbl> 1.0685130, 1.1086821, 0.90785…
    ## $ DESCRIPTION                        <chr> "Rho-related GTP-binding prot…
    ## $ ProtCat                            <chr> "Other", "Cytoskeleton", "Oth…

If you have to get rid of any NA values, you will use:

``` r
#if you have to get rid of NA values... area ratio perhaps
#filter(MSdata.cat, !is.na(AREA_RATIO) 
```

### make small lists of potentially interesting transcription factors.

Of the proteins qualified as "Transcription Factor" in your new protein category column, **filter()** those that have a good Area Ratio and Spec count.

``` r
allTF<-unique(filter(MSData.cat, ProtCat=="Transcription Factor"))
nrow(allTF)
```

    ## [1] 763

``` r
#Interesting Transcription Factors, Shuttle Out(SO)
IntTF.SO<-MSData.cat%>%filter(ProtCat=="Transcription Factor" & AREA_RATIO >= 1.5 & SPEC_COUNT>1) 
#IntTF.SO$DESCRIPTION or maybe better:
unique(IntTF.SO$DESCRIPTION)
```

    ##  [1] "Zinc finger RNA-binding protein (Fragment) OS=Mus musculus OX=10090 GN=Zfr "                     
    ##  [2] "Zinc finger CCCH domain-containing protein 14 (Fragment) OS=Mus musculus OX=10090 GN=Zc3h14 "    
    ##  [3] "Zinc finger protein ubi-d4 (Fragment) OS=Mus musculus OX=10090 GN=Dpf2 "                         
    ##  [4] "Zinc finger protein ubi-d4 OS=Mus musculus OX=10090 GN=Dpf2 "                                    
    ##  [5] "Zinc finger RNA-binding protein OS=Mus musculus OX=10090 GN=Zfr "                                
    ##  [6] "Krueppel-like factor 5 OS=Mus musculus OX=10090 GN=Klf5 "                                        
    ##  [7] "MORC family CW-type zinc finger protein 2A (Fragment) OS=Mus musculus OX=10090 GN=Morc2a "       
    ##  [8] "Forkhead box protein K1 OS=Mus musculus OX=10090 GN=Foxk1 "                                      
    ##  [9] "Zinc finger protein 512B (Fragment) OS=Mus musculus OX=10090 GN=Zfp512b "                        
    ## [10] "Zinc finger protein 48 OS=Mus musculus OX=10090 GN=Znf48 "                                       
    ## [11] "Zinc finger protein 592 OS=Mus musculus OX=10090 GN=Znf592 "                                     
    ## [12] "Zinc finger protein with KRAB and SCAN domains 3 OS=Mus musculus OX=10090 GN=Zkscan3 "           
    ## [13] "Transcription factor Sp3 OS=Mus musculus OX=10090 GN=Sp3 "                                       
    ## [14] "Cyclic AMP-dependent transcription factor ATF-2 OS=Mus musculus OX=10090 GN=Atf2 "               
    ## [15] "Paired mesoderm homeobox protein 2 OS=Mus musculus OX=10090 GN=Prrx2 "                           
    ## [16] "Paired mesoderm homeobox protein 1 OS=Mus musculus OX=10090 GN=Prrx1 "                           
    ## [17] "Zinc finger protein 335 OS=Mus musculus OX=10090 GN=Znf335 "                                     
    ## [18] "PHD finger protein 20-like protein 1 OS=Mus musculus OX=10090 GN=Phf20l1 "                       
    ## [19] "Zinc finger and BTB domain-containing 40 OS=Mus musculus OX=10090 GN=Zbtb40 "                    
    ## [20] "ETS-related transcription factor Elf-4 (Fragment) OS=Mus musculus OX=10090 GN=Elf4 "             
    ## [21] "Zinc finger protein 462 OS=Mus musculus OX=10090 GN=Zfp462 "                                     
    ## [22] "Zinc finger MYM-type protein 3 OS=Mus musculus OX=10090 GN=Zmym3 "                               
    ## [23] "Zinc finger protein 768 OS=Mus musculus OX=10090 GN=Zfp768 "                                     
    ## [24] "Zinc finger protein 768 OS=Mus musculus OX=10090 GN=Znf768 "                                     
    ## [25] "Zinc finger protein with KRAB and SCAN domains 5 (Fragment) OS=Mus musculus OX=10090 GN=Zkscan5 "
    ## [26] "Zinc finger protein with KRAB and SCAN domains 5 OS=Mus musculus OX=10090 GN=Zkscan5 "

``` r
#where AREA_RATIO CUTOFF is not hard-coded as 1.5 or anything,>>> rather is a calculation of some cut-off value (top 25% of shuttling values in data?) !!!!
```

``` r
#Shuttle In (SI)
IntTF.SI<-MSData.cat%>%filter(ProtCat=="Transcription Factor" & AREA_RATIO <= 0.5 & SPEC_COUNT>1) 
#IntTF.SI$DESCRIPTION or maybe better:
unique(IntTF.SI$DESCRIPTION)
```

    ##  [1] "Transcription factor Dp-1 (Fragment) OS=Mus musculus OX=10090 GN=Tfdp1 "      
    ##  [2] "Transcription factor Dp-1 OS=Mus musculus OX=10090 GN=Tfdp1 "                 
    ##  [3] "Zinc finger protein basonuclin-2 (Fragment) OS=Mus musculus OX=10090 GN=Bnc2 "
    ##  [4] "PHD finger protein 19 OS=Mus musculus OX=10090 GN=Phf19 "                     
    ##  [5] "Zinc finger protein 871 OS=Mus musculus OX=10090 GN=Zfp871 "                  
    ##  [6] "Transcription factor 3 OS=Mus musculus OX=10090 GN=Tcf7l1 "                   
    ##  [7] "Transcription factor 7-like 1 OS=Mus musculus OX=10090 GN=Tcf7l1 "            
    ##  [8] "Transcription factor 7-like 2 (Fragment) OS=Mus musculus OX=10090 GN=Tcf7l2 " 
    ##  [9] "Zinc finger protein 217 OS=Mus musculus OX=10090 GN=Zfp217 "                  
    ## [10] "Zinc finger protein 536 OS=Mus musculus OX=10090 GN=Znf536 "

### TF quick summary:

There are **42** Transcription Factors that potentially shuttle out of the nucleus. There are **13** Transcription Factors that potentially shuttle into the nucleus.

### Annotating Data

To annotate my data. I will want to map into the annotation database for several different protein lists that I generate leading up to this point including: 1. The whole data 2. The lists of shuttling proteins 3. The refined lists of shuttling proteins (nice intensity and &gt;1 spec count?) or potentially the refined list of potentially interesting TFs from code chunk above. 4. The list of non-shuttling proteins (I will want this to include histones and ? what else do I expect not to shuttle)

``` r
#NOTE I HAVE "my.df.intheformat.ofmymaking" as a placeholder for the name of your dataframe which you will build up to this code chunk. Once you have built up to this and named your df properly, fix the name in this code chunk and DELETE this text.
#You will be using the Uniprot ID for mapping to annotation databases because this is the first column of your data. 
#keys = will have the be either my.df.intheformat.ofmymaking$UniprotIDs if you have literally named a column "UniprotIDs" --- but if the Uniprot IDs in your data are the row names of your dataframe then you can 


#my.df.intheformat.ofmymaking$symbol <- mapIds(org.Mm.eg.db,
                    # keys=row.names(my.df.intheformat.ofmymaking),
                    # column="SYMBOL",
                    # keytype="UNIPROT",
                    # multiVals="first")
```
