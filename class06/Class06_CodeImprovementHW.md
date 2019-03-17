Code-Writing
================
STEPHANIE CURTIS
01/25/2019

###Create a function to improve existing code
#####Removing Calculation Duplication when Coding


Rather than reading into PDB three separate times, making three separate variables, and writing the same three lines of code for each, I have created a single function, **pdbplotfxn()**, to perform the lookup and analysis. This function allows the comparison of protein structure under drug-treated and untreated condition (for example) by visualizing the amino acid sequence plotted against the B-factor under both conditions.

``` r
#load package
library(bio3d)

#pdbplotfxn outputs a graph of the amino acid sequence along the x-axis to assess what regions of the protein are structurally flexible (B-factor). Different structures from PDB can be input to analyze a protein's structural change in response to treatment, in this case to assess the effect of two drugs on a kinase's structure to see if either can inhibit its ability to bind/phosphorylate substrate, and whether they act on the same region of the protein.
pdbplotfxn <- function(x){
  y <- read.pdb(x)
  sChainA <- trim.pdb(y,chain="A",elety="CA")
                      sb <- sChainA$atom$b
                      chainplot <- plotb3(sb,sse=sChainA,typ="l",ylab="Bfactor")}

#Inputs to the function are the following three PDB ID's in their normal four-letter format as you would input them to a read.pdb function. 
pdbplotfxn("4AKE")
```

    ##   Note: Accessing on-line PDB file

![](Class06_CodeImprovementHW_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
pdbplotfxn("1AKE")
```

    ##   Note: Accessing on-line PDB file
    ##    PDB has ALT records, taking A only, rm.alt=TRUE

![](Class06_CodeImprovementHW_files/figure-markdown_github/unnamed-chunk-1-2.png)

``` r
pdbplotfxn("1E4Y")
```

    ##   Note: Accessing on-line PDB file

![](Class06_CodeImprovementHW_files/figure-markdown_github/unnamed-chunk-1-3.png)

\`\`\`
