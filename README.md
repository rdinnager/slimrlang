
<!-- README.md is generated from README.Rmd. Please edit that file -->

<STYLE type='text/css' scoped>
PRE.fansi SPAN {padding-top: .25em; padding-bottom: .25em};
</STYLE>

# slimrlang

<!-- badges: start -->

[![R build
status](https://github.com/rdinnager/slimrlang/workflows/R-CMD-check/badge.svg)](https://github.com/rdinnager/slimrlang/actions)
<!-- badges: end -->

The goal of slimrlang is to provide an environment in which you can
write SLiM population genetics simulation scripts from R. It works
particularly well with RStudio, but any R IDE can be used. For much more
advanced functionality in running and processing SLiM simulations from
R, see the [`slimr`](https://github.com/rdinnager/slimr) package which
imports `slimrlang`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("rdinnager/slimrlang")
```

## Example

Using `slimrlang`, this is how you write the first example script (or
recipe) from the (excellent) [SLiM](https://messerlab.org/slim/) manual:

``` r
library(slimrlang)

slim_script(
  slim_block(initialize(),
             {
               ## set the overall mutation rate
               initializeMutationRate(1e-7); 
               ## m1 mutation type: neutral
               initializeMutationType("m1", 0.5, "f", 0.0);
               ## g1 genomic element type: uses m1 for all mutations
               initializeGenomicElementType("g1", m1, 1.0);
               ## uniform chromosome of length 100 kb
               initializeGenomicElement(g1, 0, 99999);
               ## uniform recombination along the chromosome
               initializeRecombinationRate(1e-8);
             }),
  slim_block(1,
             {
               sim.addSubpop("p1", 500);
             }),
  slim_block(10000,
             {
               sim.simulationFinished();
             })
) -> script_1

script_1
```

<PRE class="fansi fansi-output"><CODE>#&gt; <span style='background-color: #00BBBB;font-weight: bold;'>block_init</span><span>   initialize() {
#&gt;       </span><span style='color: #00BBBB;'>initializeMutationRate</span><span style='color: #BBBB00;'>(</span><span style='color: #0000BB;'>1e-07</span><span style='color: #BBBB00;'>)</span><span>;
#&gt;       </span><span style='color: #00BBBB;'>initializeMutationType</span><span style='color: #BBBB00;'>("m1"</span><span>, </span><span style='color: #0000BB;'>0.5</span><span>, </span><span style='color: #BBBB00;'>"f"</span><span>, </span><span style='color: #0000BB;'>0</span><span style='color: #BBBB00;'>)</span><span>;
#&gt;       </span><span style='color: #00BBBB;'>initializeGenomicElementType</span><span style='color: #BBBB00;'>("g1"</span><span>, m1, </span><span style='color: #0000BB;'>1</span><span style='color: #BBBB00;'>)</span><span>;
#&gt;       </span><span style='color: #00BBBB;'>initializeGenomicElement</span><span style='color: #BBBB00;'>(</span><span>g1, </span><span style='color: #0000BB;'>0</span><span>, </span><span style='color: #0000BB;'>99999</span><span style='color: #BBBB00;'>)</span><span>;
#&gt;       </span><span style='color: #00BBBB;'>initializeRecombinationRate</span><span style='color: #BBBB00;'>(</span><span style='color: #0000BB;'>1e-08</span><span style='color: #BBBB00;'>)</span><span>;
#&gt; }
#&gt; 
#&gt; </span><span style='background-color: #00BBBB;font-weight: bold;'>block_2</span><span>  1 early() {
#&gt;       </span><span style='color: #00BBBB;'>sim.addSubpop</span><span style='color: #BBBB00;'>("p1"</span><span>, </span><span style='color: #0000BB;'>500</span><span style='color: #BBBB00;'>)</span><span>;
#&gt; }
#&gt; 
#&gt; </span><span style='background-color: #00BBBB;font-weight: bold;'>block_3</span><span>  10000 early() {
#&gt;       </span><span style='color: #00BBBB;'>sim.simulationFinished</span><span style='color: #BBBB00;'>()</span><span>;
#&gt; }
</span></CODE></PRE>

You can output this script to text to run in a standalone SLiM
installation, or you can run it in SLiM directly from R using functions
from the companion R package
[`slimr`](https://github.com/rdinnager/slimr).

You can also do fancy stuff like make the above script or another script
into a template that you can dynamically fill-in with parameters
generated in R. You can also make SLiM generate R-friendly input. See
the vignettes for details of these features and how to use them.
