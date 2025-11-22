
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![repostatus](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

**Disclaimer**  
The code presented here is experimental and was developed with
*within-subject designs* in mind. Extensions to *between-subject
designs* are planned, but no concrete timeline is currently available.
Moreover, the implementation does not yet include formal tests.

Therefore, if your primary goal is to conduct inference for state-trace
analysis, we presently recommend using the approach by Dunn and Kalish
(2018). A convenient option is the package available at
<https://github.com/monotonicity/stacmr>, which provides a wrapper for
the methodology implemented in <https://github.com/michaelkalish/STA>.

If, however, you are specifically interested in PIRST and wish to
explore or experiment with it, the material provided below may serve as
a starting point.

# pirst

State-trace analysis (STA) is a novel, rigorous, and highly general
method for investigating questions of dimensionality. Although STA has
gained attention in recent years, statistical testing procedures for the
method have only recently begun to emerge.

A particularly promising approach is *Permuted Isotonic Regression for
State-Trace* (PIRST, Benjamin, Griffin, and Douglas 2019), a
nonparametric algorithm based on isotonic regression. This package
implements the core PIRST algorithm in R and extends it by including a
significance test based on a null model, similar in spirit to the
procedure described by Dunn and Kalish (2018). Additionally, functions
for simulation studies are provided to facilitate evaluation and
exploration of this relatively new method.

For a more detailed discussion of this implementation, see Lukas
Rommel’s master thesis, included in this repository. The thesis
evaluates the strengths and limitations of PIRST using ROC, sensitivity,
and specificity analyses, and concludes with practical recommendations
for applied use.

## Installation

You can install the development version of pirst from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("johannes-titz/pirst")
```

# todo

lukas

- add e-mail to description
- decide who is hosting the package for cran (cre role)

# References

<div id="refs" class="references csl-bib-body hanging-indent"
entry-spacing="0">

<div id="ref-benjamin2019" class="csl-entry">

Benjamin, Aaron S., Michael L. Griffin, and Jeffrey A. Douglas. 2019. “A
Nonparametric Technique for Analysis of State-Trace Functions.” *Journal
of Mathematical Psychology* 90 (June): 88–99.
<https://doi.org/10/gn2mk8>.

</div>

<div id="ref-dunn2018" class="csl-entry">

Dunn, John C., and Michael L. Kalish. 2018. *State-Trace Analysis*.
Computational Approaches to Cognition and Perception. Cham: Springer
International Publishing. <https://doi.org/10.1007/978-3-319-73129-2>.

</div>

</div>
