# SA-FPET

The Sexual Activity and Family Planning Estimation Tool (SA-FPET) is an R package to do Bayesian analysis of current duration data with reporting issues, applied to estimating the distribution of time-between-sex from time-since-last-sex data as collected in cross-sectional surveys in low- and middle-income countries. 

Authors: Chi Hyun Lee, Herbert Susmann, and Leontine Alkema, from the University of Massachusetts Amherst. This work was supported, in whole or in part, by the Bill & Melinda Gates Foundation (INV-00844). Contact: Leontine Alkema, lalkema@umass.edu 

For more info:  

- Main paper: CH. Lee, H. Susmann, L. Alkema (2023). New measures for family planning and exposure to risk of pregnancy based on sexual activity and contraceptive use data. Studies in Family Planning. https://doi.org/10.1111/sifp.12225. 

- Technical details: CH. Lee, H. Susmann, L. Alkema (2023). A Bayesian analysis of current duration data with reporting issues: an application to estimating the distribution of time-between-sex from time-since-last-sex data as collected in cross-sectional surveys in low- and middle-income countries. https://arxiv.org/abs/2302.00951. 



# Installation

This packages uses `RStan`, see https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started. 

Install `safpet` from Github:
```
remotes::install_github("AlkemaLab/safpet")
```
