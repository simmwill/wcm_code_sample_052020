# Code sample: Programmer Analyst I

## Description

#### Purpose

Sample code for Programmer Analyst I role with Weill Cornell Medicine's Division of Biostatistics (Department of Population Health Sciences). 

#### Background on code

This is modified code from an in-preparation manuscript. I am solely responsible for the creation of this code and analytic method, but other researchers on this study - other members of the National Birth Defects Prevention Study (NBDPS) - are as follows: Stingone J, Luben T, Sheridan S, Langlois P, Shaw G, Reefhuis J, Romitti P, Feldkamp M, Nembhard W, Browne M, Lin S. Organizational consent from NBDPS/CDC has been granted to use this code in its modified state.

The purpose of these analyses is to produce exploratory, descriptive, and analytic results related to the joint effects of maternal exposure to PM~2.5~ (fine particulate matter) and extreme heat events (EHE) on risk of certain offspring congenital heart defects (CHDs).

#### Background on starting dataset

The starting dataset was provided by researchers at the New York NBDPS site. It includes CHD cases and controls with estimated dates of delivery from 1997-2007. It also includes PM~2.5~ and temperature data, matched to each mother by geocode. There	are a number of extraneous variables in the dataset.

## Structure

* `code/` contains my relevant **sample code** in an RMarkdown file (`wcm_code_sample_052020.Rmd`).
* `data/` (not shown here) was established as a symbolic link to an external folder. Data for these analyses contain PHI and are not shareable.
* `results/` contains a knitted HTML file of **results**, complete with formatting and visualizations (`wcm_code_sample_052020.html`).

Project modified from structure created using the [projectr](https://github.com/jeff-goldsmith/projectr) package.
