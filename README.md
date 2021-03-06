# Code sample: Programmer Analyst I

## Description

#### Purpose

Sample code for Programmer Analyst I role with Weill Cornell Medicine's Division of Biostatistics (Department of Population Health Sciences). 

#### Background on code

This is modified code from an in-preparation manuscript. I am solely responsible for the creation of this document's code and analytic method, but the overall study involved other members of the National Birth Defects Prevention Study (NBDPS) as coauthors: Stingone J, Luben T, Sheridan S, Langlois P, Shaw G, Reefhuis J, Romitti P, Feldkamp M, Nembhard W, Browne M, and Lin S. Organizational consent from NBDPS has been granted to use this code in its modified state.

The purpose of these analyses is to produce exploratory, descriptive, and analytic results related to the joint effects of maternal exposure to PM<sub>2.5</sub> (fine particulate matter) and extreme heat events (EHE) on offspring risk of certain congenital heart defects (CHDs).

#### Background on starting dataset

The starting dataset was provided by researchers at the New York NBDPS site. It includes CHD cases and controls with estimated dates of delivery from 1997-2007. It also includes PM<sub>2.5</sub> and temperature data, matched to each mother by geocode. There	are a number of extraneous variables in the dataset.

## Structure

* `code/` contains my relevant **sample code** in an RMarkdown file (`wcm_code_sample_052020.Rmd`).
* `data/` (not shared here) was established as a symbolic link to an external folder. Data for these analyses contain PHI and are not shareable.
* `results/` contains a knitted HTML file of **results**, complete with formatting and visualizations (`wcm_code_sample_052020.html`).
* `source/` contains will contain bare scripts (in this case, just one function sourced by the code file).

Project modified from structure created using the [projectr](https://github.com/jeff-goldsmith/projectr) package.
