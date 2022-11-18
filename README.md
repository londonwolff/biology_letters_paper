
# Friends aren't food: pinyon jays show context-dependent numerical cognition

-   Created on 2022-10-14 by Jeffrey R. Stevens
    (<jeffrey.r.stevens@gmail.com>)
-   Finalized on 2022-11-18

This repository provides the reproducible research materials for our project that investigates how numerical difference and ratio influence numerical preferences for food and social partners in pinyon jays. This includes the following:

-   Data
-   R script for data analysis
-   R Markdown file for the manuscript
-   R Markdown file for supplementary materials

## Citation

If you use any of these materials, please cite:

Wolff, L.M., Carey, K., & Stevens, J.R. (2022). Friends aren't food: pinyon jays show context-dependent numerical cognition. https://doi.org/10.31234/osf.io/kxgwt

## Summary

One data file contains data from four studies: two replicates of both a version with food as the objects and a version with conspecifics as the objects. Data were collected on pinyon jays (Gymnorhinus cyanocephalus) in the Avian Cognition Lab at the University of Nebraska-Lincoln between Feb 2020 and Mar 2022. Food replicate 1 involved 8 subjects. Food replicate 2 involved 4 subjects. Social replicate 1 involved 10 subjects and 17 stooges. Social replicate 2 involved 10 subjects and 8 stooges. In the data file, each row represents the information and choice for a single trial for one subject.

## License

All materials presented here are released under the Creative Commons Attribution 4.0 International Public License (CC BY 4.0). You are free to:

-   Share — copy and redistribute the material in any medium or format
-   Adapt — remix, transform, and build upon the material for any
    purpose, even commercially. Under the following terms:
-   Attribution — You must give appropriate credit, provide a link to the license, and indicate if changes were made. You may do so in any     reasonable manner, but not in any way that suggests the licensor endorses you or your use.

No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.

## Files

### Data files

`wolf_etal_2022_data.csv`

-   study - study label (Food or Social)
-   rep - replicate (1 or 2)
-   subject - subject name
-   sex - subject sex (Male or Female)
-   date - date of experimental session
-   session - session number
-   pair - numerical pair of smaller and larger value (e.g., 1-2 is 1 vs. 2)
-   small_num - smaller number in numerical pair
-   large_num - larger number in numerical pair
-   large_side - side (from experimenter perspective) for larger value
-   small_birds - name of stooge birds in smaller group
-   large_birds - name of stooge birds in larger group
-   choice - side (from experimenter perspective) of option chosen by subject
-   choose_larger - flag indicating whether chosen option was larger (1) or smaller (0) value

### R code
 
`wolff_etal_2022_rcode.R` - code for running computations and generating figures

### R Markdown documents

`wolff_etal_2022.Rmd` - R Markdown document with R code embedded for main manuscript `wolff_etal_2022_SM.qmd` - Quarto document with R code embedded for supplementary materials

### Installation

To reproduce these results, first clone or unzip the Git repository into a folder. Then, ensure that a subfolder named “figures” is in the folder. Next, open `wolff_etal_2022_rcode.R` in [RStudio](https://rstudio.com) or another R interface and ensure that all packages mentioned at the top of the script are installed. Once all packages are installed, run the script in R using `source("wolff_etal_2022_rcode.R")`.

Once the script runs without errors, you can compile the R Markdown document `wolff_etal_2022.Rmd.` Open this file in RStudio and ensure that you have packages [{knitr}](https://yihui.org/knitr/) and [{rmarkdown}](https://rmarkdown.rstudio.com/) installed. Once installed, use {knitr} to render the document (control-shift-K). To render `wolff_etal_2022_SM.qmd`, first install [Quarto](https://quarto.org), then render inside RStudio.
