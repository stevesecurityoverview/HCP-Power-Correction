# HCP-Power-Correction
Power Correction Code and Analysis for the Human Connectome Project

This repository contains a spreadsheet of freesurfer outputs adhering to the [HCP (Human Connectome Project)](https://www.humanconnectome.org/) data format.
It also contains scripts the related to power, rather than linear, correction of intra-cranial volumes.
This adjustment may improve insights for the NTR dataset, using a processes to normalize correction of volumes across different regions.

## Credits
Project Started by Sam Rosenberg
Adjusted by Stephen Shin and _

Sponsored and directed by Dr. Jeremy Purcell @ University of Maryland.

## Background
[The power-proportion method for intracranial volume correction in volumetric imaging analysis](https://www.frontiersin.org/journals/neuroscience/articles/10.3389/fnins.2014.00356/full)

## Problem: Are volumes of interest are proportional or linearly related to total intracranial volume

It has been shown that many structures have a power law relationship to ICV
Prior methods: 
- Proportion method: divide raw VOI by ICV
- ANCVOA: use ICV as a covariate in a regression model w/ VOI as dependent variable
  Performs better than proportion
    
## Solution
Applying a power-proportionate method to account for ICV differences
Studies show:

        VOI=⍺*ICV^β

- All VOIs measured besides ventricles followed power law
- For estimating a and b: VOI=⍺*ICV^β+ε
- ε is random noise assumed to be independent, identical, and have a normal distribution with a mean=0
- VOI Power proportion Corrected=VOI/〖ICV〗^β 
- Leads to a near-zero correlation of PPC-VOI to ICV, whereas the lone VOI is far more correlated with ICV
- Similar to ANCOVA for ICV correction – VOI and ICV are log-transformed and a linear regression is fitted(residual method)
- Key difference is this accounts for the variability of VOI – instead of VOI variability increasing with ICV, VOI variability is constant for all ICV

## Other References
[Neuroanatomical norms in the UK Biobank: The impact of allometric scaling, sex, and age](https://onlinelibrary.wiley.com/doi/full/10.1002/hbm.25572)

Problem: No studies done to describe population-wide variations in neuroanatomy
Goal: Discover population wide variations in neuroanatomy and generate markers that consider age, sex, and brain size
- Used ~40,000 subjects from UK Biobank to take measures
- Equation for TBV  
- Equation for other global measures(ie. MCT, WMV, subcortical GMV, etc.)

- For regional voumes, surface area, and cortical thickness:
     
Conclusion: Most brain regions grow allometrically with TBV in a power-related manner. This relationship can be better estimated when accounting for both age and sex – which also create different amounts of variability. 
- Sex differences affect about. 2/3 of brain regions
- Age
- ¾ of regions have linear relationship with age
- ¼ is quadratic
- There is significant sex by age interactions in 14% of regions
- Linear covariate model misestimated about 2.35%
- Proportion adjustment overestimated 14.24% of effects
Accountiing for all these factors – importantly allometry – one can draw more accurate conclusioins


#### Ignore this section (internal powerpoint) (contributions welcome)
PowerPoint
    PowerPoint reiterates method and cites studies that have shown the efficacy of this method
    PPT also shows an example of how using just a proportion method can give incorrect results


## TEST