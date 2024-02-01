# [CMFTS]
R package for time series features extraction

Repository related to the publication:
Baldán, F. J., & Benítez, J. M. (2023). Complexity measures and features for times series classification. Expert Systems with Applications, 213, 119227. https://doi.org/10.1016/j.eswa.2022.119227

# Installation

```
# Install from GitHub using devtools package
devtools::install_github("fjbaldan/CMFTS")
```


# Examples

```
# Example of use
cmfts::cmfts(t(data.frame(sample(1:100, 200, replace=TRUE))))
```

