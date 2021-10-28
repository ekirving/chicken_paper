# Chickens
This repository contains R code for plotting Figure 2 from 
the paper [A comprehensive archaeological assessment of the cultural origins and dispersal of domestic chicken]().

![Figure 2](./figure/Figure_2.png?raw=true)

If you reuse any of this code then please cite the paper:
> TBD

## Installation
Download the code: 
```bash
git clone git@github.com:ekirving/chickens.git && cd chickens/
```
To reproduce the figure from the paper you will need to install the software dependencies.

The easiest way to do this is with the [conda package manager](https://docs.conda.io/en/latest/).

```bash
conda env create --name chickens --file environment.yaml
```

Then activate the environment:
```bash
conda activate chickens
```

## Running the code

To reproduce Figure 2, run:

```bash
Rscript kriging-plot.R 
```

## Author

Evan K. Irving-Pease, [GLOBE Institute](https://globe.ku.dk/), University of Copenhagen 

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
