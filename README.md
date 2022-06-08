# Chickens
This repository contains R code for the spatial kriging model used to make *Figure 2* from 
[The biocultural origins and dispersal of domestic chickens](https://doi.org/10.1073/pnas.2121978119).

![Figure 2](./figure/Figure_2.png?raw=true)

If you reuse any of this code then please cite the paper:
> Peters, J., Lebrasseur, O., Irving-Pease, E.K., Paxinos, P.D., Best, J., Smallman, R., Callou, C., Gardeisen, A., 
> Trixl, S., Frantz, L., Sykes, N., Fuller, D.Q., Larson, G., 2022. The biocultural origins and dispersal of domestic 
> chickens. *Proceedings of the National Academy of Sciences* 119, e2121978119. https://doi.org/10.1073/pnas.2121978119


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
