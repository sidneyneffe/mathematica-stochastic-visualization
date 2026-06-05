# Stochastic Visualization Mathematica Package
A package for visualizing uncertainty, Markov Chains and Monte-Carlo simulations.


## Accessing Random Number Generation

### SVQuantilePlot

Plot QQ and CDF plots for any list of distributions.

```mathematica
SVQuantilePlot[
    {
        NormalDistribution[],
        CauchyDistribution[0, 1],
        StudentTDistribution[2]
    },
    CDFDomain -> {-3, 3}
]
```

<img src="docs/sv-quantile-plot.jpg" width=350>

### SVQuantilePlot3D

Display a 3-dimensional Quantile-Quantile-Quantile plot of three different distributions.

```mathematica
SVQuantilePlot3D[
    NormalDistribution[], 
    CauchyDistribution[0, 1],
    StudentTDistribution[2],
    ImageSize -> 400
]
```

<img src="docs/sv-quantile-plot-3d.jpg" width=350>
