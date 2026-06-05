# Stochastic Visualization Mathematica Package
A package for visualizing uncertainty, Markov Chains and Monte-Carlo simulations.


## Accessing Random Number Generation

### SVQuantilePlot

Plots QQ and CDF plots for any list of distributions.

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
<img src="docs/sv-quantile-plot.jpg" width=250>


### SVQuantilePlot3D

Displays a 3-dimensional Quantile-Quantile-Quantile plot of three different distributions.

```mathematica
SVQuantilePlot3D[
    NormalDistribution[], 
    CauchyDistribution[0, 1],
    StudentTDistribution[2],
    ImageSize -> 400
]
```
<img src="docs/sv-quantile-plot-3d.jpg" width=250>


### SVQuantilePlot3D

Applies the [Acception-Rejection Method](https://en.wikipedia.org/wiki/Rejection_sampling) and visualizes the result, to generate random samples with a target density $\frac{f}{\vert f\vert}$ (that fulfills $f\leq C g(x)$ for some $C>0$) by sampling from a distribution $g$.

```mathematica
gDistribution = TransformedDistribution[
    1 + X,
    X \[Distributed] ExponentialDistribution[1]
];
SVRejectionPlot[
    Piecewise[{{E^(-x^2/2), 1 <= x}, {0, True}}],
    {x, 0, 5},
    gDistribution,
    1/Sqrt[E],
    NSamples -> 700
]
```
<img src="docs/sv-rejection-plot.jpg" width=200>

