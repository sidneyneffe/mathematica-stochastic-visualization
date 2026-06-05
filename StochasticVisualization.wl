
BeginPackage["StochasticVisualization`"]

CDFDomain::usage = "CDFDomain is an option for SVQuantilePlot and defines the parameter space of the CDF plot."

QQDomain::usage = "QQDomain is an option for SVQuantilePlot and SVQuantilePlot3D and defines the parameter space of the QQ plot."

SVQuantilePlot::usage = "SVQuantilePlot[{d1, d2, ...}] plots CDF and Quantile-Quantile functions for a list of distributions.\nSVQuantilePlot[{d1, d2, ...}, CDFDomain -> {x1, x2}, QQDomain -> {y1, y2}] plots with specified parameter ranges."

SVQuantilePlot3D::usage = "SVQuantilePlot3D[distX, distY, distZ, QQDomain->{-30, 30}] shows a 3D plot of three distributions with the specified parameter range."


Begin["`Private`"]


(* Common functions *)

distributionToString[distribution_] := Module[{newName}, (
	newName = Head[distribution] /. {
		NormalDistribution -> "\[ScriptCapitalN]",
		GammaDistribution -> "\[CapitalGamma]",
		ExponentialDistribution -> "Exp",
		BetaDistribution -> "\[Beta]",
        BinomialDistribution -> "Bin",
        GeometricDistribution -> "Geo",
        PoissonDistribution -> "Poisson",
        ChiSquareDistribution -> "\!\(\*SuperscriptBox[\(\[Chi]\), \(2\)]\)",
        CauchyDistribution -> "Cauchy",
        StudentTDistribution -> "t",
        UniformDistribution -> "\[ScriptCapitalU]",
        TriangularDistribution -> "\[CapitalDelta]"
    };
    If[newName === Head[distribution], ToString[Head@distribution], newName] <> "(" <> StringRiffle[ToString /@ Level[distribution, 1], ","] <> ")"
)]

        
(* Random Number Generation *)

Options[qqPlot] = Join[
   {
        QQDomain -> {-30, 30},
        Axes -> False,
        PlotRangePadding -> 0.01,
        ImageSize -> 100, 
        Frame -> True,
        FrameTicks -> None
    },
   Options[ParametricPlot]
];
qqPlot[distX_, distY_, opts : OptionsPattern[]] := (
    ParametricPlot[
        Evaluate[#],
        {
            x,
            OptionValue["QQDomain"][[1]],
            OptionValue["QQDomain"][[2]]
        },
        PlotRange -> {{0, 1}, {0, 1}},
        FrameLabel -> distributionToString /@ {distX, distY},
        Evaluate[FilterRules[{opts, Options[qqPlot]}, Options[ParametricPlot]]]
    ] &
)@{CDF[distX, x], CDF[distY, x]}

Options[SVQuantilePlot] = Join[
    {
        QQDomain -> {-30, 30},
        CDFDomain -> {-5, 5},
        PlotRange -> All,
        AspectRatio -> 1,
        ImageSize -> 100,
        Frame -> True,
        FrameTicks -> None,
        Filling -> Axis,
        PlotStyle -> Gray
    },
    Options[Plot]
];
SVQuantilePlot[distributions_, opts: OptionsPattern[]] := Grid[
    Outer[
        If[#1 === #2,
            Plot[
                CDF[#1, x], {
                    x,
                    OptionValue["CDFDomain"][[1]],
                    OptionValue["CDFDomain"][[2]]
                },
                FrameLabel -> {"CDF " <> distributionToString[#1], ""},
                Evaluate[FilterRules[{opts, Options[SVQuantilePlot]}, Options[Plot]]]
            ],
            qqPlot[
                #1,
                #2,
                Evaluate[FilterRules[{opts, Options[SVQuantilePlot]}, Options[qqPlot]]]
            ]
        ] &,
        distributions,
        distributions
    ]
]

Options[SVQuantilePlot3D] = Join[
   {
        QQDomain -> {-30, 30},
        PlotRange -> {{0, 1}, {0, 1}, {0, 1}},
        PlotRangePadding -> 0.01,
        ImageSize -> 150, 
        RotationAction -> "Clip",
        SphericalRegion -> True
    },
   Options[ParametricPlot]
];
SVQuantilePlot3D[distX_, distY_, distZ_, opts: OptionsPattern[]] := (
    ParametricPlot3D[
        Evaluate[{CDF[distX, x], CDF[distY, x], CDF[distZ, x]}],
        {
            x,
            OptionValue["QQDomain"][[1]],
            OptionValue["QQDomain"][[2]]
        },
        AxesLabel -> distributionToString /@ {distX, distY, distZ},
        Evaluate[FilterRules[{opts, Options[SVQuantilePlot3D]}, Options[ParametricPlot3D]]]
    ]
)


End[]

EndPackage[]
