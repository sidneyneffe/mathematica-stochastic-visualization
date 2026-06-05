
BeginPackage["StochasticVisualization`"]

CDFDomain::usage = "CDFDomain is an option for SVQuantilePlot and defines the parameter space of the CDF plot."

QQDomain::usage = "QQDomain is an option for SVQuantilePlot and SVQuantilePlot3D and defines the parameter space of the QQ plot."

SVQuantilePlot::usage = "SVQuantilePlot[{d1, d2, ...}] plots CDF and Quantile-Quantile functions for a list of distributions.\nSVQuantilePlot[{d1, d2, ...}, CDFDomain -> {x1, x2}, QQDomain -> {y1, y2}] plots with specified parameter ranges."

SVQuantilePlot3D::usage = "SVQuantilePlot3D[distX, distY, distZ, QQDomain->{-30, 30}] shows a 3D plot of three distributions with the specified parameter range."

NSamples::usage = "NSamples is an option for SVRejectionPlot and defines the number of samples"

SVRejectionPlot::usage = "SVRejectionPlot[f, range, gDistribution, constant, NSamples->500] applies the acception-rejection method for a function f and g (with distribution gDistribution) and constant C."


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

Options[SVRejectionPlot] = {
    PlotStyle -> RGBColor["#3498DB"],
    NSamples -> 500
};
SVRejectionPlot[function_, range_, gDistribution_, C_, opts: OptionsPattern[]] := Module[{X, y, u, g, ratio, selection}, (
    f = function;
    g = PDF[gDistribution];
    n = OptionValue["NSamples"];
    (* y = InverseCDF[gDistribution, RandomReal[{0, 1}, n]]; *)
    y = RandomVariate[gDistribution, n];
    u = RandomReal[{0, 1}, n];
    selection = {
        Select[
            Thread[{y, u}],
            #[[2]] <= (f /. (range[[1]] -> #[[1]])) / C / g[#[[1]]] &
        ],
        Select[
            Thread[{y, u}],
            #[[2]] > (f /. (range[[1]] -> #[[1]])) / C / g[#[[1]]] &
        ]
    };
    
    X = First /@ selection[[1]];
    ratio = 1/C * NIntegrate[
        f /. range[[1]] -> t,
        {t, -\[Infinity], \[Infinity]}
    ];

    Column[{
        Plot[
            Evaluate @ {f, C g[range[[1]]]},
            range,
            ImageSize -> 200,
            Axes -> {True, False},
            AspectRatio -> 1/2,
            PlotLegends -> Placed[{
                "\!\(\*FormBox[\(\"\<\>\"\*FractionBox[\"f\", TemplateBox[{\n\"f\"},\n\"Abs\"]]\),TraditionalForm]\)",
                "C g"
            }, {Right, Top}],
            PlotRange -> All,
            PlotLabel -> "Acception Rate: " <> ToString @ PercentForm[ratio, {\[Infinity], 0}],
            PlotRangePadding -> 0.1,
            PlotStyle -> {
                OptionValue["PlotStyle"],
                Opacity[0.9, Gray]
            }
        ],
        NumberLinePlot[
            {X, y},
            PlotStyle -> {Opacity[0.1, OptionValue["PlotStyle"]], Opacity[0.1, Gray]},
            ImageSize -> 200,
            PlotRange -> {range[[2]], range[[3]]},
            PlotRangePadding -> 0.1
        ],
        Show[{
            Plot[
                If[f == 0, 1, f / (C g[range[[1]]])],
                range,
                PlotStyle -> {Dashed, OptionValue["PlotStyle"]},
                AspectRatio -> 1/6,
                ImageSize -> 200,
                Axes -> {True, False},
                PlotRange -> {0, 1},
                PlotRangePadding -> 0.1
            ],
            ListPlot[
                selection,
                PlotMarkers -> {"•"},
                PlotStyle -> {Opacity[0.5, OptionValue["PlotStyle"]], Opacity[0.5, Gray]}
            ]
        }]
    }]
)]


End[]

EndPackage[]
