
BeginPackage["StochasticVisualization`"]

CDFDomain::usage = "CDFDomain is an option for SVQuantilePlot and defines the parameter space of the CDF plot."

QQDomain::usage = "QQDomain is an option for SVQuantilePlot and SVQuantilePlot3D and defines the parameter space of the QQ plot."

SVQuantilePlot::usage = "SVQuantilePlot[{d1, d2, ...}] plots CDF and Quantile-Quantile functions for a list of distributions.\nSVQuantilePlot[{d1, d2, ...}, CDFDomain -> {x1, x2}, QQDomain -> {y1, y2}] plots with specified parameter ranges."

SVQuantilePlot3D::usage = "SVQuantilePlot3D[distX, distY, distZ, QQDomain->{-30, 30}] shows a 3D plot of three distributions with the specified parameter range."

NSamples::usage = "NSamples is an option for SVRejectionPlot and defines the number of samples"

SVRejectionPlot::usage = "SVRejectionPlot[f, range, gDistribution, constant, NSamples->500] applies the acception-rejection method for a function f and g (with distribution gDistribution) and constant C."

SVDiscreteMarkovChainSimulation::usage = "SVDiscreteMarkovChainSimulation simulates a Markov chain."

SVRandomWalkSimulation::usage = "SVRandomWalkSimulation simulates a random walk as a Markov chain."

SVDistributionPlot::usage = "SVDistributionPlot[distribution, x] plots a distribution (discrete/continuous) and displays additional information."

SVBiDistributionPlot::usage = "SVDistributionPlot[distribution] plots a two-dimensional distribution and the marginal distributions."

SVConditionalBiNormalPlot::usage = "SVConditionalBiNormalPlot[mean, varianceMatrix, yCondition] plots a conditional two-dimensional normal distribution."

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


(* MARKOV CHAINS *)

analyseStochasticMatrix[matrix_] := Module[{system, positive}, (
    positive = Function[vector, If[
        AllTrue[vector, RealValuedNumberQ[#] && # <= 0 &],
        -vector,
        vector
    ]];
    system = <|
        #[[1, 1]] -> (
            MatrixForm @ positive @ N @ Chop @ Normalize[#, Norm[#, 1] &] &
        )
        /@
            #[[;;, 2]] &
        /@
            Gather[
                Thread @ Eigensystem[Transpose[ matrix]],
                #1[[1]] == #2[[1]] &
            ]
    |>;
    ComplexListPlot[
        Tooltip[#, system[#]] & /@ Keys[system],
        PlotRange -> {{-1, 1}, {-1, 1}},
        AspectRatio -> 1,
        ImageSize -> 80,
        Ticks -> False,
        Prolog -> {Opacity[0.2, LightGray], Disk[{0, 0}, 1]},
        PlotRangePadding -> 0.05,
        PlotMarkers -> {Automatic, Small}
    ]
)]

analysePeriodicity[matrix_, steps_] := (
    First
    /@ Select[#, #[[2]] > 0 &] &
    /@ Transpose[
        Thread[
            {#, Diagonal @ MatrixPower[matrix, #]}
        ] & /@ Range[steps]
    ]
)

graphStochasticMatrix[matrix_, layout_: "SpringEmbedding"] := Module[{periodicity, n}, (
    periodicity = analysePeriodicity[matrix, 15];
    AdjacencyGraph[
        Map[
            If[# == 0, 0, 1] &,
            matrix - DiagonalMatrix @ Diagonal[matrix], {2}
        ],
        PlotTheme -> "Classic",
        ImageSize -> 90,
        AspectRatio -> 1,
        GraphLayout -> layout,
        PlotRangePadding -> Scaled[.2],
        VertexLabels -> Table[
            i -> Tooltip[
                i,
                "Period (15 steps): " <> ToString[
                    GCD @@ periodicity[[i]]
                ]
            ],
            {i, 1, Length[matrix]}
        ]
    ]
)]

analyseConvergence[matrix_] := Module[{positive, vectors, table}, (
    positive = Function[vector, If[
        AllTrue[vector, RealValuedNumberQ[#] && # <= 0 &],
        -vector,
        vector
    ]];
    vectors = positive @ N @ Chop @ Normalize[#, Norm[#, 1] &] &
    /@ Select[
        Thread @ Eigensystem[Transpose[matrix]],
        #[[1]] == 1 &
    ][[;;, 2]];

    If[
        Length[vectors] == 1,
        table = Table[
            Norm[
                Array[
                    If[# == i, 1, 0] &,
                    Length[matrix]
                ] . MatrixPower[matrix, n] - vectors[[1]],
                1
            ],
            {i, 1, Length[matrix]},
            {n, 1, 30}
        ];
        ListPlot[
            table,
            Joined -> True,
            ImageSize -> 100,
            AspectRatio -> 1,
            PlotLegends -> Placed[
                Style[
                    "\!\(\*FormBox[SubscriptBox[TemplateBox[{\"\\\"\\\\!\\\\(\\\\*SubscriptBox[\\\\(\[Delta]\\\\),\\\\(x\\\\)]\\\\)\\\\!\\\\(\\\\*SuperscriptBox[\\\\(P\\\\),\\\\(n\\\\)]\\\\)-\[Pi]\\\"\"},\n\"Norm\"], \"1\"], TraditionalForm]\)",
                    "Text",
                    FontSize -> 12
                ],
                {Right, Top}
            ],
            PlotRange -> {0, 1},
            PlotRangePadding -> Scaled[.05],
            PlotStyle -> Opacity[0.5],
            Frame -> True,
            FrameTicks -> {{{0, 0.5, 1}, None}, {{0, 10, 20, 30}, None}}
        ],
        Style["Nicht eindeutig", "Text"]
    ]
)]

Options[SVDiscreteMarkovChainSimulation] = Join[
    {
        ChartLayout -> "Stacked",
        ChartStyle -> {Black, RGBColor["#3498DB"], Orange, Green},
        AspectRatio -> 1/5,
        ImageSize -> 450,
        PlotLabel -> "Simulation",
        Axes -> False,
        PlotRangePadding -> None,
        BarSpacing -> 0,
        Frame -> False,
        ChartBaseStyle -> EdgeForm[None]
    },
    Options[BarChart]
];
SVDiscreteMarkovChainSimulation[transitionMatrix_, initialValues_, steps_, opts: OptionsPattern[]] := Module[{n = Length[transitionMatrix], step, simulation, counts}, (
    step = Function[list, RandomChoice[
        transitionMatrix[[#]] -> Range[n]
    ] & /@ list];

    simulation = NestList[step, initialValues, steps];
    counts = Table[Count[r, i], {r, simulation}, {i, Range[n]}];
    
    Grid[{
        {
            If[n <= 5, Style["t=0", "Text"], ""],
            Style[
                OptionValue["PlotLabel"],
                "Text"
            ],
            If[n <= 5, Style["t="<>ToString[steps], "Text"], ""],
            If[n <= 10, Style["Eigenvalues", "Text"], ""],
            If[n <= 8, Style["Graph", "Text"], ""],
            Style["Convergence", "Text"]
        },
        {
            If[
                n <= 5,
                MatrixForm @ Chop @ N[1 / Length[initialValues] * Table[
                    Count[initialValues, n],
                    {n, Range[n]}
                ]],
                ""
            ],
            BarChart[
                counts,
                PlotLabel -> "",
                Evaluate[FilterRules[{opts, Options[SVDiscreteMarkovChainSimulation]}, Options[BarChart]]]
            ],
            If[
                n <= 5,
                MatrixForm @ Chop @ N[1 / Length[initialValues] * Table[
                    Count[initialValues, n],
                    {n, Range[n]}
                ] . MatrixPower[transitionMatrix, steps]],
                ""
            ],
            If[
                n <= 10,
                analyseStochasticMatrix[transitionMatrix],
                ""
            ],
            If[
                n <= 8,
                graphStochasticMatrix[transitionMatrix],
                ""
            ],
            analyseConvergence[transitionMatrix]
        }
    }]
)]

randomWalkMatrix[n_, a_] := Normal[
    SparseArray[
        {
            {1, 1} | {n, n} -> 1 - a,
            {i_, i_} -> 1 - 2 a,
            {i_, j_} /; Abs[i - j] == 1 -> a
        },
        {n, n}
    ]
]

Options[SVRandomWalkSimulation] = Join[
    {
        ChartLayout -> "Stacked",
        AspectRatio -> 1/5,
        ImageSize -> 450,
        PlotLabel -> "Simulation",
        Axes -> False,
        PlotRangePadding -> None,
        BarSpacing -> 0,
        Frame -> False,
        ChartBaseStyle -> EdgeForm[None]
    },
    Options[BarChart]
];
SVRandomWalkSimulation[n_, p_, initialValues_, steps_, opts: OptionsPattern[]] := Module[{transitionMatrix, step, simulation, counts, colors}, (
    transitionMatrix = randomWalkMatrix[n, p];
    step = Function[list, RandomChoice[
        transitionMatrix[[#]] -> Range[n]
    ] & /@ list];

    simulation = NestList[step, initialValues, steps];
    counts = Table[Count[r, i], {r, simulation}, {i, Range[n]}];

    colors = ColorData["SolarColors"][(# - 1) / n] & /@ Range[n];
    
    Grid[{
        {
            Style[
                OptionValue["PlotLabel"],
                "Text"
            ],
            Style["Transition Matrix", "Text"]
        },
        {
            BarChart[
                counts,
                PlotLabel -> "",
                ChartStyle -> colors,
                Evaluate[FilterRules[{opts, Options[SVDiscreteMarkovChainSimulation]}, Options[BarChart]]]
            ],
            MatrixForm[{
                {1 - p, p, 0, "\[Ellipsis]", 0, 0},
                {p, 1 - 2 p, p, "\[Ellipsis]", 0, 0},
                {0, p, 1 - 2 p, "\[Ellipsis]", 0, 0},
                {"\[Ellipsis]", "\[Ellipsis]", "\[Ellipsis]", "\[Ellipsis]", "\[Ellipsis]", "\[Ellipsis]"} ,
                {0, 0, 0, "\[Ellipsis]", p, 1 - p}
            }]
        }
    }]
)]



(* PROBABILITY DISTRIBUTIONS *)

SVDistributionPlot[distribution_, x_] := Module[{pdf, char, moment,formatTerm}, (
    pdf = PDF[distribution, x];
    char = CharacteristicFunction[distribution, x];
    moment = MomentGeneratingFunction[distribution, x];

    discreteQ = Statistics`Library`DiscreteUnivariateDistributionQ[distribution];

    formatTerm = Function[
        term,
        Style[
            FullSimplify[term, Assumptions -> {x \[Element] Reals}],
            FontSize -> 10
        ]
    ];
    
    Grid[{
        Style[#,"Text"] &
        /@ {
            If[discreteQ, "PMF", "PDF"],
            "",
            "Char. Func.",
            "",
            "Moment gen. Func.",
            ""
        },
        {
            If[
                discreteQ,
                DiscretePlot[
                    pdf,
                    {x,-10, 10},
                    PlotRange -> {{-10, 10}, All},
                    ImageSize -> 100,
                    PlotRangePadding -> Scaled[.05],
                    Axes -> {True, False},
                    ExtentSize -> 1/2
                ],
                Plot[
                    pdf,
                    {x,-8, 8},
                    PlotRange -> {{-10, 10}, All},
                    ImageSize -> 100,
                    PlotRangePadding -> Scaled[.1],
                    Axes -> {True, False},
                    Filling -> Axis
                ]
            ],
            formatTerm[pdf],
            AbsArgPlot[
                char,
                {x,-8, 8},
                PlotRange -> All,
                ImageSize -> 100,
                Ticks -> {True, False}
            ],
            formatTerm[char],
            LogPlot[
                moment,
                {x,-2, 2},
                PlotRange -> All,
                ImageSize -> 100,
                Ticks -> {True, False}
            ],
            formatTerm[moment]
        }
    }]
)]

Options[SVBiDistributionPlot] = Join[
   {
        ImageSize -> 200,
        PlotRange -> {{-4, 4}, {-4, 4}},
        PlotPoints -> 50,
        ColorFunction -> "GrayYellowTones",
        Axes -> False,
        FrameTicks -> False,
        PlotRangePadding -> 0
    },
   Options[DensityPlot]
];
SVBiDistributionPlot[distribution_, opts: OptionsPattern[]] := Module[{pdfX, pdfY, pdfXY, ranges, colorFunction}, (
    pdfXY = PDF[distribution];
    pdfX = PDF[MarginalDistribution[distribution, 1]];
    pdfY = PDF[MarginalDistribution[distribution, 2]];
    ranges = OptionValue["PlotRange"];
    colorFunction = If[
        StringQ[OptionValue["ColorFunction"]],
        ColorData[OptionValue["ColorFunction"]],
        OptionValue["ColorFunction"]
    ];
    
    Grid[{
        {
            "",
            ParametricPlot[
                {t, u * pdfX[t]},
                {t, ranges[[1, 1]], ranges[[1, 2]]},
                {u, 0, 1},
                ImageSize -> OptionValue["ImageSize"],
                AspectRatio -> 1/6,
                PlotPoints -> 30,
                Axes -> False,
                Frame -> False,
                PlotRange -> {ranges[[1]], {0, All}},
                PlotRangePadding -> {0, Scaled[.1]},
                ColorFunction -> (colorFunction[#2] &)
            ]
        },
        {
            ParametricPlot[
                {-u * pdfY[t], t},
                {t, ranges[[2, 1]], ranges[[2, 2]]},
                {u, 0, 1},
                ImageSize -> OptionValue["ImageSize"] / 6,
                AspectRatio -> 6,
                Axes -> False,
                Frame -> False,
                PlotRange -> {{All, 0}, ranges[[2]]},
                PlotRangePadding -> {Scaled[.1], 0},
                ColorFunction -> (colorFunction[1-#1] &)
            ],
            DensityPlot[
                pdfXY[{t1, t2}],
                {t1, ranges[[1, 1]], ranges[[1, 2]]},
                {t2, ranges[[2, 1]], ranges[[2, 2]]},
                Evaluate @ FilterRules[
                    {opts, Options[SVBiDistributionPlot]},
                    Options[DensityPlot]
                ]
            ]
        }
    }]
)]

Options[SVConditionalBiNormalPlot] = Join[
   {
        ImageSize -> 200,
        PlotRange -> {{-4, 4}, {-4, 4}},
        PlotPoints -> 50,
        ColorFunction -> "GrayYellowTones",
        Axes -> False,
        FrameTicks -> False,
        PlotRangePadding -> 0
    },
   Options[DensityPlot]
];
SVConditionalBiNormalPlot[mean_, varianceMatrix_, yCondition_, opts: OptionsPattern[]] := Module[{dist, ranges, colorFunction, distPlot}, (
    dist = MultinormalDistribution[mean, varianceMatrix];
    ranges = OptionValue["PlotRange"];
    colorFunction = If[
        StringQ[OptionValue["ColorFunction"]],
        ColorData[OptionValue["ColorFunction"]],
        OptionValue["ColorFunction"]
    ];

    distPlot = Identity @@ SVBiDistributionPlot[
        dist,
        Epilog -> {
            Dashed,
            White,
            Line[{
                {ranges[[1, 1]], yCondition},
                {ranges[[1, 2]], yCondition}
            }]
        },
        FilterRules[
            {opts, Options[SVConditionalBiNormalPlot]},
            Options[SVBiDistributionPlot]
        ]
    ];

    conditionalMean = mean[[1]] + varianceMatrix[[1, 2]]/varianceMatrix[[2, 2]] (yCondition - mean[[2]]);

    conditionalVariance = varianceMatrix[[1, 1]] - varianceMatrix[[1, 2]]^2 / varianceMatrix[[2, 2]];

    conditionalDistribution = NormalDistribution[conditionalMean, conditionalVariance];

    conditionalPlot = ParametricPlot[
        {t, u * Evaluate@PDF[conditionalDistribution, t]},
        {t, ranges[[1, 1]], ranges[[1, 2]]},
        {u, 0, 1},
        ImageSize -> OptionValue["ImageSize"],
        AspectRatio -> 1,
        PlotPoints -> 30,
        Axes -> {True, False},
        Frame -> False,
        PlotRange -> All,
        PlotRangePadding -> {0, Scaled[.1]},
        ColorFunction -> (colorFunction[#2] &),
        PlotLabel -> Row[{
            "\[ScriptCapitalN](",
            NumberForm[conditionalMean, {\[Infinity], 1}],
            ",",
            NumberForm[Sqrt[conditionalVariance], {\[Infinity], 1}],
            ")"
        }]
    ];

    Grid[{
        {
            distPlot[[1, 1]],
            distPlot[[1, 2]],
            Style["Condition: y=" <> ToString[yCondition], "Text"]
        },
        {
            distPlot[[2, 1]],
            distPlot[[2, 2]],
            conditionalPlot
        }
    }]
)]

End[]

EndPackage[]
