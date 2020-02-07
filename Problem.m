(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Problem`"];


setProblemProperties::usage = "setProblemProperties[nelx,nely,b,h,P,\[Mu],contacts] set the properites of the current problem.";
generateContacts::usage = "generateContacts[] sets randomly the contact mechanism to every block.";
setBoundaryConditions::usage = "setBoundaryConditions[loads] sets the loads to be applied.";
solveProblem::usage = "solveProblem[] applies the loads, computes the solution.";
solveProblemAndDisplay::usage = "solveProblemAndDisplay[filter] applies the loads, computes the solution and displays the wall colored with the selected filter.";
displayWallWithFilter::usage = "displayWallWithFilter[filter] displays the wall colored with the selected filter.";
nelx;nely;b;h;P;\[Mu];contacts;
\[Sigma]v;\[Sigma]h;loads;eqCheck;


Begin["`Private`"];
Needs["ProbabilisticBricks`Wall`"];


initStressVectors[$nelx_, $nely_] := Module[{},
	\[Sigma]v = Table[{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {totalBlocks}];
	\[Sigma]h = Table[{0, 0, 0, 0}, {totalBlocks + $nely}];
];


setProblemProperties[$nelx_, $nely_, $b_, $h_, $P_, $\[Mu]_] := Module[{},
	nelx = $nelx;nely = $nely;
	b = $b;h = $h;
	P = $P;\[Mu] = $\[Mu];
	(*compute number of total blocks*)
	If[EvenQ[$nely],
		totalBlocks = getBlockNum[{$nely, $nelx - 1}];,
		totalBlocks = getBlockNum[{$nely, $nelx}];
	];
];


generateContacts[] := Module[{},
	contacts = RandomInteger[{1, 3}, totalBlocks]
];


setBoundaryConditions[$loads_] := Module[{},
	loads = $loads;
];


applyLoads[] := Module[{j},
	initStressVectors[nelx, nely];
	For[j = 1, j <= nelx, j++,
		\[Sigma]v[[j]] = Join[loads[[6(j - 1) + 1 ;; 6j]], {0, 0, 0, 0, 0, 0}];
	];
];


solveProblem[] := Module[{},
	applyLoads[];
	eqCheck = solveWall[];
];


displayWallWithFilter[filter_] := Module[{},
	displayWall[filter]
];


solveProblemAndDisplay[filter_] := Module[{},
	applyLoads[];
	eqCheck = solveWall[];
	displayWall[filter]
];


End[];


EndPackage[];
