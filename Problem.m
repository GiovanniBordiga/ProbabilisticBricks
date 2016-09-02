(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Problem`"];


setProblemProperties::usage="setProblemProperties[nelx,nely,b,h,P,\[Mu],contacts] set the properites of the current problem.";
generateContacts::usage="generateContacts[] sets randomly the contact mechanism to every block.";
setBoundaryConditions::usage="setBoundaryConditions[loads] sets the loads to be applied.";
solveProblem::usage="solveProblem[] applies the loads, computes the solution and displays the wall.";
nelx;nely;b;h;P;\[Mu];contacts;
\[Sigma]v;\[Sigma]h;loads;


Begin["`Private`"];
Needs["ProbabilisticBricks`Wall`"];


(*Properties initialization*)
nelx=80;nely=50;b=2;h=1;P=0.1;\[Mu]=0.8;
\[Sigma]v=Table[{0,0,0,0,0,0,0,0,0,0,0,0},{nely nelx}];
\[Sigma]h=Table[{0,0,0,0},{(nelx+1)nely}];


setProblemProperties[$nelx_,$nely_,$b_,$h_,$P_,$\[Mu]_,$contacts_]:=Module[{},
nelx=$nelx;nely=$nely;
b=$b;h=$h;
contacts=$contacts;
P=$P;\[Mu]=$\[Mu];
\[Sigma]v=Table[{0,0,0,0,0,0,0,0,0,0,0,0},{$nely $nelx}];
\[Sigma]h=Table[{0,0,0,0},{($nelx+1)$nely}];
];


generateContacts[]:=Module[{},
contacts=RandomInteger[{1,3},{nely ,nelx}];
];


setBoundaryConditions[$loads_]:=Module[{},
loads=$loads;
];


applyLoads[]:=Module[{j},
For[j=1,j<=nelx,j++,
\[Sigma]v[[j]]=Join[loads[[6(j-1)+1;;6j]],{0,0,0,0,0,0}];];
];


solveProblem[]:=Module[{},
applyLoads[];
solveWall[];
displayWall[]
];


End[];


EndPackage[];
