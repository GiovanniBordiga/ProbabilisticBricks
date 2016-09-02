(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Wall`"];


solveWall::usage="solveWall[] computes the stress vectors for the wall.";
displayWall::usage="displayWall[] generates a drawing of the wall showing the stress path.";


Begin["`Private`"];
Needs["ProbabilisticBricks`Block`"];
Needs["ProbabilisticBricks`Problem`"]


getBlockLoads[blockPos_]:=Module[{pvnew,Ns,Ts,Ncs,Tcs,Ncd,Tcd,Nd,Td,nBlock,blockULPos,blockURPos,nBlockUL,nBlockUR},
nBlock=(blockPos[[1]]-1)nelx+blockPos[[2]];

If[blockPos[[1]]==1,

(*first row*)
pvnew=\[Sigma]v[[nBlock]];,

(*other rows*)
(*get upper blocks' positions*)
If[EvenQ[blockPos[[1]]],
blockULPos={blockPos[[1]]-1,blockPos[[2]]-1};blockURPos={blockPos[[1]]-1,blockPos[[2]]};,
blockULPos={blockPos[[1]]-1,blockPos[[2]]};blockURPos={blockPos[[1]]-1,blockPos[[2]]+1};
];
nBlockUL=(blockULPos[[1]]-1)nelx+blockULPos[[2]];
nBlockUR=(blockURPos[[1]]-1)nelx+blockURPos[[2]];

(*get loads acting on top from the reactions of the upper blocks*)
If[blockULPos[[2]]!=0&&blockURPos[[2]]<=nelx,
(*block is internal*)
Ncs=\[Sigma]v[[nBlockUL,11]];Tcs=\[Sigma]v[[nBlockUL,12]];
Ncd=\[Sigma]v[[nBlockUR,7]];Tcd=\[Sigma]v[[nBlockUR,8]];
If[contacts[[blockULPos[[1]],blockULPos[[2]]]]==3,
Ns=\[Sigma]v[[nBlockUL,9]];Ts=\[Sigma]v[[nBlockUL,10]];,
Ns=0;Ts=0;];
If[contacts[[blockURPos[[1]],blockURPos[[2]]]]==2,
Nd=\[Sigma]v[[nBlockUR,9]];Td=\[Sigma]v[[nBlockUR,10]];,
Nd=0;Td=0;];,
(*block in not internal*)
If[blockULPos[[2]]==0,
(*block protruding to the left*)
Ns=0;Ts=0;Ncs=0;Tcs=0;
Ncd=\[Sigma]v[[nBlockUR,7]];Tcd=\[Sigma]v[[nBlockUR,8]];
If[contacts[[blockURPos[[1]],blockURPos[[2]]]]==2,
Nd=\[Sigma]v[[nBlockUR,9]];Td=\[Sigma]v[[nBlockUR,10]];,
Nd=0;Td=0;];,
(*block protruding to the right*)
Nd=0;Td=0;Ncd=0;Tcd=0;
Ncs=\[Sigma]v[[nBlockUL,11]];Tcs=\[Sigma]v[[nBlockUL,12]];
If[contacts[[blockULPos[[1]],blockULPos[[2]]]]==3,
Ns=\[Sigma]v[[nBlockUL,9]];Ts=\[Sigma]v[[nBlockUL,10]];,
Ns=0;Ts=0;];
];
];
pvnew={Ns,Ts,Ncs+Ncd,Tcs+Tcd,Nd,Td,0,0,0,0,0,0};
];

Join[\[Sigma]h[[nBlock]],pvnew,\[Sigma]h[[nBlock+1]]]];


updateStress[pBlock_,blockPos_]:=Module[{nBlock},
nBlock=(blockPos[[1]]-1)nelx+blockPos[[2]];
\[Sigma]v[[nBlock]]=pBlock[[5;;16]];
\[Sigma]h[[nBlock+blockPos[[1]]-1]]=pBlock[[1;;4]];(*interface on the left side of the current block*)
\[Sigma]h[[nBlock+blockPos[[1]]]]=pBlock[[17;;20]];(*interface on the right side of the current block*)
];


checkRowEquilibrium[pBlock_]:=pBlock[[17;;20]]=={0,0,0,0};


solveWall[]:=Module[{blockLoads,pBlock,i,j},
eqCheck=True;
For[i=1,i<=nely&&eqCheck,i++,
For[j=1,j<=nelx,j++,
blockLoads=getBlockLoads[{i,j}];

pBlock=solveBlock[blockLoads,contacts[[i,j]]];

updateStress[pBlock,{i,j}];
];
eqCheck=checkRowEquilibrium[pBlock];
];
];


displayWall[]:=Module[{blocks,stressAvg,interfaces,frictionRatio,blockLoads,ptBL,ptBR,ptTL,ptTR,i,j},
blocks={}; stressAvg={};interfaces={};
For[i=1,i<=nely,i++,
For[j=1,j<=nelx,j++,
ptBL={(j -1)b+Mod[i,2]b/2,(nely-i)h};(*bottom left vertex's coordinates of the current block*)
ptBR={j b+Mod[i,2]b/2,(nely-i)h};(*bottom right vertex's coordinates of the current block*)
ptTL={(j -1)b+Mod[i,2]b/2,(nely-i+1)h};(*top left vertex's coordinates of the current block*)
ptTR={j b+Mod[i,2]b/2,(nely-i+1)h};(*top right vertex's coordinates of the current block*)
(*create graphical elements colored using stress measure*)
blockLoads=getBlockLoads[{i,j}];
AppendTo[stressAvg,Norm[blockLoads]];(*stress measure defined as the norm of 'blockLoads'*)
AppendTo[blocks,{EdgeForm[{Black}],GrayLevel[0.5],Rectangle[ptBL,ptTR]}];

frictionRatio=Piecewise[{{Abs[Total[blockLoads[[12;;16;;2]]]]/(\[Mu] Total[blockLoads[[11;;15;;2]]]),Total[blockLoads[[11;;15;;2]]]>0}}];
AppendTo[interfaces,{RGBColor[frictionRatio,0,0],Line[{ptBL,ptBR}]}];(*friction on the base*)
(*frictionRatio=Piecewise[{{Abs[Total[blockLoads[[18;;20;;2]]]]/(\[Mu] Total[blockLoads[[17;;19;;2]]]),Total[blockLoads[[17;;19;;2]]]>0}}];
AppendTo[interfaces,{RGBColor[frictionRatio,0,0],Line[{ptBR,ptTR}]}];*)(*friction on the right side*)
];
];
blocks[[;;,2]]=GrayLevel/@(stressAvg/Max[stressAvg]);(*assign GrayLevel based on strees*)

Show[Graphics[blocks],Graphics[interfaces]]];


End[];


EndPackage[];
