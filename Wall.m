(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Wall`"];


getBlockNum::usage="getBlockNum[{nRow,j}] returns the block number corresponding to the postion {nRow,j}.";
solveWall::usage="solveWall[] computes the stress vectors for the wall.";
displayWall::usage="displayWall[] generates a drawing of the wall showing the stress path.";


Begin["`Private`"];
Needs["ProbabilisticBricks`Block`"];
Needs["ProbabilisticBricks`Problem`"]


getBlockNum[{nRow_,j_}]:=Module[{nBlock},
If[EvenQ[nRow],
nBlock=nelx nRow/2+(nelx-1)(nRow/2-1);,
nBlock=nelx (nRow-1)/2+(nelx-1)(nRow-1)/2;
];

nBlock+=j
];

isBlockOnRightEdge[{nRow_,j_}]:=Module[{},
If[EvenQ[nRow],
j==nelx-1,
j==nelx]
];

isBlockOnLeftEdge[{nRow_,j_}]:=j==1;


findStartBlocks[nRow_]:=Module[{j,prev,next,signs,rowLoads,startBlocks},
rowLoads=Flatten[\[Sigma]v[[getBlockNum[{nRow,1}];;getBlockNum[{nRow+1,0}],1;;6]]];
startBlocks={1};
signs={Sign[Total[rowLoads[[2;;6;;2]]]]};
For[j=2,j<=nelx,j++,
prev=Sign[Total[rowLoads[[6(j-1-1)+2;;6(j-1);;2]]]];
next=Sign[Total[rowLoads[[6(j-1)+2;;6j;;2]]]];
If[prev next<=0&&next!=0,
AppendTo[signs,next];
AppendTo[startBlocks,j-1];
AppendTo[startBlocks,j];
];
];
AppendTo[startBlocks,nelx];
startBlocks=Partition[startBlocks,2];
For[j=1,j<=Length[signs],j++,
If[signs[[j]]<0,
startBlocks[[j,{1,2}]]=startBlocks[[j,{2,1}]];
];
];

startBlocks
];


getBlockLoads[{nRow_,j_}]:=Module[{pvnew,Ns,Ts,Ncs,Tcs,Ncd,Tcd,Nd,Td,nBlock,blockULPos,blockURPos,nBlockUL,nBlockUR},nBlock=getBlockNum[{nRow,j}];

If[nRow==1,

(*first row*)
pvnew=\[Sigma]v[[nBlock]];,

(*other rows*)
(*get upper blocks' positions*)
If[EvenQ[nRow],
blockULPos={nRow-1,j};blockURPos={nRow-1,j+1};,
blockULPos={nRow-1,j-1};blockURPos={nRow-1,j};
(*If[isBlockOnLeftEdge[{nRow,j}],
blockULPos={0,0};
];
If[isBlockOnRightEdge[{nRow,j}],
blockURPos={0,0};
];*)
];
nBlockUL=getBlockNum[blockULPos];
nBlockUR=getBlockNum[blockURPos];

(*get loads acting on top from the reactions of the upper blocks*)
If[!(isBlockOnLeftEdge[{nRow,j}]||isBlockOnRightEdge[{nRow,j}]),
(*block is internal*)
Ncs=\[Sigma]v[[nBlockUL,11]];Tcs=\[Sigma]v[[nBlockUL,12]];
Ncd=\[Sigma]v[[nBlockUR,7]];Tcd=\[Sigma]v[[nBlockUR,8]];
If[contacts[[nBlockUL]]==3,
Ns=\[Sigma]v[[nBlockUL,9]];Ts=\[Sigma]v[[nBlockUL,10]];,
Ns=0;Ts=0;
];
If[contacts[[nBlockUR]]==2,
Nd=\[Sigma]v[[nBlockUR,9]];Td=\[Sigma]v[[nBlockUR,10]];,
Nd=0;Td=0;
];,

(*block in not internal*)
If[isBlockOnLeftEdge[{nRow,j}],(*TODO: modify to support edge blocks!*)
(*block on the left edge*)
Ncd=\[Sigma]v[[nBlockUR,7]];Tcd=\[Sigma]v[[nBlockUR,8]];
If[contacts[[nBlockUR]]==2,
Nd=\[Sigma]v[[nBlockUR,9]];Td=\[Sigma]v[[nBlockUR,10]];,
Nd=0;Td=0;
];
If[EvenQ[nRow],
Ns=\[Sigma]v[[nBlockUL,9]];Ts=\[Sigma]v[[nBlockUL,10]];Ncs=\[Sigma]v[[nBlockUL,11]];Tcs=\[Sigma]v[[nBlockUL,12]];,
Ns=0;Ts=0;Ncs=0;Tcs=0;
];,
(*block on the right edge*)
Ncs=\[Sigma]v[[nBlockUL,11]];Tcs=\[Sigma]v[[nBlockUL,12]];
If[contacts[[nBlockUL]]==3,
Ns=\[Sigma]v[[nBlockUL,9]];Ts=\[Sigma]v[[nBlockUL,10]];,
Ns=0;Ts=0;
];
If[EvenQ[nRow],
Nd=\[Sigma]v[[nBlockUR,9]];Td=\[Sigma]v[[nBlockUR,10]];Ncd=\[Sigma]v[[nBlockUR,7]];Tcd=\[Sigma]v[[nBlockUR,8]];,
Nd=0;Td=0;Ncd=0;Tcd=0;
];
];
];
pvnew={Ns,Ts,Ncs+Ncd,Tcs+Tcd,Nd,Td,0,0,0,0,0,0};(*TODO: modify to support edge blocks!*)
];

Join[\[Sigma]h[[nBlock]],pvnew,\[Sigma]h[[nBlock+1]]]
];


solveRow[nRow_]:=Module[{j,startBlocks,blockNum,blockLoads,contact},
startBlocks=findStartBlocks[nRow];
For[j=1,j<=Length[startBlocks],j++,
Do[
blockLoads=getBlockLoads[{nRow,k}];
contact=contacts[[getBlockNum[{nRow,k}]]];
blockLoads=solveBlock[blockLoads,contact];
updateStress[blockLoads,{nRow,k}];,
{k,startBlocks[[j]]}
];
];
(*TODO: check equilibrium at the vertical interfaces*)
];


updateStress[pBlock_,blockPos_]:=Module[{blockNum},
blockNum=getBlockNum[{blockPos[[1]],blockPos[[2]]}];
\[Sigma]v[[blockNum]]=pBlock[[5;;16]];
\[Sigma]h[[blockNum+blockPos[[1]]-1]]=pBlock[[1;;4]];(*interface on the left side of the current block*)
\[Sigma]h[[blockNum+blockPos[[1]]]]=pBlock[[17;;20]];(*interface on the right side of the current block*)
];


checkRowEquilibrium[pBlock_]:=pBlock[[17;;20]]=={0,0,0,0};


solveWall[]:=Module[{i},
eqCheck=True;
For[i=1,i<=nely&&eqCheck,i++,
solveRow[i];
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
