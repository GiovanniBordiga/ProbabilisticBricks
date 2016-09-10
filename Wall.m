(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Wall`"];


getBlockNum::usage="getBlockNum[{nRow,j}] returns the block number corresponding to the postion {nRow,j}.";
getTotalBlocksInRow::usage="getTotalBlocksInRow[nRow] returns the number of blocks in the nRow row.";
isBlockOnLeftEdge::usage="isBlockOnLeftEdge[{nRow,j}] returns True if the block in position {nRow,j} is on the left edge of the wall.";
isBlockOnRightEdge::usage="isBlockOnRightEdge[{nRow,j}] returns True if the block in position {nRow,j} is on the right edge of the wall.";
isBlockHalved::usage="isBlockHalved[{nRow,j}] returns True if the block in position {nRow,j} is half the size of a normal block."
solveWall::usage="solveWall[] computes the stress vectors for the wall.";
displayWall::usage="displayWall[] generates a drawing of the wall showing the stress path.";


Begin["`Private`"];
Needs["ProbabilisticBricks`Block`"];
Needs["ProbabilisticBricks`Problem`"];


getBlockNum[{nRow_,j_}]:=Module[{nBlock},
If[EvenQ[nRow],
nBlock=nelx nRow/2+(nelx-1)(nRow/2-1);,
nBlock=nelx (nRow-1)/2+(nelx-1)(nRow-1)/2;
];

nBlock+=j
];

getTotalBlocksInRow[nRow_]:=Module[{},
If[EvenQ[nRow],
nelx-1,
nelx
]
];

isBlockOnRightEdge[{nRow_,j_}]:=Module[{},
If[EvenQ[nRow],
j==nelx-1,
j==nelx]
];

isBlockOnLeftEdge[{nRow_,j_}]:=j==1;

isBlockHalved[{nRow_,j_}]:=OddQ[nRow]&&(isBlockOnLeftEdge[{nRow,j}]||isBlockOnRightEdge[{nRow,j}]);


getBlockLoads[{nRow_,j_}]:=Module[{pvnew,Ns,Ts,Ncs,Tcs,Ncd,Tcd,Nd,Td,nBlock,blockULPos,blockURPos,nBlockUL,nBlockUR},
nBlock=getBlockNum[{nRow,j}];

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
pvnew={Ns,Ts,Ncs+Ncd,Tcs+Tcd,Nd,Td,0,0,0,0,0,0};
];

Join[\[Sigma]h[[nBlock]],pvnew,\[Sigma]h[[nBlock+1]]]
];


findSolvableBlockSequence[nRow_]:=Module[{j,shear,rowShears,blockSequence,len,direction,criticalBlocks,result},
rowShears=Total[\[Sigma]v[[getBlockNum[{nRow,1}];;getBlockNum[{nRow+1,0}],2;;6;;2]],{2}];
rowShears=SplitBy[rowShears,#>0&];
(*build block sequence and direction lists*)
blockSequence={};len=0;direction={};
For[j=1,j<=Length[rowShears],j++,
shear=Total[rowShears[[j]]];
AppendTo[blockSequence,Range[len+1,len+Length[rowShears[[j]]]]];
If[shear<0,
blockSequence[[j]]=Reverse[blockSequence[[j]]];
AppendTo[direction,-1];,
AppendTo[direction,1];
];
len=Length[Flatten[blockSequence]];
];
(*find potentially critical blocks in the sequence*)
criticalBlocks={};
For[j=2,j<=Length[direction],j++,
If[direction[[j-1]]>0&&direction[[j]]<0,
AppendTo[criticalBlocks,{Last[blockSequence[[j-1]]],Last[blockSequence[[j]]]}];
];
];

result["seq"]=blockSequence;
result["dir"]=direction;
result["cri"]=criticalBlocks;
result
];


updateStress[pBlock_,{nRow_,j_}]:=Module[{blockNum},
blockNum=getBlockNum[{nRow,j}];
\[Sigma]v[[blockNum]]=pBlock[[5;;16]];
\[Sigma]h[[blockNum+nRow-1]]=pBlock[[1;;4]];(*interface on the left side of the current block*)
\[Sigma]h[[blockNum+nRow]]=pBlock[[17;;20]];(*interface on the right side of the current block*)
];


checkRowEquilibrium[nRow_,criticalBlocksInRow_]:=Module[{unbalancedBlocksData,rowEqCheck,unbalancedBlocks,unbalancedBlockLoads,j,firstBlockLoads,lastBlockLoads,leftBlockLoads,rightBlockLoads,contact},
unbalancedBlocks={};
unbalancedBlockLoads={};
(*check first and last block in the row*)
firstBlockLoads=getBlockLoads[{nRow,1}];
lastBlockLoads=getBlockLoads[{nRow,getTotalBlocksInRow[nRow]}];
rowEqCheck=firstBlockLoads[[1;;4]]==lastBlockLoads[[17;;20]]=={0,0,0,0};
(*check the interface of each pair of critical blocks*)
For[j=1,j<=Length[criticalBlocksInRow],j++,
(*solve the left block*)
leftBlockLoads=getBlockLoads[{nRow,criticalBlocksInRow[[j,1]]}];
contact=contacts[[getBlockNum[{nRow,criticalBlocksInRow[[j,1]]}]]];
leftBlockLoads=solveBlock[leftBlockLoads,contact,{nRow,criticalBlocksInRow[[j,1]]},1];
(*solve the right block*)
rightBlockLoads=getBlockLoads[{nRow,criticalBlocksInRow[[j,2]]}];
contact=contacts[[getBlockNum[{nRow,criticalBlocksInRow[[j,2]]}]]];
rightBlockLoads=solveBlock[rightBlockLoads,contact,{nRow,criticalBlocksInRow[[j,2]]},-1];
(*check the interface*)
rowEqCheck=leftBlockLoads[[17;;20]]==rightBlockLoads[[1;;4]];
(*if the current pair is unbalanced save the block numbers and solutions*)
If[!rowEqCheck,
AppendTo[unbalancedBlocks,criticalBlocksInRow[[j]]];
AppendTo[unbalancedBlockLoads,{leftBlockLoads,rightBlockLoads}];
];
];

unbalancedBlocksData["eq_check"]=rowEqCheck;
unbalancedBlocksData["blocks"]=unbalancedBlocks;
unbalancedBlocksData["loads"]=unbalancedBlockLoads;
unbalancedBlocksData
];


computeGlobalRot[leftBlockLoads_,rightBlockLoads_]:=Module[{Lsu,Vsu,Lsb,Vsb,Ns,Ts,Nc,Tc,Nd,Td,Rs,Vs,Rc,Vc,Rd,Vd,Ldu,Vdu,Ldb,Vdb,M1,M2},

{Lsu,Vsu,Lsb,Vsb,Ns,Ts,Nc,Tc,Nd,Td,Rs,Vs,Rc,Vc,Rd,Vd,Ldu,Vdu,Ldb,Vdb}=leftBlockLoads;
M1=-((Ns+Vsu+Vsb+Nc/2+P/2)b-h(Lsu+Ts+Tc+Td));

{Lsu,Vsu,Lsb,Vsb,Ns,Ts,Nc,Tc,Nd,Td,Rs,Vs,Rc,Vc,Rd,Vd,Ldu,Vdu,Ldb,Vdb}=rightBlockLoads;
M2=(Nc/2+P/2+Nd-Vdu-Vdb)b+h(-Ldu+Ts+Tc+Td);

M1+M2
];


correctUnbalancedBlocks[nRow_,{leftBlock_,rightBlock_},{leftBlockLoads_,rightBlockLoads_},dir_]:=Module[{nextBlocksData,leftHasBaseContact,rightHasBaseContact,rowEqCheck,nextBlocks,nextBlockLoads,nextDir,nextLeftLoads,nextRightLoads},

rowEqCheck=True;nextBlocks={};nextBlockLoads={};nextDir=0;

If[leftBlockLoads[[{11,13}]]!={0,0},
leftHasBaseContact=True;,
leftHasBaseContact=False;
];
If[rightBlockLoads[[{13,15}]]!={0,0},
rightHasBaseContact=True;,
rightHasBaseContact=False;
];

(*TODO: add checks to detect edge blocks and return the appropriate rowEqCheck*)

(*first correction*)
If[dir==0,

If[leftHasBaseContact&&rightHasBaseContact,
(*both blocks have base contacts*)
(*check which block has the bigger Lb*)
If[leftBlockLoads[[19]]>rightBlockLoads[[3]],
(*Ldb>Lsb*)
rightBlockLoads[[1;;4]]=leftBlockLoads[[17;;20]];
(*solve right block with corrected loads*)
nextLeftLoads=correctBlock[rightBlockLoads,contacts[[getBlockNum[{nRow,rightBlock}]]],{nRow,rightBlock}];
(*update stress*)
updateStress[nextLeftLoads,{nRow,rightBlock}];
(*check if the wave needs to continue*)
If[nextLeftLoads[[17;;20]]!=rightBlockLoads[[17;;20]],
(*right interface has changed, wave continues towards the right*)
nextBlocks={leftBlock+1,rightBlock+1};
nextRightLoads=getBlockLoads[{nRow,rightBlock+1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=1;
];,
(*Ldb<Lsb*)
leftBlockLoads[[17;;20]]=rightBlockLoads[[1;;4]];
(*TODO: solve left block with corrected loads*)
nextRightLoads=correctBlock[leftBlockLoads,contacts[[getBlockNum[{nRow,leftBlock}]]],{nRow,leftBlock}];
(*update stress*)
updateStress[nextRightLoads,{nRow,leftBlock}];
(*check if the wave needs to continue*)
If[nextRightLoads[[1;;4]]!=leftBlockLoads[[1;;4]],
(*left interface has changed, wave continues towards the left*)
nextBlocks={leftBlock-1,rightBlock-1};
nextLeftLoads=getBlockLoads[{nRow,leftBlock-1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=-1;
];
];
];

If[leftHasBaseContact&&!rightHasBaseContact,
(*only left block has base contacts*)
leftBlockLoads[[17;;20]]=rightBlockLoads[[1;;4]];
(*solve left block with corrected loads*)
nextRightLoads=correctBlock[leftBlockLoads,contacts[[getBlockNum[{nRow,leftBlock}]]],{nRow,leftBlock}];
(*update stress*)
updateStress[nextRightLoads,{nRow,leftBlock}];
(*check if the wave needs to continue*)
If[nextRightLoads[[1;;4]]!=leftBlockLoads[[1;;4]],
(*left interface has changed, wave continues towards the left*)
nextBlocks={leftBlock-1,rightBlock-1};
nextLeftLoads=getBlockLoads[{nRow,leftBlock-1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=-1;
];
If[nextRightLoads[[17;;20]]!=leftBlockLoads[[17;;20]],
(*right interface has changed, wave continues towards the right*)
nextBlocks={leftBlock+1,rightBlock+1};
nextLeftLoads=nextRightLoads;
nextRightLoads=getBlockLoads[{nRow,rightBlock+1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=1;
];
];

If[!leftHasBaseContact&&rightHasBaseContact,
(*only right block has base contacts*)
rightBlockLoads[[1;;4]]=leftBlockLoads[[17;;20]];
(*solve right block with corrected loads*)
nextLeftLoads=correctBlock[rightBlockLoads,contacts[[getBlockNum[{nRow,rightBlock}]]],{nRow,rightBlock}];
(*update stress*)
updateStress[nextLeftLoads,{nRow,rightBlock}];
(*check if the wave needs to continue*)
If[nextLeftLoads[[17;;20]]!=rightBlockLoads[[17;;20]],
(*right interface has changed, wave continues towards the right*)
nextBlocks={leftBlock+1,rightBlock+1};
nextRightLoads=getBlockLoads[{nRow,rightBlock+1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=1;
];
If[nextLeftLoads[[1;;4]]!=rightBlockLoads[[1;;4]],
(*left interface has changed, wave continues towards the left*)
nextBlocks={leftBlock-1,rightBlock-1};
nextRightLoads=nextLeftLoads;
nextLeftLoads=getBlockLoads[{nRow,leftBlock-1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=-1;
];
];

If[!leftHasBaseContact&&!rightHasBaseContact,
(*Check which block really has base contacts*)
If[computeGlobalRot[leftBlockLoads,rightBlockLoads]>0,
rightBlockLoads[[1;;4]]=leftBlockLoads[[17;;20]];
(*correct towards the right*)
nextLeftLoads=correctBlock[rightBlockLoads,contacts[[getBlockNum[{nRow,rightBlock}]]],{nRow,rightBlock}];
(*update stress*)
updateStress[nextLeftLoads,{nRow,rightBlock}];
(*check if the wave needs to continue*)
If[nextLeftLoads[[17;;20]]!=rightBlockLoads[[17;;20]],
(*right interface has changed, wave continues towards the right*)
nextBlocks={leftBlock+1,rightBlock+1};
nextRightLoads=getBlockLoads[{nRow,rightBlock+1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=1;
];
If[nextLeftLoads[[1;;4]]!=rightBlockLoads[[1;;4]],
(*left interface has changed, wave continues towards the left*)
nextBlocks={leftBlock-1,rightBlock-1};
nextRightLoads=nextLeftLoads;
nextLeftLoads=getBlockLoads[{nRow,leftBlock-1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=-1;
];,
leftBlockLoads[[17;;20]]=rightBlockLoads[[1;;4]];
(*correct towards the left*)
nextRightLoads=correctBlock[leftBlockLoads,contacts[[getBlockNum[{nRow,leftBlock}]]],{nRow,leftBlock}];
(*update stress*)
updateStress[nextRightLoads,{nRow,leftBlock}];
(*check if the wave needs to continue*)
If[nextRightLoads[[1;;4]]!=leftBlockLoads[[1;;4]],
(*left interface has changed, wave continues towards the left*)
nextBlocks={leftBlock-1,rightBlock-1};
nextLeftLoads=getBlockLoads[{nRow,leftBlock-1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=-1;
];
If[nextRightLoads[[17;;20]]!=leftBlockLoads[[17;;20]],
(*right interface has changed, wave continues towards the right*)
nextBlocks={leftBlock+1,rightBlock+1};
nextLeftLoads=nextRightLoads;
nextRightLoads=getBlockLoads[{nRow,rightBlock+1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=1;
];
];
];
];

(*correction wave continues*)
If[dir>0,
(*left solution is correct, proceed towards the right*)
nextLeftLoads=correctBlock[rightBlockLoads,contacts[[getBlockNum[{nRow,rightBlock}]]],{nRow,rightBlock}];
(*update stress*)
updateStress[nextLeftLoads,{nRow,rightBlock}];
(*check if the wave needs to continue*)
If[nextLeftLoads[[17;;20]]!=rightBlockLoads[[17;;20]],
(*right interface has changed, wave continues towards the right*)
nextBlocks={leftBlock+1,rightBlock+1};
nextRightLoads=getBlockLoads[{nRow,rightBlock+1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=1;
];
];

If[dir<0,
(*right solution is correct, proceed towards the left*)
nextRightLoads=correctBlock[leftBlockLoads,contacts[[getBlockNum[{nRow,leftBlock}]]],{nRow,leftBlock}];
(*update stress*)
updateStress[nextRightLoads,{nRow,leftBlock}];
(*check if the wave needs to continue*)
If[nextRightLoads[[1;;4]]!=leftBlockLoads[[1;;4]],
(*left interface has changed, wave continues towards the left*)
nextBlocks={leftBlock-1,rightBlock-1};
nextLeftLoads=getBlockLoads[{nRow,leftBlock-1}];
nextBlockLoads={nextLeftLoads,nextRightLoads};
nextDir=-1;
];
];

nextBlocksData["eq_check"]=rowEqCheck;
nextBlocksData["blocks"]=nextBlocks;
nextBlocksData["loads"]=nextBlockLoads;
nextBlocksData["dir"]=nextDir;
nextBlocksData
];


startCorrectionWaves[nRow_,unbalancedBlocksData_]:=Module[{rowEqCheck,unbalancedBlocks,unbalancedBlockLoads,newUnbalancedBlocks,newUnbalancedBlockLoads,directions,newDirections,nextBlocksData,j,leftLoads,rightLoads},
unbalancedBlocks=unbalancedBlocksData["blocks"];
unbalancedBlockLoads=unbalancedBlocksData["loads"];
directions=Table[0,{Length[unbalancedBlocks]}];
While[Length[unbalancedBlocks]!=0&&rowEqCheck,
newUnbalancedBlocks={};
newUnbalancedBlockLoads={};
newDirections={};
For[j=1,j<=Length[unbalancedBlocks],j++,
nextBlocksData=correctUnbalancedBlocks[nRow,unbalancedBlocks[[j]],unbalancedBlockLoads[[j]],directions[[j]]];
rowEqCheck=nextBlocksData["eq_check"];
If[Length[nextBlocksData["blocks"]]!=0,
AppendTo[newUnbalancedBlocks,nextBlocksData["blocks"]];
AppendTo[newUnbalancedBlockLoads,nextBlocksData["loads"]];
AppendTo[newDirections,nextBlocksData["dir"]];
];
];

(*check for wave collisions*)
j=1;
While[j<=Length[newUnbalancedBlocks],
If[newUnbalancedBlocks[[j-1]]==newUnbalancedBlocks[[j]],
leftLoads=newUnbalancedBlockLoads[[j-1,1]];
rightLoads=newUnbalancedBlockLoads[[j,2]];
newUnbalancedBlocks=Drop[newUnbalancedBlocks,{j}];
newUnbalancedBlockLoads=Drop[newUnbalancedBlockLoads,{j}];
newUnbalancedBlockLoads[[j-1]]={leftLoads,rightLoads};
newDirections=Drop[newDirections,{j}];
newDirections[[j-1]]=0;
j--;
];
j++;
];

unbalancedBlocks=newUnbalancedBlocks;
unbalancedBlockLoads=newUnbalancedBlockLoads;
directions=newDirections;
];

rowEqCheck
];


solveRow[nRow_]:=Module[{j,blockSequence,blockLoads,contact,unbalancedBlocksData,rowEqCheck},
blockSequence=findSolvableBlockSequence[nRow];
For[j=1,j<=Length[blockSequence["seq"]],j++,
Do[
blockLoads=getBlockLoads[{nRow,k}];
contact=contacts[[getBlockNum[{nRow,k}]]];
blockLoads=solveBlock[blockLoads,contact,{nRow,k},blockSequence["dir"][[j]]];
updateStress[blockLoads,{nRow,k}];,
{k,blockSequence["seq"][[j]]}
];
];

(*equilibrium check*)
unbalancedBlocksData=checkRowEquilibrium[nRow,blockSequence["cri"]];
If[unbalancedBlocksData["eq_check"],
rowEqCheck=True;,
rowEqCheck=startCorrectionWaves[nRow,unbalancedBlocksData];
];

rowEqCheck
];


transferContactActionsBelow[nRow_]:=Module[{j,totalBlocksInRowBelow},
totalBlocksInRowBelow=getTotalBlocksInRow[nRow+1];
(*transfer base reactions of blocks in row nRow to the blocks in row nRow+1*)
For[j=1,j<=totalBlocksInRowBelow,j++,
updateStress[getBlockLoads[{nRow+1,j}],{nRow+1,j}];
];
];


solveWall[]:=Module[{i},
eqCheck=True;
For[i=1,i<=nely&&eqCheck,i++,
eqCheck=solveRow[i];
If[i!=nely,
transferContactActionsBelow[i];
];
];
];


displayWall[]:=Module[{blocks,stressAvg,interfaces,frictionRatio,blockLoads,ptBL,ptBR,ptTR,i,j,totalBlocksInRow},
blocks={}; stressAvg={};interfaces={};
For[i=1,i<=nely,i++,
totalBlocksInRow=getTotalBlocksInRow[i];
For[j=1,j<=totalBlocksInRow,j++,
If[!(isBlockOnLeftEdge[{i,j}]||isBlockOnRightEdge[{i,j}]),
ptBL={(j -1)b-Mod[i,2]b/2,(nely-i)h};(*bottom left vertex's coordinates of the current block*)
ptBR={j b-Mod[i,2]b/2,(nely-i)h};(*bottom right vertex's coordinates of the current block*)
ptTR={j b-Mod[i,2]b/2,(nely-i+1)h};(*top right vertex's coordinates of the current block*),
If[isBlockOnLeftEdge[{i,j}],
(*block on the left edge*)
ptBL={0,(nely-i)h};
ptBR={b-Mod[i,2]b/2,(nely-i)h};
ptTR={b-Mod[i,2]b/2,(nely-i+1)h};,
(*block on the right edge*)
ptBL={b(nelx-2)+Mod[i,2]b/2,(nely-i)h};
ptBR={b(nelx-1),(nely-i)h};
ptTR={b(nelx-1),(nely-i+1)h};
];
];

(*create graphical elements colored using stress measure*)
blockLoads=getBlockLoads[{i,j}];
AppendTo[stressAvg,Norm[blockLoads]];(*stress measure defined as the norm of 'blockLoads'*)
AppendTo[blocks,{EdgeForm[{Black}],GrayLevel[0.5],Rectangle[ptBL,ptTR]}];

frictionRatio=Piecewise[{{Abs[Total[blockLoads[[12;;16;;2]]]]/(\[Mu] Total[blockLoads[[11;;15;;2]]]),Total[blockLoads[[11;;15;;2]]]>0}}];
AppendTo[interfaces,{RGBColor[frictionRatio,0,0],Line[{ptBL,ptBR}]}];(*friction on the base*)
];
];
blocks[[;;,2]]=GrayLevel/@(stressAvg/Max[stressAvg]);(*assign GrayLevel based on strees*)

Show[Graphics[blocks],Graphics[interfaces]]];


End[];


EndPackage[];
