(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Block`"];


solveBlock::usage="solveBlock[pBlock, contact] returns the vector of interface forces satisfying the equilibrium, the friction criterion and the unilaterality condition.";


Begin["`Private`"];
Needs["ProbabilisticBricks`Problem`"];


V[H_,Rt_,\[Mu]_,R_]:=Piecewise[{{R/Rt H,Abs[H]<=\[Mu] Rt},{Sign[H]\[Mu] R,Abs[H]>\[Mu] Rt}}];


switchLR[p_]:=Module[{pnew},
pnew=Table[0,{20}];
(*switch normal forces*)
pnew[[1;;4;;2]]=p[[17;;20;;2]];pnew[[17;;20;;2]]=p[[1;;4;;2]];(*Ls\[UndirectedEdge]Ld*)
pnew[[5;;9;;2]]=p[[9;;5;;-2]];(*Ns\[UndirectedEdge]Nc\[UndirectedEdge]Nd*)
pnew[[11;;15;;2]]=p[[15;;11;;-2]];(*Rs\[UndirectedEdge]Rc\[UndirectedEdge]Rd*)
(*switch tangential forces*)
pnew[[2;;4;;2]]=-p[[18;;20;;2]];pnew[[18;;20;;2]]=-p[[2;;4;;2]];(*Vsu\[UndirectedEdge]Vdu,Vsb\[UndirectedEdge]Vdb*)
pnew[[6;;10;;2]]=-p[[10;;6;;-2]];(*Ts\[UndirectedEdge]Td*)
pnew[[12;;16;;2]]=-p[[16;;12;;-2]];(*Vs\[UndirectedEdge]Vc\[UndirectedEdge]Vd*)

pnew
];


solveBlockL2R[pBlock_,contact_]:=Module[{Lsu,Vsu,Lsb,Vsb,Ns,Ts,Nc,Tc,Nd,Td,Rs,Vs,Rc,Vc,Rd,Vd,Ldu,Vdu,Ldb,Vdb,Ms,Mc,Md,H,Rt},
{Lsu,Vsu,Lsb,Vsb,Ns,Ts,Nc,Tc,Nd,Td,Rs,Vs,Rc,Vc,Rd,Vd,Ldu,Vdu,Ldb,Vdb}=pBlock;

(*solution left to right*)
Ldu=0;Vdu=0;Ldb=0;Vdb=0;
Md=(Ns+Vsu+Vsb+Nc/2+P/2)b-h(Lsu-Ldu+Ts+Tc+Td);
Ms=(Nc/2+P/2+Nd-Vdu-Vdb)b+h(Lsu-Ldu+Ts+Tc+Td);
Mc=(Ns+Vsu+Vsb-Nd+Vdu+Vdb)b/2-h(Lsu-Ldu+Ts+Tc+Td);
Rt=Vsu+Vsb+Ns+Nc+P+Nd-Vdu-Vdb;
H=Lsu+Lsb+Ts+Tc+Td-Ldu-Ldb;

If[contact==1,
(*meccanismo 1*)
Rc=0;
Rs=Max[Md/b,0];
Rd=Rt-Rs;,
(*meccanismo 2 e 3*)
Rs=Max[Mc/(b/2),0];
Rd=Piecewise[{{Max[-Mc/(b/2),0],Md>=0},{Rt,Md<0}}];
Rc=Piecewise[{{Max[Rt-Rs-Rd,0],Md>=0},{0,Md<0}}];
];

Ldu=Max[-Md/h,0];
Vs=V[H,Rt,\[Mu],Rs];
Vc=V[H,Rt,\[Mu],Rc];
Vd=Piecewise[{{V[H,Rt,\[Mu],Rd],Md>=0},{Min[H-Ldu,\[Mu] Rd],Md<0}}];
Ldb=Piecewise[{{Max[H-\[Mu] Rt,0],Md>=0},{H-Ldu-Vd,Md<0}}];

{Lsu,Vsu,Lsb,Vsb,Ns,Ts,Nc,Tc,Nd,Td,Rs,Vs,Rc,Vc,Rd,Vd,Ldu,Vdu,Ldb,Vdb}
];


solveBlockR2L[pBlock_,contact_]:=Module[{pBlockR2L},
switchLR[solveBlockL2R[switchLR[pBlock],contact]]
];


solveBlock[pBlock_,contact_,dir_]:=Module[{},
If[dir>0,
solveBlockL2R[pBlock,contact],
solveBlockR2L[pBlock,contact]]
];


End[];


EndPackage[];
