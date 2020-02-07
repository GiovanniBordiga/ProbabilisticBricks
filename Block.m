(* ::Package:: *)

BeginPackage["ProbabilisticBricks`Block`"];


solveBlock::usage = "solveBlock[pBlock, contact] returns the vector of interface forces satisfying the equilibrium, the friction criterion and the unilaterality condition.";
correctBlock::usage = "correctBlock[pBlock,contact,{nRow,j}] solve the block equilibrium assuming the forces on both vertical interfaces are known.";


Begin["`Private`"];
Needs["ProbabilisticBricks`Problem`"];
Needs["ProbabilisticBricks`Wall`"];


swapLR[p_] := Module[{pnew},
	pnew = Table[0, {20}];
	(*swap normal forces*)
	pnew[[1 ;; 4 ;; 2]] = p[[17 ;; 20 ;; 2]];pnew[[17 ;; 20 ;; 2]] = p[[1 ;; 4 ;; 2]];(*Ls--Ld*)
	pnew[[5 ;; 9 ;; 2]] = p[[9 ;; 5 ;; -2]];(*Ns--Nc--Nd*)
	pnew[[11 ;; 15 ;; 2]] = p[[15 ;; 11 ;; -2]];(*Rs--Rc--Rd*)
	(*swap tangential forces*)
	pnew[[2 ;; 4 ;; 2]] = -p[[18 ;; 20 ;; 2]];pnew[[18 ;; 20 ;; 2]] = -p[[2 ;; 4 ;; 2]];(*Vsu--Vdu,Vsb--Vdb*)
	pnew[[6 ;; 10 ;; 2]] = -p[[10 ;; 6 ;; -2]];(*Ts--Td*)
	pnew[[12 ;; 16 ;; 2]] = -p[[16 ;; 12 ;; -2]];(*Vs--Vc--Vd*)

	pnew
];


Vsimp[H_, Rt_, \[Mu]_, R_] := Piecewise[{{R / Rt H, Abs[H] <= \[Mu] Rt}, {Sign[H]\[Mu] R, Abs[H] > \[Mu] Rt}}];


Vbm[H_, V2e_, V3e_, R1_, R2_, R3_, \[Mu]_] := Piecewise[{{H - Min[Abs[V2e], \[Mu] R2]Sign[V2e] - Min[Abs[V3e], \[Mu] R3]Sign[V3e], Abs[H - Min[Abs[V2e], \[Mu] R2]Sign[V2e] - Min[Abs[V3e], \[Mu] R3]Sign[V3e]] <= \[Mu] R1}, {\[Mu] R1 Sign[H - Min[Abs[V2e], \[Mu] R2]Sign[V2e] - Min[Abs[V3e], \[Mu] R3]Sign[V3e]], Abs[H - Min[Abs[V2e], \[Mu] R2]Sign[V2e] - Min[Abs[V3e], \[Mu] R3]Sign[V3e]] > \[Mu] R1}}];


solveBlockL2R[pBlock_, contact_] := Module[{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb, Ms, Mc, Md, H, Rt, Vse, Vce, Vde},
	{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb} = pBlock;

	(*solution left to right*)
	Ldu = 0;Vdu = 0;Ldb = 0;Vdb = 0;
	Md = (Ns + Vsu + Vsb + Nc / 2 + P / 2)b - h(Lsu - Ldu + Ts + Tc + Td);
	Ms = (Nc / 2 + P / 2 + Nd - Vdu - Vdb)b + h(Lsu - Ldu + Ts + Tc + Td);
	Mc = (Ns + Vsu + Vsb - Nd + Vdu + Vdb)b / 2 - h(Lsu - Ldu + Ts + Tc + Td);
	Rt = Vsu + Vsb + Ns + Nc + P + Nd - Vdu - Vdb;
	H = Lsu + Lsb + Ts + Tc + Td - Ldu - Ldb;

	If[contact == 1,
		(*mechanism 1*)
		Rc = 0;
		Rs = Max[Md / b, 0];
		Rd = Rt - Rs;
		(*compute the elastic value of the base friction forces*)
		Vse = Ts + Tc / 2 + Lsu + Lsb;
		Vce = 0;
		Vde = Td + Tc / 2 - Ldu - Ldb;,
		(*mechanism 2 or 3*)
		Rs = Max[Mc / (b / 2), 0];
		Rd = Piecewise[{{Max[-Mc / (b / 2), 0], Md >= 0}, {Rt, Md < 0}}];
		Rc = Piecewise[{{Max[Rt - Rs - Rd, 0], Md >= 0}, {0, Md < 0}}];
		(*compute the elastic value of the base friction forces*)
		Vse = Piecewise[{{Ts + Lsu + Lsb, Mc >= 0}}];
		Vce = Piecewise[{{Tc + Td - Ldu - Ldb, Mc >= 0}, {Tc + Ts + Lsu + Lsb, Mc < 0}}];
		Vde = Piecewise[{{Td - Ldu - Ldb, Mc <= 0}}];
	];

	Ldu = Max[-Md / h, 0];
	Vs = Vbm[H, Vce, Vde, Rs, Rc, Rd, \[Mu]];
	Vc = Vbm[H, Vse, Vde, Rc, Rs, Rd, \[Mu]];
	Vd = Piecewise[{{Vbm[H, Vse, Vce, Rd, Rs, Rc, \[Mu]], Md >= 0}, {Min[H - Ldu, \[Mu] Rd], Md < 0}}];
	Ldb = H - Ldu - Vs - Vc - Vd;

	Chop[{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb}]
];


solveHalvedBlockL2R[pBlock_, isOnLeftEdge_] := Module[{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb, Ms, Mc, Md, H, Rt, Vse, Vce, Vde},
	{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb} = pBlock;

	(*solution left to right*)
	Ldu = 0;Vdu = 0;Ldb = 0;Vdb = 0;
	Rt = Vsu + Vsb + Ns + Nc + P / 2 + Nd - Vdu - Vdb;
	H = Lsu + Lsb + Ts + Tc + Td - Ldu - Ldb;

	(*modify resultants for the halved block*)
	If[isOnLeftEdge,
		(*halved block on the left edge*)
		Md = (Ns + Vsu + Vsb + Nc / 2 + P / 2 / 4)b - h(Lsu - Ldu + Ts + Tc + Td);
		Mc = (Ns + Vsu + Vsb - Nd + Vdu + Vdb)b / 2 - h(Lsu - Ldu + Ts + Tc + Td) - P / 2b / 4;
		(*compute reactions for the halved block - only one mechanism*)
		Rs = 0;
		Rd = Piecewise[{{Max[-Mc / (b / 2), 0], Md >= 0}, {Rt, Md < 0}}];
		Rc = Piecewise[{{Max[Rt - Rs - Rd, 0], Md >= 0}, {0, Md < 0}}];
		(*compute the elastic value of the base friction forces*)
		Vse = 0;
		Vce = Tc + Ts + Lsu + Lsb;
		Vde = Td - Ldu - Ldb;,
		(*halved block on the right edge*)
		Ms = (Nc / 2 + P / 2 / 4 + Nd - Vdu - Vdb)b + h(Lsu - Ldu + Ts + Tc + Td);
		Md = (Ns + Vsu + Vsb - Nd + Vdu + Vdb)b / 2 - h(Lsu - Ldu + Ts + Tc + Td) + P / 2b / 4; (*it's actually Mc...*)
		(*compute reactions for the halved block - only one mechanism*)
		Rd = 0;
		Rc = Piecewise[{{Max[Ms / (b / 2), 0], Md >= 0}, {Rt, Md < 0}}];
		Rs = Piecewise[{{Max[Rt - Rc - Rd, 0], Md >= 0}, {0, Md < 0}}];
		(*compute the elastic value of the base friction forces*)
		Vse = Ts + Lsu + Lsb;
		Vce = Tc + Td - Ldu - Ldb;
		Vde = 0;
	];

	Ldu = Max[-Md / h, 0];
	Vs = Vbm[H, Vce, Vde, Rs, Rc, Rd, \[Mu]];
	If[isOnLeftEdge,
		Vc = Vbm[H, Vse, Vde, Rc, Rs, Rd, \[Mu]];
		Vd = Piecewise[{{Vbm[H, Vse, Vce, Rd, Rs, Rc, \[Mu]], Md >= 0}, {Min[H - Ldu, \[Mu] Rd], Md < 0}}];,
		Vd = 0;
		Vc = Piecewise[{{Vbm[H, Vse, Vde, Rc, Rs, Rd, \[Mu]], Md >= 0}, {Min[H - Ldu, \[Mu] Rc], Md < 0}}];
	];
	Ldb = H - Ldu - Vs - Vc - Vd;

	Chop[{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb}]
];


solveBlockR2L[pBlock_, contact_] := Module[{},
	swapLR[solveBlockL2R[swapLR[pBlock], contact]]
];

solveHalvedBlockR2L[pBlock_, isOnLeftEdge_] := Module[{},
	swapLR[solveHalvedBlockL2R[swapLR[pBlock], !isOnLeftEdge]]
];


solveBlock[pBlock_, contact_, {nRow_, j_}, dir_] := Module[{},
	If[!isBlockHalved[{nRow, j}],
		(*normal blocks*)
		If[dir > 0,
			solveBlockL2R[pBlock, contact],
			solveBlockR2L[pBlock, contact]
		],
		(*halved blocks*)
		If[dir > 0,
			solveHalvedBlockL2R[pBlock, isBlockOnLeftEdge[{nRow, j}]],
			solveHalvedBlockR2L[pBlock, isBlockOnLeftEdge[{nRow, j}]]
		]
	]
];


correctBlock[pBlock_, contact_, {nRow_, j_}] := Module[{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb, Mc, Md, H, Rt, Vse, Vce, Vde},
	{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb} = pBlock;

	If[isBlockHalved[{nRow, j}],
		(*halved blocks*)
		If[isBlockOnLeftEdge[{nRow, j}],
			{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb} = solveHalvedBlockR2L[pBlock, True];,
			{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb} = solveHalvedBlockL2R[pBlock, False];
		];,
		(*normal blocks*)
		Rt = Vsu + Vsb + Ns + Nc + P + Nd - Vdu - Vdb;
		H = Lsu + Lsb + Ts + Tc + Td - Ldu - Ldb;
		Md = (Ns + Vsu + Vsb + Nc / 2 + P / 2)b - h(Lsu - Ldu + Ts + Tc + Td);
		Mc = (Ns + Vsu + Vsb - Nd + Vdu + Vdb)b / 2 - h(Lsu - Ldu + Ts + Tc + Td);

		If[Abs[Mc / Rt] > b / 2,
			(*no base mechanism is possible, solve L2R or R2L*)
			{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb} = solveBlock[pBlock, contact, {nRow, j}, -Sign[Mc / Rt]];
			,
			(*base mechanism is possible*)
			If[contact == 1,
				(*mechanism 1*)
				Rc = 0;
				Rs = Md / b;
				Rd = Rt - Rs;
				(*compute the elastic value of the base friction forces*)
				Vse = Ts + Tc / 2 + Lsu + Lsb;
				Vce = 0;
				Vde = Td + Tc / 2 - Ldu - Ldb;,
				(*mechanism 2 and 3*)
				Rs = Max[Mc / (b / 2), 0];
				Rd = Max[-Mc / (b / 2), 0];
				Rc = Rt - Rs - Rd;
				(*compute the elastic value of the base friction forces*)
				Vse = Piecewise[{{Ts + Lsu + Lsb, Mc >= 0}}];
				Vce = Piecewise[{{Tc + Td - Ldu - Ldb, Mc >= 0}, {Tc + Ts + Lsu + Lsb, Mc < 0}}];
				Vde = Piecewise[{{Td - Ldu - Ldb, Mc <= 0}}];
			];
			Vs = Vbm[H, Vce, Vde, Rs, Rc, Rd, \[Mu]];
			Vc = Vbm[H, Vse, Vde, Rc, Rs, Rd, \[Mu]];
			Vd = Vbm[H, Vse, Vce, Rd, Rs, Rc, \[Mu]];
			(*check if the friction criterion is satisfied*)
			If[H - Vs - Vc - Vd > 0,
				Ldb = H + Ldb - Vs - Vc - Vd;
			];
			If[H - Vs - Vc - Vd < 0,
				Lsb = -(H - Lsb - Vs - Vc - Vd);
			];
		];
	];

	Chop[{Lsu, Vsu, Lsb, Vsb, Ns, Ts, Nc, Tc, Nd, Td, Rs, Vs, Rc, Vc, Rd, Vd, Ldu, Vdu, Ldb, Vdb}]
];


End[];


EndPackage[];
