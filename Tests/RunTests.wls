#!/usr/bin/env wolframscript
(* ::Package:: *)

$helpMessage=("  Usage: RunTests.wls [options]

  Options:
    -h, --help                   Output usage information")


If[
	MemberQ[Rest@$ScriptCommandLine,"/?"|"-h"|"--help"],
	Print[$helpMessage];Quit[1]
];


Print["  Running \"MeshTools\" tests...","\n"];

(* Modify paths depending from which directory the script is started.
By default we assume script is started from git root directory. *)
PacletDirectoryAdd@Directory[];
Get["MeshTools`"];

Print["  Using Mathematica: ",$VersionNumber];

Module[
	{report,time,results,failIdx},
	
	report=TestReport[
		FileNameJoin[{Directory[],"Tests","Tests.wl"}]
	];
	time=Round[
		QuantityMagnitude@report["TimeElapsed"],
		0.001
	];
	results=report["TestResults"];
	failIdx=report["TestsFailedIndices"];
	
	Print["\n","  ",Length[results]," tests run in ",time," seconds."];
	
	If[
		TrueQ@report["AllTestsSucceeded"]
		,
		Print["  All tests succeeded!"];
		Quit[0] (* exit code if all test pass *)
		,
		Print["  ",Length[failIdx]," tests failed!"];
		Do[
			Print["  ",i," / ",results[i]["Outcome"]," / ",results[i]["TestID"]],
			{i,failIdx}
		];
		Quit[1] (* exit code if any test fails *)
	]
]
