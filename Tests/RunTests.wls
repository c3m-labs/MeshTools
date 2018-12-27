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

Module[
	{report,time,results},
	report=TestReport[FileNameJoin[{Directory[],"Tests","Tests.wl"}]];
	time=QuantityMagnitude@report["TimeElapsed"];
	results=report["TestResults"];
	
	If[
		TrueQ@report["AllTestsSucceeded"]
		,
		Print["  All tests passed in ",ToString@time," seconds."];
		Quit[0] (* exit code if all test pass *)
		,
		Print["  Tests failed!"];
		Do[
			Print["  ",i," / ",results[i]["Outcome"]," / ",results[i]["TestID"]],
			{i,report["TestsFailedIndices"]}
		];
		Quit[1] (* exit code if any test fails *)
	]
]
