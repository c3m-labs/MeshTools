#!/usr/bin/env wolframscript
(* ::Package:: *)

$helpMessage=("  Usage: RunTests.wls [options]

  Options:
    -h, --help                   Output usage information")


If[
	MemberQ[Rest@$ScriptCommandLine,"/?"|"-h"|"--help"],
	Print[$helpMessage];Quit[1]
];


Print["  Running \"MeshTools\" tests..."];

(* Modify paths depending from which directory the script is started.
By default we assume script is started from git root directory. *)
PacletDirectoryAdd@Directory[];
Get["MeshTools`"];

Module[
	{tr,time},
	tr=TestReport[FileNameJoin[{Directory[],"Tests","Tests.wl"}]];
	time=QuantityMagnitude@tr["TimeElapsed"];
	
	If[
		TrueQ@tr["AllTestsSucceeded"]
		,
		Print["\n","  All tests passed in ",ToString@time," seconds."];
		Quit[0] (* exit code if all test pass *)
		,
		Print["\n","  Tests failed! IDs: ",tr["TestsFailedIndices"]];
		Quit[1] (* exit code if any test fails *)
	]
]
