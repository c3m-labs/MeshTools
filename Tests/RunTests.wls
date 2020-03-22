#!/usr/bin/env wolframscript
(* ::Package:: *)

$name = "MeshTools";

Print["  Running tests for package ","\"",$name,"\"..."];

(* Modify paths depending from which directory the script is started.
By default we assume script is started from git root directory. *)
(* Message appearing in Mma 12.1 is suppressed. *)
Quiet[
	PacletDirectoryAdd@Directory[],
	PacletDirectoryAdd::expobs
];

Get[$name<>"`"];

Print["  Mathematica: ",$VersionNumber];

Module[
	{report,time,results,count},
	(* Set here the name of file(s) containing actual tests. *)
	report=TestReport@FileNameJoin[{Directory[],"Tests","Tests.wl"}];
	time=Round[QuantityMagnitude@report["TimeElapsed"],0.01];
	results=report["TestResults"];
	count=Length[results];

	Print["\n","  ",ToString@count," tests run in ",ToString@time," seconds."];

	If[
		TrueQ@report["AllTestsSucceeded"]
		,
		Print["\n","  All tests passed."];
		Quit[0] (* exit code if all test pass *)
		,
		Print["\n","  Some tests failed!"];
		Print["  Idx | Outcome | TestID"];
		Do[
			Print["  ",StringPadLeft[ToString[i], 3]," | ",results[i]["Outcome"]," | ",results[i]["TestID"]],
			{i,report["TestsFailedIndices"]}
		];
		Quit[1] (* exit code if any test fails *)
	]
]
