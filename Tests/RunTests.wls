#!/usr/bin/env wolframscript
(* ::Package:: *)

With[{
	tr=TestReport[FileNameJoin[{Directory[],"Tests","Tests.wl"}]]
	},
	
	Exit@If[
		TrueQ[tr["AllTestsSucceeded"]],
		Print["All tests passed!"];
		0, (* exit code if all test pass *)
		Print["Tests failed! Indices: ",tr["TestsFailedIndices"]];
		1
	]
]
