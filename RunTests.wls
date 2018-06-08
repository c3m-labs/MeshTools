#!/usr/bin/env wolframscript
(* ::Package:: *)

With[{
	tr=TestReport[FileNameJoin[{Directory[],"Tests.wl"}]]
	},
	
	Exit@If[
		tr["AllTestsSucceeded"],
		0, (* exit code if all test pass *)
		Print["Tests failed! Indices: ",tr["TestsFailedIndices"]];
		1
	]
]
