#!/usr/bin/env wolframscript
(* ::Package:: *)

Print["  Running unit tests..."];

(* This script should be run from Git repository root directory. *)
Get["MeshTools.wl",Path->Directory[]];

With[{
	tr=TestReport[FileNameJoin[{Directory[],"Tests","Tests.wl"}]]
	},
	Exit@If[
		TrueQ[tr["AllTestsSucceeded"]],
		Print["  All tests passed!"];
		0, (* exit code if all test pass *)
		Print["  Tests failed! ID: ",tr["TestsFailedIndices"]];
		1 (* exit code if any test fails *)
	]
]
