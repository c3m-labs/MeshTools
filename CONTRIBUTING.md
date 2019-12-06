# Development of MeshTools package

If you would like to modify _MeshTools_ package yourself or contribute back to the original repository,
then the following instructions can help you.
First you need to install [Git](https://git-scm.com/) and
[clone](https://help.github.com/articles/cloning-a-repository/) the project
from its GitHub homepage to your local computer.

You will need:

* [Mathematica](https://www.wolfram.com/mathematica/) version 11.1 or later.
* [Wolfram Workbench](https://www.wolfram.com/workbench/) for building documentation.
* [WolframScript](https://www.wolfram.com/wolframscript/) for building the `.paclet` file from command line.
 On most systems it already comes bundled with Mathematica installation.

## Testing code

It is considered good practice that every (public) function in this package includes its own set of unit tests.
A bunch of them is collected in `Tests/Tests.wl` file, using the Mathematica testing
[framework](https://reference.wolfram.com/language/guide/SystematicTestingAndVerification.html).
It is recommended that you run them periodically during development and especially before every commit.
This can be done by calling script file `Tests/RunTests.wls` in command line
(from repository root directory) or by evaluating whole notebook `Tests/RunTests.nb`.

Make sure that all tests pass on the earliest supported version of Mathematica (currently version 10.0).

### Integration of tests in Git hook

Unit test can be run automatically before every commit via Git client-side
[hooks](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks).
File `pre-commit` should contain call to `Tests/RunTests.wls` script,
which exits with value 0 if all tests pass and aborts the commit otherwise.
Minimal example of `pre-commit` file content is:

    #!/bin/sh
    ./Tests/RunTests.wls

## Building the package

Package building covers procedures to convert contents of repository to `.paclet`
file which can be installed by the users.
This is done by the build script included in the repository, which can be run
from command line (terminal) with `Build.wls`.
On Unix-like operating systems it may be necessary to set executable
permissions first with `chmod a+x Build.wls`. For more information see also
`wolframscript`[documentation](https://reference.wolfram.com/language/ref/program/wolframscript.html).

### Documentation

The package documentation comes in a form of notebooks with textual
explanation and executable code examples.
Raw documentation notebooks are created and processed with additional
software [Wolfram Workbench](https://www.wolfram.com/workbench/).

Before manipulating documentation for the first time, you have to import
the project into Workbench. This procedure needs to be done only once.

* From the menu bar, select __File > Open Project from File System__.
* In the Import wizard, select _MeshTools_ repository root directory for
the input field "Import source:".
* You can leave all checkboxes as default and click __Finish__.
* _MeshTools_ directory tree now appears in "Package Explorer" tab (on the left side).

For more information about adding and modifying documentation notebooks, please see
[Wolfram Workbench documentation](https://reference.wolfram.com/workbench/index.jsp).

Documentation build procedure refers to the process of taking "source" notebooks
and converting them to form suitable for Mathematica documentation centre.
For example [`Graphics`](https://reference.wolfram.com/language/ref/Graphics.html)
objects are rasterized to minimize documentation file size.
To build documentation follow these steps:

* From the Workbench menu bar, select __Window > Show View > Application Tools__.
* In the "Application Tools" tab select _MeshTools_ in project drop down list.
* Click on button "Build", just bellow the drop down list.
* Build procedure may take a few minutes and copies processed notebooks to
"build" subdirectory in repository root directory.
* You can also see the result of procedure after it is completed
by clicking button "Preview" in "Application Tools" tab.

### Creating a .paclet file

Running the build script in command line from repository root directory
will automatically create a `MeshTools-X.Y.Z.paclet` file in the "build"
folder.
Keep in mind that you have to build documentation with Workbench before creating the paclet.

Script option `--install` will also install it locally to `$UserBasePacletsDirectory`.
Option `--release` adds metadata to indicate a proper public releases.

### Release checklist

The following tasks should be performed for successful release of a new public version of the package.

* Check if all known bugs have been fixed.
* Check if all changes in functionality since the previous release have been documented
(use `git log --oneline` to refresh memory).
* Run a full suite of automated tests.
* Build the package along with documentation and test if installed version works ok (e.g. documentation examples).
You can send this version to other test users for additional feedback.
* Update [PacletInfo.m]( PacletInfo.m ) with a new version number.
It should be determined according to [semantic versioning](https://semver.org/) principles.
* Update [README.md]( README.md ) and [CHANGELOG.md]( CHANGELOG.md ) with relevant changes.
* Make a new [`git tag`](https://git-scm.com/book/en/v2/Git-Basics-Tagging)
with corresponding version number and push it to remote repository.
* Build a public version with `--release` option of build script.
* Create a new release on GitHub [releases](https://github.com/c3m-labs/MeshTools/releases)
page and attach there resulting `.paclet` file.
* Inform users about a new version (email, website, etc).
* Celebrate.
