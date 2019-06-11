# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.7.1] - 2019-04-23

### Fixed

- Compatibility with [Mathematica 12](https://www.wolfram.com/language/new-in-12/)

## [0.7.0] - 2019-04-10

### Added

- Options `"Refinement"` and `"MeshOrder"` for `StructuredMesh`
- Function `RevolveMesh`

## [0.6.1] - 2019-03-19

### Fixed

- Updated documentation with appropriate cross-links

## [0.6.0] - 2019-03-07

### Added

- Function `IdentifyMeshBoundaries`

## Changed

- Significant performance improvement for `MeshElementMeasure` and `BoundaryElementMeasure`
- Changed node and element ordering in `StructuredMesh` to be compatible with AceFEM functions

## [0.5.0] - 2019-02-14

### Added

- New option `"SplitDirection"` in `QuadToTriangleMesh`

### Changed

- Arbitrary number of triangles per edge in `TriangleMesh`
- Arbitrary number of tetrahedra per edge in `TetrahedronMesh`
- `AddMeshMarkers` supports all element types

### Removed

- `SelectElementsByMarker` is removed and its functionality merged into `SelectElements`

## [0.4.0] - 2019-01-24

### Added

- Functions `HexahedronMesh`, `PrismMesh`, `CylinderMesh` and `CircularVoidMesh`
- Functions `TriangularToQuadMesh` and `StructuredMesh` become public

### Removed

- Function `RodriguesSpaceMesh` is removed
  
## [0.3.1] - 2019-08-23

### Fixed

- `MergeMesh` and `TransformMesh` accept options of `ElementMesh` object 
and this fixes some weird behaviour when handling boundary meshes

## [0.3.0] - 2019-01-25

### Added

- Functions `TriangleMesh` and `TetrahedronMesh`
- Function `SelectElements` for selecting elements according to their position
- More documentation examples

## [0.2.0] - 2018-08-17

### Added

- Added and improved functions to transform meshes

### Changed

- Improved functions to create structured meshes on simple geometries

## [0.1.0] - 2018-05-18

### Added

- Initial functionality

[Unreleased]: https://github.com/c3m-labs/MeshTools/compare/v0.7.1...HEAD
[0.7.1]: https://github.com/c3m-labs/MeshTools/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/c3m-labs/MeshTools/compare/v0.6.1...v0.7.0
[0.6.1]: https://github.com/c3m-labs/MeshTools/compare/v0.6.0...v0.6.1
[0.6.0]: https://github.com/c3m-labs/MeshTools/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/c3m-labs/MeshTools/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/c3m-labs/MeshTools/compare/v0.3.1...v0.4.0
[0.3.1]: https://github.com/c3m-labs/MeshTools/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/c3m-labs/MeshTools/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/c3m-labs/MeshTools/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/c3m-labs/MeshTools/releases/tag/v0.1.0
