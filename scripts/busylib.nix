{ pkgs ? import <nixpkgs> {} }:

pkgs.python3Packages.buildPythonPackage rec {
  pname = "busylib";
  version = "1.3.0";
  format = "pyproject";

  src = pkgs.python3Packages.fetchPypi {
    inherit pname version;
    hash = "sha256-u9sPfsUdxTZwO7lqcXYnLVz5CH82ktKi/SfL7qb5TR8=";
  };

  build-system = with pkgs.python3Packages; [
    setuptools
    setuptools-scm
    wheel
  ];

  propagatedBuildInputs = with pkgs.python3Packages; [
    httpx
    pillow
    protobuf
    pydantic
    pydantic-extra-types
    pydantic-settings
    typing-extensions
    websockets
    zeroconf
  ];

  doCheck = false;
}
