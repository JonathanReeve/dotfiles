{ config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
     # Python Development
     pipenv
     # poetry
     # mach-nix
     (python3.withPackages(ps: with ps; [
       pandas
       matplotlib
       python-lsp-server # Emacs integration
       plotly
       flake8 # Syntax checking for emacs
       scikit-learn
       # altair
       # vega
       # vega_datasets
       jupyter
       jupyterlab
       # jupyter-book
       # jupytext
       # tensorflow
       nltk
       pip
       # poetry-dynamic-versioning
       numpy
       tldextract # required by qute-pass
     ]))
  ];
}
