# Creates an index.html page for bundling with funflow documentation
{ runCommand, docker
, doc-index, api-docs, generate-funflow-tutorials}:
runCommand "doc-pages" {
  buildInputs = [ docker ];
  srcFunflowTutorials = ../../funflow-tutorial;
}
''
  export HOME=$(pwd)
  mkdir -p $out/share
  # Docs index page
  cp -r ${doc-index}/share/* $out/share
  # Copy API
  cp -r ${api-docs}/share/* $out/share
  # Make tutorials
  cp -r $srcFunflowTutorials funflow-tutorials && chmod -R +rwx funflow-tutorials
  ${generate-funflow-tutorials}/bin/generate-funflow-tutorial funflow-tutorials/notebooks $out/share/tutorials
''
