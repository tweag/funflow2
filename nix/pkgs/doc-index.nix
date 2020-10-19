# Creates an index.html page for bundling with funflow documentation
{ runCommand }:
runCommand "doc-index" {
  src = ../../funflow-pages;
} ''
  mkdir -p $out/share
  cp $src/index.html $src/style.css $out/share
''
