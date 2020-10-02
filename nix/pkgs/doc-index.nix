# Creates an index.html page for bundling with funflow documentation
{ runCommand, pandoc }:
let
  htmlTemplate = ''
    <!DOCTYPE html>
    <html lang="en">
        <head>
        <meta name="viewport" content="width=device-width, initial-scale=1.0">
        <!-- Bootstrap -->
        <link href="css/bootstrap.min.css" rel="stylesheet" media="screen">
        <script src="https://code.jquery.com/jquery.js"></script>
        <script src="js/bootstrap.min.js"></script>
        </head>
        <body>
        <div class="container">
    $body$
        </div>
        </body>
    </html>
  '';

  # Note: If you want to add links to the doc index do so here
  indexMD = ''
    # `funflow` Documentation Index

    Welcome to the `funflow` developer documentation!

    ## Contents

    1. Tutorials
        1. [Getting Started](./tutorial/Tutorial1.html)
        2. [WordCount](./tutorial/WordCount.html)
        3. [ML Pipeline with Docker and TensorFlow](./tutorial/TensorflowDocker.html)
        4. [Quick Reference](./tutorial/QuickReference.html)
        5. [Advanced Tutorial](./tutorial/Tutorial2.html)
        
    2. [API Docs](./doc/index.html)
  '';
in runCommand "generate-doc-index" {
  src = ./.;
  buildInputs = [ ];
} ''
  mkdir -p $out/share
  echo '${htmlTemplate}' > template.html
  echo '${indexMD}' | ${pandoc}/bin/pandoc -f markdown -t html --template=template.html -o $out/share/index.html
''
