{ runCommand,
  funflow-tutorial
}:
runCommand "generate-funflow-tutorial" {
  src = ../funflow-tutorial;
  buildInputs = [
    funflow-tutorial.quick-reference
    funflow-tutorial.tutorial1
    funflow-tutorial.wordcount
  ];
  # wordcount reads a "words.txt" file from the working directory
  # Here, we take the example included with funflow-tutorial
} ''
  mkdir -p $out/docs/tutorial
  cp $src/words.txt .
  quick-reference > $out/docs/tutorial/quick-reference.html
  tutorial1 > $out/docs/tutorial/tutorial1.html
  wordcount > $out/docs/tutorial/wordcount.html
''
