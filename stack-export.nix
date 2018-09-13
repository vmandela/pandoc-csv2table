{ mkDerivation, base, csv, pandoc, pandoc-types, stdenv, text }:
mkDerivation {
  pname = "pandoc-csv2table";
  version = "1.0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [ base csv pandoc pandoc-types text ];
  executableHaskellDepends = [ base csv pandoc pandoc-types ];
  homepage = "https://github.com/baig/pandoc-csv2table-filter";
  description = "Convert CSV to Pandoc Table Markdown";
  license = stdenv.lib.licenses.mit;
}
