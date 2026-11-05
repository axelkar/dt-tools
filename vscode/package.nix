{
  lib,
  stdenv,
  buildNpmPackage,
  vsce,
  executable ? null,
  vscodeTarget ?
    if executable == null then
      null
    else if stdenv.hostPlatform.isAarch64 then
      "linux-arm64"
    else
      "linux-x64",
  vsixName ? "dt-tools-${if executable == null then "no-server" else vscodeTarget}.vsix",
}:
let
  manifest = lib.importJSON ./package.json;
in
buildNpmPackage {
  pname = manifest.name;
  version = manifest.version;
  src = lib.cleanSource ./.;

  npmDepsHash = "sha256-GwRF6G9geWB0c4XkdDcPVKVnMpwTUwPqKqbTUbapYyU=";

  # Skip optional fsevents dev-dependency node-gyp build
  npmInstallFlags = [ "--omit=optional" ];

  nativeBuildInputs = [ vsce ];

  # These were symlinks to files outside the copied source tree.
  postPatch = ''
    rm -f CHANGELOG.md LICENSE-APACHE LICENSE-MIT
    cp ${../CHANGELOG.md} CHANGELOG.md
    cp ${../LICENSE-APACHE} LICENSE-APACHE
    cp ${../LICENSE-MIT} LICENSE-MIT
  '';

  buildPhase = ''
    runHook preBuild
    ${lib.optionalString (executable != null) ''
      install -Dm755 ${lib.getExe executable} bin/dt-tools
    ''}
    vsce package --out "${vsixName}" ${
      lib.optionalString (vscodeTarget != null) ''--target "${vscodeTarget}"''
    } --skip-license
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    install -Dm644 "${vsixName}" "$out/${vsixName}"
    runHook postInstall
  '';

  meta = {
    description = "VS Code support for Devicetree files${
      lib.optionalString (executable != null) " (including dt-tools executable)"
    }";
    homepage = "https://github.com/axelkar/dt-tools/tree/master/vscode";
    license = [
      lib.licenses.mit
      lib.licenses.asl20
    ];
  };
}
