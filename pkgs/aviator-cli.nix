{
  lib,
  stdenvNoCC,
  fetchurl,
  gh,
  git,
  installShellFiles,
  makeBinaryWrapper,
}: let
  version = "0.1.45";
  releases = {
    "x86_64-linux" = {
      platform = "linux_x86_64";
      hash = "sha256-f8ieDQ5ywCqFfw6cuGXB7Z+scTE9FRLIs0tS35SOnOw=";
    };
    "aarch64-linux" = {
      platform = "linux_arm64";
      hash = "sha256-IoRXUs2mkw8M1O+Rwdrlsce1Y9mOsGDMov3GoU/FwuE=";
    };
    "x86_64-darwin" = {
      platform = "darwin_x86_64";
      hash = "sha256-N/5EiRzIZLP44B8glr1zbgZUnwTAAWttR1lC0CxWPA8=";
    };
    "aarch64-darwin" = {
      platform = "darwin_arm64";
      hash = "sha256-9ofZARlSuaU0k9+widamwUbpCGiht4Vbx0X5u2D5/l8=";
    };
  };
  release = releases.${stdenvNoCC.hostPlatform.system};
in
  stdenvNoCC.mkDerivation {
    pname = "aviator-cli";
    inherit version;

    src = fetchurl {
      url = "https://github.com/aviator-co/av/releases/download/v${version}/av_${version}_${release.platform}.tar.gz";
      inherit (release) hash;
    };
    sourceRoot = ".";

    nativeBuildInputs = [
      installShellFiles
      makeBinaryWrapper
    ];

    dontBuild = true;

    installPhase = ''
      runHook preInstall

      install -Dm755 av "$out/libexec/aviator-cli/av"
      install -Dm644 LICENSE "$out/share/licenses/aviator-cli/LICENSE"
      installManPage man/man1/*.1 man/man7/*.7

      installShellCompletion --cmd av \
        --bash <("$out/libexec/aviator-cli/av" completion bash) \
        --fish <("$out/libexec/aviator-cli/av" completion fish) \
        --zsh <("$out/libexec/aviator-cli/av" completion zsh)

      makeBinaryWrapper "$out/libexec/aviator-cli/av" "$out/bin/av" \
        --prefix PATH : ${lib.makeBinPath [
        gh
        git
      ]}

      runHook postInstall
    '';

    doInstallCheck = true;
    installCheckPhase = ''
      runHook preInstallCheck
      "$out/bin/av" --version | grep -Fqx "v${version}"
      runHook postInstallCheck
    '';

    meta = {
      description = "CLI for creating, updating, reviewing, and merging stacked GitHub pull requests";
      homepage = "https://github.com/aviator-co/av";
      changelog = "https://github.com/aviator-co/av/releases/tag/v${version}";
      license = lib.licenses.mit;
      mainProgram = "av";
      platforms = builtins.attrNames releases;
      sourceProvenance = with lib.sourceTypes; [binaryNativeCode];
    };
  }
