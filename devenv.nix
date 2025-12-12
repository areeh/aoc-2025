{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [
    pkgs.git
    pkgs.fontconfig
    pkgs.cairo
    pkgs.libjpeg
    pkgs.glib
    pkgs.pango
    pkgs.gtk2-x11
    pkgs.graphviz
  ];

  # https://devenv.sh/languages/
  # languages.rust.enable = true;
  languages.racket.enable = true;
  dotenv.enable = true;

  # https://devenv.sh/processes/
  # processes.dev.exec = "${lib.getExe pkgs.watchexec} -n -- ls -la";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  scripts.hello.exec = ''
    echo hello from $GREET
  '';

  # https://devenv.sh/basics/
  enterShell = ''
    export LD_LIBRARY_PATH="${
      lib.makeLibraryPath [
        pkgs.fontconfig
        pkgs.cairo
        pkgs.libjpeg
        pkgs.glib
        pkgs.pango
        pkgs.gtk2-x11
      ]
    }:''${LD_LIBRARY_PATH:-}"
    hello         # Run scripts directly
    git --version # Use packages
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
