{
  lib,
  pkgs,
  config,
  ...
}: {
  # Edit of below to allow for any shell
  # https://github.com/hsjobeki/nixpkgs/tree/migrate-doc-comments/pkgs/build-support/trivial-builders/default.nix#L246:C5
  writeAnyShellApplication = {
    /*
    *
    Package of the runtime shell

    # Type

    ```
    Deriviation
    ```
    */
    shellPackage,
    /*
    *
    The name of the script to write.

    # Type

    ```
    String
    ```
    */
    name,
    /*
    *
    The shell script's text, not including a shebang.

    # Type

    ```
    String
    ```
    */
    text,
    /*
    *
    Inputs to add to the shell script's `$PATH` at runtime.
    # Type

    ```
    [String|Derivation]
    ```
    */
    runtimeInputs ? [],
    /*
    *
    Extra environment variables to set at runtime.

    # Type

    ```
    AttrSet
    ```
    */
    runtimeEnv ? null,
    /*
    *
    `stdenv.mkDerivation`'s `meta` argument.

    # Type

    ```
    AttrSet
    ```
    */
    meta ? {},
    /*
    *
    `stdenv.mkDerivation`'s `passthru` argument.

    # Type

    ```
    AttrSet
    ```
    */
    passthru ? {},
    /*
    *
    The `checkPhase` to run. Defaults to `shellcheck` on supported
    platforms and `bash -n`.

    The script path will be given as `$target` in the `checkPhase`.

    # Type

    ```
    String
    ```
    */
    checkPhase ? null,
    /*
    *
    Checks to exclude when running `shellcheck`, e.g. `[ "SC2016" ]`.

    See <https://www.shellcheck.net/wiki/> for a list of checks.

    # Type

    ```
    [String]
    ```
    */
    excludeShellChecks ? [],
    /*
    *
    Extra command-line flags to pass to ShellCheck.

    # Type

    ```
    [String]
    ```
    */
    extraShellCheckFlags ? [],
    /*
    *
    Bash options to activate with `set -o` at the start of the script.

    Defaults to `[ "errexit" "nounset" "pipefail" ]`.

    # Type

    ```
    [String]
    ```
    */
    derivationArgs ? {},
    /*
    *
    Whether to inherit the current `$PATH` in the script.

    # Type

    ```
    Bool
    ```
    */
    inheritPath ? true,
  }:
    pkgs.writeTextFile {
      inherit
        name
        meta
        passthru
        derivationArgs
        ;
      executable = true;
      destination = "/bin/${name}";
      allowSubstitutes = true;
      preferLocalBuild = false;
      text =
        ''
          #!${lib.getExe shellPackage}
        ''
        + lib.optionalString (runtimeEnv != null) (
          lib.concatStrings (
            lib.mapAttrsToList (name: value: ''
              ${lib.toShellVar name value}
              export ${name}
            '')
            runtimeEnv
          )
        )
        + lib.optionalString (runtimeInputs != []) ''

          export PATH="${lib.makeBinPath runtimeInputs}${lib.optionalString inheritPath ":$PATH"}"
        ''
        + ''

          ${text}
        '';

      checkPhase =
        # GHC (=> shellcheck) isn't supported on some platforms (such as risc-v)
        # but we still want to use writeShellApplication on those platforms
        let
          shellcheckSupported =
            lib.meta.availableOn pkgs.stdenv.buildPlatform pkgs.shellcheck-minimal.compiler
            && (builtins.tryEval pkgs.shellcheck-minimal.compiler.outPath).success;
          excludeFlags = lib.optionals (excludeShellChecks != []) [
            "--exclude"
            (lib.concatStringsSep "," excludeShellChecks)
          ];
          shellcheckCommand = lib.optionalString shellcheckSupported ''
            # use shellcheck which does not include docs
            # pandoc takes long to build and documentation isn't needed for just running the cli
            ${lib.getExe pkgs.shellcheck-minimal} ${
              lib.escapeShellArgs (excludeFlags ++ extraShellCheckFlags)
            } "$target"
          '';
        in
          if checkPhase == null
          then ''
            runHook preCheck
            ${pkgs.stdenv.shellDryRun} "$target"
            ${shellcheckCommand}
            runHook postCheck
          ''
          else checkPhase;
    };

  # Used for wrapping something like "awesome" with its own xinitrc, so
  # it can be launched like wayland compositors
  wrapWithXinitrc = xinitrc: wmExe: let
    xinitrcFile = pkgs.writeTextFile {
      name = ".xinitrc";
      executable = true;
      text = xinitrc;
    };
  in
    if (!config.services.xserver.displayManager.startx.enable && !config.services.xserver.displayManager.sx.enable)
    then throw "You need either startx or sx enabled"
    else let
      startx-wrapper-if = lib.mkIf config.services.xserver.displayManager.startx.enable [
        (pkgs.writeShellApplication {
          name = "startx-${wmExe}";
          runtimeInputs = [pkgs.xorg.xinit];
          text = ''
            XINITRC=${xinitrcFile} startx
          '';
        })
      ];
      sx-wrapper-if = lib.mkIf config.services.xserver.displayManager.sx.enable [
        (pkgs.writeShellScriptBin "sx-${wmExe}"
          ''
            sx ${xinitrcFile}
          '')
      ];
      unpackBasedIfListOnCon = ifAttrset:
        if ifAttrset.condition
        then ifAttrset.content
        else [];
    in
      (unpackBasedIfListOnCon startx-wrapper-if) ++ (unpackBasedIfListOnCon sx-wrapper-if);
}
