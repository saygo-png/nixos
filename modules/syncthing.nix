{
  config,
  conHome,
  conUsername,
  ...
}: let
  syncthingCfg = config.services.syncthing;
in {
  custom.persist.home.cache.directories = [".config/syncthing"];

  services.syncthing = {
    enable = true;
    dataDir = conHome;
    user = conUsername;
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = true;
    settings = {
      options.relaysEnabled = false;
      devices = {
        nixos.id = "PSTKO2I-QKU64OB-LMEOEGC-LTAVJHU-O6SCT4V-2GQ7QI2-KARS3FJ-VSZ4ZAT";
        phone.id = "Z7AOC2O-CYXT6XV-Y67O5RB-VAXE2JT-JV36AMW-KWQ3U6Z-PVTINXB-IQ2UHQ7";
        thinkpad.id = "R3RAH4P-BEWWRO6-S5HYB2N-HZIHYCH-ERUDTE2-R2XLRAQ-CAZNG7U-S5BYYAF";
        pixel8.id = "EBTJEDI-BV735LM-24RZ7AK-ODAVUMX-KK7IHA6-T2RDBXO-Y5YNCSC-U3TS5QK";
      };
      folders = let
        allDevices = builtins.attrNames syncthingCfg.settings.devices;
        home = config.home-manager.users.${conUsername}.home.homeDirectory;
      in {
        Sync = {
          path = "${home}/Sync";
          id = "default";
          devices = allDevices;
        };

        builds = {
          path = "${home}/builds";
          devices = ["thinkpad" "nixos"];
        };

        Music = {
          path = "${home}/Music";
          devices = allDevices;
        };

        gnupg = {
          path = "${home}/.local/share/gnupg";
          id = "fxmdx-ymklb";
          devices = ["nixos" "thinkpad"];
        };

        "mc instances" = {
          path = "${home}/.local/share/PrismLauncher";
          id = "ahybq-dbssj";
          devices = ["nixos" "thinkpad"];
        };
      };
    };
  };

  home-manager.users.${conUsername} = {lib, ...}: {
    home = {
      activation.makeSyncthingDirectoriesAndIgnorePatterns = let
        inherit (lib) attrValues;
        inherit (lib.strings) concatLines;
        sFolders = syncthingCfg.settings.folders;

        makeSTDirs = syncthingFolders:
          concatLines
          (map (dir: ''run mkdir $VERBOSE_ARG -p "${dir.path}"'') (attrValues syncthingFolders));

        makeSTFile = folder: content: path: ''run cat "${builtins.toFile "syncthingFileContent" content}" > ${folder.path}/${path}'';

        makeSTFiles = syncthingFolders: content: path:
          concatLines
          (map (folder: makeSTFile folder content path) (attrValues syncthingFolders));

        makeSTSharedIgnores = syncthingFolders: let
          content = ''
            (?d)battlenet

            (?d).direnv
            (?d).devenv
            (?d)node_modules
            (?d)node_modules/
            (?d)__pycache__/
            (?d)*.py[cod]
            (?d)*$py.class
            (?d)*.so
            (?d).Python
            (?d)develop-eggs/
            (?d).eggs/
            (?d)*.egg-info/
            (?d).installed.cfg
            (?d)*.egg
            (?d)pip-log.txt
            (?d)pip-delete-this-directory.txt
            (?d)htmlcov/
            (?d).tox/
            (?d).nox/
            (?d).coverage
            (?d).coverage.*
            (?d).cache
            (?d)nosetests.xml
            (?d)coverage.xml
            (?d)*.cover
            (?d)*.py,cover
            (?d).hypothesis/
            (?d).pytest_cache/
            (?d).webassets-cache
            (?d).scrapy
            (?d)docs/_build/
            (?d).pybuilder/
            (?d)target/
            (?d).ipynb_checkpoints
            (?d).pdm.toml
            (?d).pdm-python
            (?d).pdm-build/
            (?d)__pypackages__/
            (?d)celerybeat.pid
            (?d)*.sage.py
            (?d).env
            (?d).venv
            (?d)venv/
            (?d)env.bak/
            (?d)venv.bak/
            (?d).spyderproject
            (?d).spyproject
            (?d).mypy_cache/
            (?d).dmypy.json
            (?d)dmypy.json
            (?d).pyre/
            (?d).pytype/
            (?d)cython_debug/
            (?d).idea/
            (?d)*.class
            (?d)*.ctxt
            (?d).mtj.tmp/
            (?d)hs_err_pid*
            (?d)replay_pid*
            (?d).devenv*
            (?d)devenv.local.nix
            (?d).pre-commit-config.yaml
            (?d).lsp
            (?d)natives/
            (?d)/target
            (?d)/classes
            (?d)/checkouts
            (?d).rebel_readline_history
            (?d)*.sync-conflict-*
            (?d).cpcache/
            (?d)/node_modules
            (?d)/.shadow-cljs
            (?d)/*.iml
            (?d)/.nrepl-port
            (?d)/.idea
            (?d).calva
            (?d).clj-kondo
            (?d)/classes/
            (?d)/target/
            (?d)/checkouts/
            (?d).lein-deps-sum
            (?d).lein-repl-history
            (?d).lein-plugins/
            (?d).lein-failures
            (?d).nrepl-port
            (?d).cljs*
            (?d)/out
            (?d).lein*
            (?d)*.iml
            (?d).repl*
            (?d)*.swp
            (?d)**/.shadow-cljs
            (?d)**/cljs-runtime
            (?d)**/dist-newstyle
          '';
        in
          makeSTFiles syncthingFolders content ".stignore-shared";

        makeSTIgnorePattern = folder: pattern: makeSTFile folder pattern ".stignore";

        makeSTIncludeSharedI = folder:
          makeSTIgnorePattern folder ''
            #include .stignore-shared
          '';

        customIgnores = {
          "mc instances" = ''
            // Downloaded again by PrismLauncher when missing.
            (?d)/cache
            (?d)/meta
            (?d)/libraries
            (?d)/logs
          '';
        };

        makePatterns = syncthingFolders:
          concatLines
          (lib.mapAttrsToList (name: folder:
            if customIgnores ? ${name}
            then makeSTIgnorePattern folder customIgnores.${name}
            else makeSTIncludeSharedI folder)
          syncthingFolders);
      in
        lib.hm.dag.entryAfter ["writeBoundary"]
        (concatLines (map (f: f sFolders) [
          makeSTDirs
          makePatterns
          makeSTSharedIgnores
        ]));
    };
  };
}
