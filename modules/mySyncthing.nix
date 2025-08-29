{
  config,
  conHome,
  conUsername,
  ...
}: let
  syncthingCfg = config.services.syncthing;
in {
  # File synchronization.
  services.syncthing = {
    enable = true;
    dataDir = conHome;
    user = conUsername;
    openDefaultPorts = true;
    overrideDevices = true;
    overrideFolders = true;
    settings.options.relaysEnabled = false;
    settings.devices = {
      nixos = {
        addresses = [
          "tcp://192.168.1.11:22000"
        ];
        id = "PSTKO2I-QKU64OB-LMEOEGC-LTAVJHU-O6SCT4V-2GQ7QI2-KARS3FJ-VSZ4ZAT";
      };
      phone = {
        addresses = [
          "tcp://192.168.1.10:22000"
        ];
        id = "Z7AOC2O-CYXT6XV-Y67O5RB-VAXE2JT-JV36AMW-KWQ3U6Z-PVTINXB-IQ2UHQ7";
      };
      thinkpad = {
        addresses = [
          "tcp://192.168.1.13:22000 "
        ];
        id = "R3RAH4P-BEWWRO6-S5HYB2N-HZIHYCH-ERUDTE2-R2XLRAQ-CAZNG7U-S5BYYAF";
      };
    };
    settings.folders = let
      allDevices = builtins.attrNames syncthingCfg.settings.devices;
      home = config.home-manager.users.${conUsername}.home.homeDirectory;
    in {
      Sync = {
        path = "${home}/Sync";
        id = "default";
        type = "sendreceive";
        versioning = null;
        devices = allDevices;
      };

      Games = {
        path = "${home}/Games";
        id = "5mfmg-kwfkf";
        type = "sendreceive";
        versioning = null;
        devices = [
          "thinkpad"
          "nixos"
        ];
      };

      builds = {
        path = "${home}/builds";
        id = "builds";
        type = "sendreceive";
        versioning = null;
        devices = [
          "thinkpad"
          "nixos"
        ];
      };

      Music = {
        path = "${home}/Music";
        id = "Music";
        type = "sendreceive";
        versioning = null;
        devices = allDevices;
      };

      gnupg = {
        path = "${home}/.local/share/gnupg";
        id = "fxmdx-ymklb";
        type = "sendreceive";
        versioning = null;
        devices = [
          "nixos"
          "thinkpad"
        ];
      };

      "mc instances" = {
        path = "${home}/.local/share/PrismLauncher";
        id = "ahybq-dbssj";
        type = "sendreceive";
        versioning = null;
        devices = [
          "nixos"
          "thinkpad"
        ];
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
            (?d)build/
            (?d)develop-eggs/
            (?d)dist/
            (?d)downloads/
            (?d)eggs/
            (?d).eggs/
            (?d)lib/
            (?d)lib64/
            (?d)parts/
            (?d)sdist/
            (?d)wheels/
            (?d)share/python-wheels/
            (?d)*.egg-info/
            (?d).installed.cfg
            (?d)*.egg
            (?d)MANIFEST
            (?d)*.manifest
            (?d)*.spec
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
            (?d)cover/
            (?d)*.mo
            (?d)*.pot
            (?d)*.log
            (?d)local_settings.py
            (?d)db.sqlite3
            (?d)db.sqlite3-journal
            (?d)instance/
            (?d).webassets-cache
            (?d).scrapy
            (?d)docs/_build/
            (?d).pybuilder/
            (?d)target/
            (?d).ipynb_checkpoints
            (?d)profile_default/
            (?d)ipython_config.py
            (?d).pdm.toml
            (?d).pdm-python
            (?d).pdm-build/
            (?d)__pypackages__/
            (?d)celerybeat-schedule
            (?d)celerybeat.pid
            (?d)*.sage.py
            (?d).env
            (?d).venv
            (?d)env/
            (?d)venv/
            (?d)ENV/
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
            (?d).direnv
            (?d).pre-commit-config.yaml
            (?d).direnv
            (?d).lsp
            (?d)natives/
            (?d)/target
            (?d)/classes
            (?d)/checkouts
            (?d)pom.xml
            (?d)node_modules/
            (?d).rebel_readline_history
            (?d)*.sync-conflict-*
            (?d).cpcache/
            (?d)**/public/js
            (?d)/node_modules
            (?d)/target
            (?d)/yarn.lock
            (?d)/package-lock.json
            (?d)/.shadow-cljs
            (?d)/*.iml
            (?d)/.nrepl-port
            (?d)/.idea
            (?d).calva
            (?d).lsp
            (?d).clj-kondo
            (?d)pom.xml
            (?d)pom.xml.asc
            (?d)*.jar
            (?d)*.class
            (?d)/lib/
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

        makePatterns = syncthingFolders:
          concatLines
          (map makeSTIncludeSharedI (attrValues syncthingFolders));
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
