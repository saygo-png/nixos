{
  conUsername,
  pkgs,
  lib,
  ...
}: {
  home-manager.users.${conUsername} = {
    programs = {
      git-credential-oauth.enable = true;

      lazygit = {
        enable = true;
        settings = {
          gui.border = "single";
          git = {
            overrideGpg = true;
            commit.signOff = true;
            branchLogCmd = "git log --graph --color=always --abbrev-commit --decorate --date=relative --pretty=medium --oneline {{branchName}} --";
            paging.pager = "delta --dark --paging=never";
          };
          customCommands = [
            {
              key = "f";
              command = "git difftool -y {{.SelectedLocalCommit.Sha}} -- {{.SelectedCommitFile.Name}}";
              context = "commitFiles";
              description = "Compare (difftool) with local copy";
            }
            {
              key = "F";
              command = "git pull --rebase --autostash {{ SelectedLocalBranch.Name }}";
              context = "localBranches";
              output = "log";
            }
          ];
        };
      };

      git = {
        enable = true;
        delta = {
          enable = true;
          options = {
            syntax-theme = "none";
            hunk-header-style = "omit";
            file-style = "omit";
            diff-output.line-numbers = true;
            keep-plus-minus-markers = true;

            plus-style = ''"#98971a" normal'';
            plus-emph-style = ''"#b8bb26" bold normal'';

            minus-style = ''"#cc241d" normal'';
            minus-emph-style = ''"#fb4934" bold normal'';
          };
        };
        aliases = {
          aa = "add -A"; # [A]dd [A]ll
          amend = "commit -a --amend";
          undo = "reset HEAD~1 --mixed";
          deleteGitignored = ''rm --cached ''${git ls-files -i -c --exclude-from=.gitignore}'';
          prettylog = "log --pretty=\"(%C(Green)%cr%C(reset)) %C(Cyan)%an: %C(reset)%s\" --date=short";
        };
        extraConfig = {
          color.ui = "auto";
          pull.rebase = true;
          commit.gpgsign = true;
          rerere.enabled = true;
          branch.autosetupmerge = true;
          merge.tool = "${lib.getExe pkgs.meld}";
          init.defaultBranch = "main";
          diff.colorMoved = "default";
          core = {
            excludesfile = "~/.gitignore_global";
            interactive.diffFilter = "delta";
          };
          user = {
            signingKey = "86B6FCCC3563C00B";
            name = "saygo-png";
            email = "saygo.mail@proton.me";
          };
          push = {
            autoSetupRemote = true;
            useForceIfIncludes = true;
          };
        };
      };
    };
  };
}
