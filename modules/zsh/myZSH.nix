{
  lib,
  inputs,
  pkgs,
  conUsername,
  ...
}: {
  programs.zsh.enable = true;

  # Set shell of the user.
  users.users.${conUsername}.shell = pkgs.zsh;

  # Declare zsh as an available shell.
  environment.shells = [pkgs.zsh];
  environment.systemPackages = [pkgs.gitstatus];

  # Provides autocompletion for system programs for zsh.
  environment.pathsToLink = ["/share/zsh"];
  home-manager.users.${conUsername} = {config, ...}: {
    home.shellAliases = {"nix-shell" = "nix-shell --run zsh";};
    programs.yazi.enableZshIntegration = true;
    programs.zsh = {
      enable = true;
      enableVteIntegration = true;
      history.save = 50;
      history.size = 50;
      initContent = let
        zshConfig = config.xdg.configHome + "/zsh";
        cache = config.xdg.cacheHome;

        zcomp = f: ''zcompile -R -- "${f}".zwc "${f}"'';
        zcompdumpFile = "${zshConfig}/.zcompdump";

        withPlug = x: lib.my.relativeToRoot ("modules/zsh/plugins/" + x);
        ovSrc = p: s: (pkgs.callPackage (withPlug "${p}") {}).overrideAttrs (_: {src = s;});

        p10k = ovSrc "p10k.nix" inputs.powerlevel10k;
        notify = ovSrc "notify.nix" inputs.zsh-auto-notify;
        clipboard = ovSrc "clipboard.nix" inputs.zsh-system-clipboard;
        autosuggestions = ovSrc "autosuggestions.nix" inputs.zsh-autosuggestions;
      in
        # sh
        ''
          # Activate Powerlevel10k Instant Prompt.
          if [[ -r "${cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
            source "${cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
          fi

          autoload -Uz compinit && compinit
          [[ ${zshConfig}/.zcompdump.zwc -nt ${zshConfig}/.zcompdump ]] || ${zcomp zcompdumpFile}

          # Autocomplete.
          unsetopt EXTENDED_GLOB
          setopt no_list_ambiguous
          setopt GLOB_COMPLETE        # Show autocompletion menu with globs
          setopt MENU_COMPLETE        # Automatically highlight first element of completion menu
          setopt AUTO_LIST            # Automatically list choices on ambiguous completion.
          setopt COMPLETE_IN_WORD     # Complete from both ends of a word.
          setopt GLOB_DOTS
          unsetopt CASE_GLOB
          zstyle ':completion:*' menu select
          zmodload zsh/complist

          # Use hjlk in menu selection (during completion)
          bindkey -M menuselect 'h' vi-backward-char
          bindkey -M menuselect 'k' vi-up-line-or-history
          bindkey -M menuselect 'j' vi-down-line-or-history
          bindkey -M menuselect 'l' vi-forward-char
          bindkey -M menuselect '^xg' clear-screen
          bindkey -M menuselect '^xi' vi-insert                      # Insert
          bindkey -M menuselect '^xh' accept-and-hold                # Hold
          bindkey -M menuselect '^xn' accept-and-infer-next-history  # Next
          bindkey -M menuselect '^xu' undo                           # Undo

          # Define completers
          zstyle ':completion:*' completer _extensions _complete _approximate
          # Use cache for commands using cache
          zstyle ':completion:*' use-cache on
          zstyle ':completion:*' cache-path "${cache}/zsh/.zcompcache"
          # Autocomplete options for cd instead of directory stack
          zstyle ':completion:*' complete-options true
          zstyle ':completion:*' file-sort modification
          zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
          zstyle ':completion:*:*:*:*:descriptions' format '%F{blue}-- %D %d --%f'
          zstyle ':completion:*:*:*:*:messages' format ' %F{purple} -- %d --%f'
          zstyle ':completion:*:*:*:*:warnings' format ' %F{red}-- no matches found --%f'
          # Colors for files and directory
          zstyle ':completion:*:*:*:*:default' list-colors ''${(s.:.)LS_COLORS}
          zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
          zstyle ':completion:*:complete:git:argument-1:' tag-order !aliases

          # Required for completion to be in good groups (named after the tags)
          zstyle ':completion:*' group-name '''
          zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands

          # Vi mode.
          export KEYTIMEOUT=1
          bindkey -v

          # Efficient cursor shape changes
          cursor_mode() {
              case $KEYMAP in
                  vicmd) printf '\e[2 q';;      # block
                  viins|main) printf '\e[6 q';; # beam
              esac
          }
          zle -N zle-keymap-select cursor_mode
          zle -N zle-line-init cursor_mode

          # Edit line in vim with ctrl-e:
          autoload edit-command-line; zle -N edit-command-line
          bindkey '^e' edit-command-line
          bindkey -M vicmd '^[[P' vi-delete-char
          bindkey -M vicmd '^e' edit-command-line
          bindkey -M visual '^[[P' vi-delete

          # Backspace delete.
          bindkey -v '^?' backward-delete-char

          # Don't glob urls.
          autoload -Uz bracketed-paste-magic
          zle -N bracketed-paste bracketed-paste-magic
          autoload -Uz url-quote-magic
          zle -N self-insert url-quote-magic

          fzf-cd-filedir() {
              local dir
              dir="$(fzfcd)" || return 1
              [[ -z "$dir" ]] && { zle redisplay; return 1; }

              zle push-line
              if builtin cd -- "''${dir:a}"; then
                  zle accept-line
              else
                  zle redisplay
                  return 1
              fi
              unset dir
          }
          zle -N fzf-cd-filedir
          bindkey '^f' fzf-cd-filedir
          bindkey -M vicmd '^f' fzf-cd-filedir

          nbuild() {
            local file="''${1:-./package.nix}"
            local abs_file
            abs_file=$(realpath "$file")
            nix-build -E "with import <nixpkgs> {}; callPackage \"$abs_file\" {}"
          }

          detach() {
            prog=$1
            shift
            nohup setsid $prog $@ > /dev/null 2>&1
          }

          source ${p10k}/powerlevel10k/powerlevel10k.zsh-theme
          source ${lib.my.relativeToRoot "resources/zsh/p10k-prompt.zsh"}

          source ${pkgs.zsh-fast-syntax-highlighting}/share/zsh/site-functions/fast-syntax-highlighting.plugin.zsh
          source ${notify}/zsh-auto-notify/auto-notify.plugin.zsh
          export AUTO_NOTIFY_CANCEL_ON_SIGINT=1
          export AUTO_NOTIFY_WHITELIST=("nh" "nix" "nbuild" "nix-build" "nix-shell" "git" "cabal" "cp" "rclone" "borg" "borgmatic")
          source ${clipboard}/zsh-system-clipboard/zsh-system-clipboard.zsh
          source ${autosuggestions}/zsh-autosuggestions/zsh-autosuggestions.zsh
        '';
    };
  };
}
