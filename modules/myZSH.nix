{
  pkgs,
  conUsername,
  conFlakePathRel,
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
  home-manager.users.${conUsername} = {
    home.shellAliases = {"nix-shell" = "nix-shell --run zsh";};
    programs.yazi.enableZshIntegration = true;
    programs.zsh = {
      enable = true;
      enableVteIntegration = true;
      history.save = 50;
      history.size = 50;
      initExtra = ''
        function zcompile-many() {
          local f
          for f; do zcompile -R -- "$f".zwc "$f"; done
        }

        # Clone and compile to wordcode missing plugins.
        if [[ ! -e ~/.local/zsh-syntax-highlighting ]]; then
          git clone --depth=1 https://github.com/zsh-users/zsh-syntax-highlighting.git ~/.local/zsh-syntax-highlighting
          zcompile-many ~/.local/zsh-syntax-highlighting/{zsh-syntax-highlighting.zsh,highlighters/*/*.zsh}
        fi
        if [[ ! -e ~/.local/zsh-autosuggestions ]]; then
          git clone --depth=1 https://github.com/zsh-users/zsh-autosuggestions.git ~/.local/zsh-autosuggestions
          zcompile-many ~/.local/zsh-autosuggestions/{zsh-autosuggestions.zsh,src/**/*.zsh}
        fi
        if [[ ! -e ~/.local/powerlevel10k ]]; then
          git clone --depth=1 https://github.com/romkatv/powerlevel10k.git ~/.local/powerlevel10k
          make -C ~/.local/powerlevel10k pkg
          zcompile-many ~/.local/zsh-autosuggestions/{powerlevel10k.zsh-theme,internal/**/*.zsh,config/**/*.zsh,gitstatus/**/*.zsh}
        fi

        unfunction zcompile-many

        # Activate Powerlevel10k Instant Prompt.
        if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
          source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
        fi

        # Autocomplete.
        setopt extendedglob
        setopt no_list_ambiguous
        setopt GLOB_COMPLETE        # Show autocompletion menu with globs
        setopt MENU_COMPLETE        # Automatically highlight first element of completion menu
        setopt AUTO_LIST            # Automatically list choices on ambiguous completion.
        setopt COMPLETE_IN_WORD     # Complete from both ends of a word.
        setopt GLOB_DOTS
        unsetopt CASE_GLOB
        zstyle ':completion:*' menu select
        zmodload zsh/complist
        # Load more completions.
        _comp_options+=(globdots)  # Include hidden files.

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

        # Don't glob urls.
        autoload -Uz bracketed-paste-magic
        zle -N bracketed-paste bracketed-paste-magic
        autoload -Uz url-quote-magic
        zle -N self-insert url-quote-magic

        # Only work with the Zsh function vman
        compdef vman="man"
        # Define completers
        zstyle ':completion:*' completer _extensions _complete _approximate
        # Use cache for commands using cache
        zstyle ':completion:*' use-cache on
        zstyle ':completion:*' cache-path "$XDG_CACHE_HOME/zsh/.zcompcache"
        # Complete the alias when _expand_alias is used as a function
        zstyle ':completion:*' complete true
        zle -C alias-expension complete-word _generic
        bindkey '^Xa' alias-expension
        zstyle ':completion:alias-expension:*' completer _expand_alias
        # Autocomplete options for cd instead of directory stack
        zstyle ':completion:*' complete-options true
        zstyle ':completion:*' file-sort modification
        zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
        zstyle ':completion:*:*:*:*:descriptions' format '%F{blue}-- %D %d --%f'
        zstyle ':completion:*:*:*:*:messages' format ' %F{purple} -- %d --%f'
        zstyle ':completion:*:*:*:*:warnings' format ' %F{red}-- no matches found --%f'
        # zstyle ':completion:*:default' list-prompt '%S%M matches%s'
        # Colors for files and directory
        zstyle ':completion:*:*:*:*:default' list-colors ''${(s.:.)LS_COLORS}

        # Only display some tags for the command cd
        zstyle ':completion:*:*:cd:*' tag-order local-directories directory-stack path-directories
        zstyle ':completion:*:complete:git:argument-1:' tag-order !aliases

        # Required for completion to be in good groups (named after the tags)
        zstyle ':completion:*' group-name '''

        zstyle ':completion:*:*:-command-:*:*' group-order aliases builtins functions commands

        # Vi mode.
        export KEYTIMEOUT=1
        bindkey -v

        # Change cursor shape for different vi modes.
        function zle-keymap-select () {
            case $KEYMAP in
                vicmd) echo -ne '\e[1 q';;      # block
                viins|main) echo -ne '\e[5 q';; # beam
            esac
        }
        zle -N zle-keymap-select
        zle-line-init() {
            zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
            echo -ne "\e[5 q"
        }
        zle -N zle-line-init
        echo -ne '\e[5 q' # Use beam shape cursor on startup.
        preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

        # Edit line in vim with ctrl-e:
        autoload edit-command-line; zle -N edit-command-line
        bindkey '^e' edit-command-line
        bindkey -M vicmd '^[[P' vi-delete-char
        bindkey -M vicmd '^e' edit-command-line
        bindkey -M visual '^[[P' vi-delete

        # Backspace delete.
        bindkey -v '^?' backward-delete-char

        detach() {
          prog=$1
          shift
          nohup setsid $prog $@ > /dev/null 2>&1
        }

        source ~/.local/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
        source ~/.local/zsh-autosuggestions/zsh-autosuggestions.zsh
        source ~/.local/powerlevel10k/powerlevel10k.zsh-theme
        source ${conFlakePathRel}/resources/zsh/p10k-prompt.zsh
        source ${pkgs.zsh-system-clipboard}/share/zsh/zsh-system-clipboard/zsh-system-clipboard.zsh
      '';
    };
  };
}
