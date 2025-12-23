{
  lib,
  pkgs,
  conUsername,
  ...
}: {
  programs.fish.enable = true;
  environment.pathsToLink = ["/share/fish"];

  stylix.targets.fish.enable = false;

  home-manager.users.${conUsername} = {
    stylix.targets.fish.enable = false;
    programs.alacritty.settings.terminal.shell = lib.getExe pkgs.fish;
    programs.yazi.enableFishIntegration = true;
    programs.fish = {
      enable = true;
      interactiveShellInit =
        # Fish
        ''
          set fish_greeting
          set -g fish_transient_prompt 1
          set -g __fish_git_prompt_show_informative_status 1
          fish_vi_key_bindings

          # hjkl in completion menu
          bind -M insert \cj down-line
          bind -M insert \ck up-line
          bind -M insert \ch backward-char
          bind -M insert \cl forward-char

          bind -M insert \cp clear-screen

          function fish_default_mode_prompt --description "Display vi prompt mode"
            switch $fish_bind_mode
            case default
              set_color --bold green
              echo '<'
              set_color normal
            case insert
              set_color --bold green
              echo '>'
              set_color normal
            case replace_one
              set_color --bold green
              echo '/'
              set_color normal
            case replace
              set_color --bold cyan
              echo '^'
              set_color normal
            case visual
              set_color --bold magenta
              echo '*'
              set_color normal
            end
          end

          # This is used for vi mode but I don't like it
          function fish_mode_prompt; end

          function fish_prompt --description 'Write out the prompt'
            set -l last_pipestatus $pipestatus
            set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
            set -l normal (set_color normal)
            set -q fish_color_status
            or set -g fish_color_status red

            # Nix shell
            set -l nix_shell_info (
              if echo "$PATH" | rg -q /nix/store
                echo -n "(nix shell)"
              end
            )

            # Color the prompt differently when we're root
            set -l color_cwd $fish_color_cwd
            set -l root_prefix ""
            if functions -q fish_is_root_user; and fish_is_root_user
              if set -q fish_color_cwd_root
                set color_cwd $fish_color_cwd_root
              end
              set root_prefix '#'
            end

            # Write pipestatus
            # If the status was carried over (if no command is issued or if `set` leaves the status untouched), don't bold it.
            set -l bold_flag --bold
            set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
            if test $__fish_prompt_status_generation = $status_generation
              set bold_flag
            end
            set __fish_prompt_status_generation $status_generation
            set -l status_color (set_color $fish_color_status)
            set -l statusb_color (set_color $bold_flag $fish_color_status)
            set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

            if contains -- --final-rendering $argv
              echo -n -s (fish_default_mode_prompt)
            else
              echo -n -s \
                (prompt_login)" " \
                (set_color $color_cwd) (prompt_pwd) \
                $normal (fish_vcs_prompt) $normal" " $prompt_status" " \
                $nix_shell_info
              echo ""
              echo -n -s $root_prefix (fish_default_mode_prompt)
            end
          end
        '';
    };
  };
}
