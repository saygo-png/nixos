{
  pkgs,
  conHome,
  ...
}: {
  environment.systemPackages = with pkgs; [
    (writeShellApplication {
      name = "update-secrets.sh";
      runtimeInputs = [coreutils];
      text = ''
        key=$(cat ${conHome}/Sync/secrets/openai.txt)
        echo "$key"
        export OPENAI_API_KEY="$key"
        echo "$OPENAI_API_KEY"
      '';
    })
  ];
}
