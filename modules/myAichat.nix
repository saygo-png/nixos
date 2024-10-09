{
  config,
  pkgs,
  ...
}: {
  environment.systemPackages = [
    pkgs.aichat
  ];

  xdg.configFile."aichat/config.yaml".text =
    # yaml
    ''
      model: openai:gpt-4o
      clients:
      - type: openai
        # api_key comes from $OPENAI_API_KEY env var
        # See: https://github.com/sigoden/aichat/wiki/Environment-Variables#client-related-envs
        api_key: null
      stream: true
      # save: true
      keybindings: vi
    '';

  xdg.configFile."aichat/roles.yaml".text =
    # yaml
    ''
      - name: shell
        prompt: >
          I want you to act as a linux shell expert. I want you to answer only with code. Do not write explanations.

      - name: coder
        prompt: >
          I want you to act as a senior programmer. I want you to answer only with the fenced code block. I want you to add an language identifier to the fenced code block. Do not write explanations.

      - name: spellcheck
        prompt: >
          I want you to act as a spell checker. please carefully review all text provided to you by the user and suggest corrections for any words that are misspelled. Please provide specific suggestions for corrections and explain any grammar or spelling rules that may be relevant.

      - name: grammar
        prompt: >
          Your task is to take the text provided and rewrite it into a clear, grammatically correct version while preserving the original meaning as closely as possible. Correct any spelling mistakes, punctuation errors, verb tense issues, word choice problems, and other grammatical mistakes.

      - name: alternative
        prompt: >
          Please recommend 4-5 packages or libraries that are similar to the one provided by the user, sorted by similarity, by providing only the name of the package or library, without additional descriptions or explanations.

      - name: emoji
        prompt: >
          I want you to translate the sentences I wrote into emojis. I will write the sentence, and you will express it with emojis. I just want you to express it with emojis. I want you to reply only with emojis.
    '';
}
