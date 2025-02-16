{
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    home.packages = [
      (let
        prismlauncher-unwrapped-patched = pkgs.prismlauncher-unwrapped.overrideAttrs {
          patches = [
            (builtins.toFile "changes.patch" ''
              diff --unified --recursive --text package.orig/launcher/ui/pages/global/AccountListPage.cpp package.new/launcher/ui/pages/global/AccountListPage.cpp
              --- package.orig/launcher/ui/pages/global/AccountListPage.cpp   2023-11-17 18:11:46.717525260 +0100
              +++ package.new/launcher/ui/pages/global/AccountListPage.cpp    2023-11-17 18:13:11.560520571 +0100
              @@ -146,13 +146,13 @@

               void AccountListPage::on_actionAddOffline_triggered()
               {
              -    if (!m_accounts->anyAccountIsValid()) {
              +    /*if (!m_accounts->anyAccountIsValid()) {
                       QMessageBox::warning(this, tr("Error"),
                                            tr("You must add a Microsoft account that owns Minecraft before you can add an offline account."
                                               "<br><br>"
                                               "If you have lost your account you can contact Microsoft for support."));
                       return;
              -    }
              +    }*/

                   MinecraftAccountPtr account =
                       OfflineLoginDialog::newAccount(this, tr("Please enter your desired username to add your offline account."));            '')
          ];
        };
      in
        pkgs.prismlauncher.override {
          prismlauncher-unwrapped = prismlauncher-unwrapped-patched;
        })
    ];
  };
}
