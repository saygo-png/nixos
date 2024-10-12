{
  pkgs,
  conUsername,
  ...
}: {
  home-manager.users.${conUsername} = {
    home.packages = with pkgs; [
      (let
        prismlauncher-unwrapped-patched = pkgs.prismlauncher-unwrapped.overrideAttrs {
          patches = [
            (builtins.toFile "changes.patch" ''
              diff --git i/launcher/minecraft/auth/AccountData.h w/launcher/minecraft/auth/AccountData.h
              index bac77e1..91e0aea 100644
              --- i/launcher/minecraft/auth/AccountData.h
              +++ w/launcher/minecraft/auth/AccountData.h
              @@ -57,8 +57,8 @@ struct Cape {
               };

               struct MinecraftEntitlement {
              -    bool ownsMinecraft = false;
              -    bool canPlayMinecraft = false;
              +    bool ownsMinecraft = true;
              +    bool canPlayMinecraft = true;
                   Katabasis::Validity validity = Katabasis::Validity::None;
               };

              diff --git i/launcher/minecraft/auth/MinecraftAccount.h w/launcher/minecraft/auth/MinecraftAccount.h
              index f773b3b..a1347ee 100644
              --- i/launcher/minecraft/auth/MinecraftAccount.h
              +++ w/launcher/minecraft/auth/MinecraftAccount.h
              @@ -120,7 +120,7 @@ class MinecraftAccount : public QObject, public Usable {

                   [[nodiscard]] AccountType accountType() const noexcept { return data.type; }

              -    bool ownsMinecraft() const { return data.minecraftEntitlement.ownsMinecraft; }
              +    bool ownsMinecraft() const { return true; }

                   bool hasProfile() const { return data.profileId().size() != 0; }

              diff --git i/launcher/minecraft/auth/Parsers.cpp w/launcher/minecraft/auth/Parsers.cpp
              index f6179a9..87f7762 100644
              --- i/launcher/minecraft/auth/Parsers.cpp
              +++ w/launcher/minecraft/auth/Parsers.cpp
              @@ -405,8 +405,8 @@ bool parseMinecraftEntitlements(QByteArray& data, MinecraftEntitlement& output)
                   }

                   auto obj = doc.object();
              -    output.canPlayMinecraft = false;
              -    output.ownsMinecraft = false;
              +    output.canPlayMinecraft = true;
              +    output.ownsMinecraft = true;

                   auto itemsArray = obj.value("items").toArray();
                   for (auto item : itemsArray) {
              diff --git i/launcher/ui/pages/global/AccountListPage.cpp w/launcher/ui/pages/global/AccountListPage.cpp
              index abd8fa2..85b4ef9 100644
              --- i/launcher/ui/pages/global/AccountListPage.cpp
              +++ w/launcher/ui/pages/global/AccountListPage.cpp
              @@ -147,14 +147,6 @@ void AccountListPage::on_actionAddMicrosoft_triggered()

               void AccountListPage::on_actionAddOffline_triggered()
               {
              -    if (!m_accounts->anyAccountIsValid()) {
              -        QMessageBox::warning(this, tr("Error"),
              -                             tr("You must add a Microsoft account that owns Minecraft before you can add an offline account."
              -                                "<br><br>"
              -                                "If you have lost your account you can contact Microsoft for support."));
              -        return;
              -    }
              -
                   MinecraftAccountPtr account =
                       OfflineLoginDialog::newAccount(this, tr("Please enter your desired username to add your offline account."));

            '')
          ];
        };
      in (pkgs.prismlauncher.override {
        prismlauncher-unwrapped = prismlauncher-unwrapped-patched;
        jdks = [
          temurin-bin-8
          temurin-bin-17
          temurin-bin-21
        ];
      }))
    ];
  };
}
