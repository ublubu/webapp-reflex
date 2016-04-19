{ reflex-platform, ... }: reflex-platform.ghcjs.override {
  overrides = self: super: {
    webapp-api = self.callPackage ../api {};
    ghcjs-servant-client = self.callPackage ./lib/ghcjs-servant-client {};
  };
}