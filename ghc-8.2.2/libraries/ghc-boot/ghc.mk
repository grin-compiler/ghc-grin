libraries/ghc-boot_PACKAGE = ghc-boot
libraries/ghc-boot_dist-install_GROUP = libraries
$(if $(filter ghc-boot,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-boot,dist-boot,0)))
$(if $(filter ghc-boot,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-boot,dist-install,1)))
$(if $(filter ghc-boot,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-boot,dist-install,2)))
