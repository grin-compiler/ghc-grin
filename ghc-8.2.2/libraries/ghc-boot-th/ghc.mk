libraries/ghc-boot-th_PACKAGE = ghc-boot-th
libraries/ghc-boot-th_dist-install_GROUP = libraries
$(if $(filter ghc-boot-th,$(PACKAGES_STAGE0)),$(eval $(call build-package,libraries/ghc-boot-th,dist-boot,0)))
$(if $(filter ghc-boot-th,$(PACKAGES_STAGE1)),$(eval $(call build-package,libraries/ghc-boot-th,dist-install,1)))
$(if $(filter ghc-boot-th,$(PACKAGES_STAGE2)),$(eval $(call build-package,libraries/ghc-boot-th,dist-install,2)))
