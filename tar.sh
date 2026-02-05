#!/bin/bash

set -xe

tar czf emacs-offline.tar.gz \
  --exclude='auto-save-list' \
  --exclude='history' \
  --exclude='recentf' \
  --exclude='places' \
  --exclude='eshell' \
  --exclude='eln-cache' \
  --exclude='elpa/gnupg' \
  init.el early-init.el output.el \
  _base _modes _templates elpa emacs-themes _fonts

