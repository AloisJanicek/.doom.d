;;;  -*- lexical-binding: t; -*-

(load! "+bindings")
(load! "+hydras")
(load! "+after")
(load! "+def")

(add-hook 'org-load-hook '(lambda () (setq org-modules (append '(org-man org-eww org-protocol org-habit) org-modules))))

(set-popup-rule! "*backtrace\*" :size 0.4 :side 'right :select t)
