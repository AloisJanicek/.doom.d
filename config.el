;;;  -*- lexical-binding: t; -*-

(load! "+after")
(load! "+def")
(load! "+hydras")
(load! "+bindings")

(add-hook 'org-load-hook '(lambda () (setq org-modules (append '(org-man org-eww org-protocol org-habit) org-modules))))

(set-popup-rule! "*backtrace\*" :size 0.4 :side 'right :select t)
