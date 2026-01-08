
;; emacs的自定义设置转移到自定义文件中
(setq custom-file "~/.config/emacs/.emacs.custom.el")

(add-to-list 'load-path
	     (expand-file-name ( concat user-emacs-directory "plugins")))  ;; ~/.
;; 1. 加速启动 (暂时调大垃圾回收阈值)
(setq gc-cons-threshold most-positive-fixnum)

;; 2. 定义软件源 (必须在最前面！且使用 HTTP 解决卡顿)
(require 'package)
(setq package-archives '(("gnu"    . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
                         ("nongnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/nongnu/")
                         ("melpa"  . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))


(unless package-archive-contents
  (message "Refeshing package archives...")
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'init-package)
(require 'init-config)

