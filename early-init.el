;; early-init.el

;; 1. 强制指定 user-emacs-directory 为当前配置文件所在目录
;; 这样后续所有基于 user-emacs-directory 的变量都会指向 ~/.config/emacs/
(setq user-emacs-directory (file-name-directory (file-truename load-file-name)))

;; 2. 修正 Native Compilation 的缓存路径
;; 这一步至关重要，防止 Emacs 30 自动创建 ~/.emacs.d/eln-cache
(setq native-comp-eln-load-path
      (list (expand-file-name "eln-cache/" user-emacs-directory)))

;; 3. (可选) 禁止 package.el 在初始化时自动创建目录，直到真正加载时
(setq package-enable-at-startup nil)
