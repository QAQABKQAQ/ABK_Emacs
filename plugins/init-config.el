
;; 菜单栏
(menu-bar-mode 0)
;; 工具栏
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 0)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)
(winner-mode 1)
(load-file custom-file)
(ido-mode 1)
(ido-everywhere 1)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)

(setq create-lockfiles nil)

(defconst *is-mac* (eq system-type 'darwin))

(when *is-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none))


(use-package no-littering
  :ensure t
  :config
  ;; 1. 配合 recentf 清理
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  
  ;; 2. 备份文件设置 (现在这里是安全的)
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))

  ;; 3. 自动保存设置
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq make-backup-files t)          ;; 依然进行备份
(setq version-control t)            ;; 启用版本控制式备份
(setq backup-by-copying t)          ;; 备份时复制
(setq delete-old-versions t)        ;; 自动删除过旧的备份
(setq kept-old-versions 2)          ;; 保留最旧的2个
(setq kept-new-versions 5)          ;; 保留最新的5个

(provide 'init-config)
