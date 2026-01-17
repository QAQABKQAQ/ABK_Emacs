
;; --- 1. Use-Package 全局设定 ---
;; 确保 use-package 自身已被加载
(require 'use-package)

;; 默认让每个包都自动安装 (不用每次都写 :ensure t)
(setq use-package-always-ensure t)

;; 默认延迟加载 (提升启动速度，除非特别指定 :demand t)
(setq use-package-always-defer t)

;; 即使在 Daemon 模式下也不强制立即加载所有包 (按需加载)
(setq use-package-always-demand nil)

;; 最小化展开宏 (让生成的代码更简洁，方便调试)
(setq use-package-expand-minimally t)

;; 安装过程详细打印 (如果出错方便看日志，稳定后可设为 nil)
(setq use-package-verbose t)

;; --- 2. 基础工具类插件 ---

;; 快速重启 Emacs 的命令 (M-x restart-emacs)
(use-package restart-emacs
  :ensure t)

;; 增强的序列处理库 (很多新插件依赖这个)
(use-package seq
  :ensure t)


(use-package avy
  :ensure t
  :bind
  (("C-'" . avy-goto-char-timer)  ;; 核心命令：按 C-'，再输入你想去的字符（支持连续输入过滤）
   ("M-g l" . avy-goto-line)      ;; 快速跳到某一行（比 M-g g 这种要输入数字的快多了）
   ))


;; 推荐设置：让 Avy 更懂你
(setq avy-background t) ;; 即使在背景中也能看清文字
(setq avy-timeout-seconds 0.5) ;; 输入间隔，给你 0.3 秒连按字符来精确定位


;; 自动管理垃圾文件 (把自动保存、备份文件统一放到 var 目录下，保持整洁)
(use-package no-littering
  :ensure t
  :demand t  ;; 立即加载，因为下面的路径配置依赖它
  :config
  ;; 1. 配合 recentf (最近文件列表)，排除垃圾目录
  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude no-littering-var-directory)
    (add-to-list 'recentf-exclude no-littering-etc-directory))
  
  ;; 2. 设置备份文件存放路径 (存到 .emacs.d/var/backup/)
  (setq backup-directory-alist
        `(("." . ,(no-littering-expand-var-file-name "backup/"))))

  ;; 3. 设置自动保存文件存放路径 (存到 .emacs.d/var/auto-save/)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; --- 3. 备份策略设置 ---
;;(setq make-backup-files t)      ;; 开启：编辑文件时自动备份
(setq make-backup-files nil);; 带有~的文件
(setq auto-save-default nil);; 自动保存文件，带有#的文件
(setq create-lockfiles nil);;.#文件，主要告诉别人这个文件我在使用
(setq version-control t)        ;; 开启：使用版本控制方式命名备份文件 (file.~1~, file.~2~)
(setq backup-by-copying t)      ;; 开启：通过复制方式备份 (防止破坏硬链接)
(setq delete-old-versions t)    ;; 开启：自动删除过旧的备份文件
(setq kept-old-versions 2)      ;; 设置：保留最旧的 2 个版本
(setq kept-new-versions 5)      ;; 设置：保留最新的 5 个版本


;; --- 4. 主题设置 (Catppuccin) ---
(use-package catppuccin-theme
  :ensure t
  :demand t  ;; 立即加载，否则启动时会有一瞬间白屏
  :init
  ;; 设置主题风味 (latte, frappe, macchiato, mocha)
  (setq catppuccin-flavor 'macchiato)
  :config
  ;; 加载主题，t 表示不询问确认
  (load-theme 'catppuccin t))

;; 再次确认加载 (防止某些情况失效)
(load-theme 'catppuccin :no-confirm)



;; --- 5. 字体自动化配置函数 ---
(defun my-setup-fonts ()
  "自动检测系统字体并设置，优先使用 JetBrains Mono。"
  (interactive)
  ;; 1. 设置英文字体 (等宽)
  (let ((font-name (cond ((find-font (font-spec :name "JetBrains Mono")) "JetBrains Mono")
                         ((find-font (font-spec :name "Fira Code")) "Fira Code")
                         ((find-font (font-spec :name "Monaco")) "Monaco")
                         (t "Menlo")))) ;; 如果都没有，回退到 Menlo
    ;; 设置默认字体大小为 140 (即 14pt)
    (set-face-attribute 'default nil :font font-name :height 140))

  ;; 2. 设置中文字体 (防止中英文不对齐)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family "PingFang SC"))) ;; Mac 上首选萍方
  
  ;; 3. 调整中文字体缩放比例 (1.2倍通常能完美对齐)
  (add-to-list 'face-font-rescale-alist '("PingFang SC" . 1.2)))

;; 这里的逻辑是：如果是 Daemon 模式启动，等创建 Frame 后再设字体；否则直接设
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (my-setup-fonts))))
  (my-setup-fonts))


;; --- 6. 核心补全 UI (Telescope 替代方案) ---

;; [UI] Vertico: 垂直补全列表 (替代 Ivy)
(use-package vertico
  :ensure t
  :demand t
  :init
  (vertico-mode) ;; 开启 Vertico 模式
  :config
  (setq vertico-cycle t)) ;; 开启循环选择 (到底部后回到顶部)

;; [UI] Vertico-Posframe: 让补全列表像 Telescope 一样悬浮居中
(use-package vertico-posframe
  :ensure t
  :after vertico ;; 在 Vertico 加载后再加载
  :config
  (vertico-posframe-mode 1) ;; 开启悬浮模式
  ;; 设置边框宽度
  (setq vertico-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
  ;; 设置位置处理函数：居中显示
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center))

;; [算法] Orderless: 强大的模糊搜索算法 (空格分隔)
(use-package orderless
  :ensure t
  :custom
  ;; 设置补全样式：优先用 orderless，basic 作为保底
  (completion-styles '(orderless basic))
  ;; 文件路径补全使用 partial-completion (比如 /u/b/z -> /usr/bin/zsh)
  (completion-category-overrides '((file (styles partial-completion)))))

;; [信息] Marginalia: 在补全列表旁显示详细信息 (大小、时间、描述)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode)) ;; 开启边注模式

;; [命令] Consult: 增强版搜索命令 (替代 Counsel/Swiper)
(use-package consult
  :ensure t
  :bind (;; 替代 C-s，支持实时预览搜索
         ("C-s" . consult-line)
         ;; 替代 C-x b，切换 Buffer 时有预览
         ("C-x b" . consult-buffer)
         ;; 类似 Telescope find_files (查找文件)
         ("C-c f" . consult-find)
         ;; 类似 Telescope live_grep (需安装 ripgrep)
         ("C-c g" . consult-ripgrep)
         ;; 类似 Telescope outline (大纲跳转)
         ("M-g o" . consult-outline))
  :config
  ;; 开启自动预览功能 (按上下键时直接预览)
  (setq consult-preview-key 'any))


;; --- 7. Git 管理 ---
(use-package magit
  :bind (("C-x g" . magit-status)) ;; 绑定 C-x g 打开 Git 面板
  :config
  ;; 写 Commit 信息时关闭拼写检查 (防止红线干扰)
  (add-hook 'git-commit-setup-hook 'turn-off-flyspell))


;; --- 8. 编辑增强 ---
(use-package crux
  :bind ("C-c k" . crux-smart-kill-line)) ;; 智能删除行 (利用率很高)


;; --- 9. 图标与文件管理 ---
(use-package nerd-icons
  :ensure t) ;; 安装图标库 (Dirvish 依赖它)

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode) ;; 让 Dirvish 接管 Emacs 默认的文件管理器 Dired
  :config
  ;; 顶部显示：路径导航条 + 剩余空间
  (setq dirvish-header-line-format '(:left (path) :right (free-space)))
  ;; 底部显示：排序方式 + 链接信息 + 复制/索引状态
  (setq dirvish-mode-line-format '(:left (sort symlink) :right (omit yank index)))
  
  ;; [重要] 设置文件列表显示的属性
  ;; 注意：去掉了 collapse-subtree 以防止计算宽度报错
  (setq dirvish-attributes 
        '(nerd-icons      ;; 图标
          subtree-state   ;; 折叠状态箭头
          file-time       ;; 修改时间
          file-size       ;; 文件大小
          git-msg))       ;; Git 提交信息
  
  :bind
  (("C-c d" . dirvish)      ;; 开启全屏文件管理
   ("C-c s" . dirvish-side) ;; 侧边栏模式打开 (Side)
   :map dirvish-mode-map    ;; 仅在 Dirvish 界面生效的快捷键
   ("a"   . dirvish-quick-access)   ;; 快速访问常用目录
   ("f"   . dirvish-file-info-menu) ;; 文件详情菜单
   ("y"   . dirvish-yank-menu)      ;; 复制菜单
   ("s"   . dirvish-quicksort)      ;; 排序菜单
   ("TAB" . dirvish-subtree-toggle) ;; TAB键：展开/折叠目录
   ))


;; --- 10. 编程语言支持 ---

;; SQL 缩进规则优化
(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode)) ;; 在 SQL 模式下自动开启

;; Org-Mode 设置
(use-package org
  :ensure nil ;; Org 是内置的，不需要 ensure
  :config
  ;; 允许在 Org 代码块中执行的语言
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sql . t)      ;; 允许运行 SQL
     (shell . t)))) ;; 允许运行 Shell

;; 快捷键提示 (按下前缀键后弹出提示)
(use-package which-key
  :config (which-key-mode))


;; --- 11. 加载其他模块 ---
;; LSP 配置 (lsp.el)
;;(require 'lsp)
;;(require 'fast-lsp)
(require 'lsp-eglot)
;; 声明本文件已提供 init-package 功能
(provide 'init-package)
