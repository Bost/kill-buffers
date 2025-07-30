# kill-buffers.el

Kill various unwanted buffers in Emacs with specialized functions for different buffer types.

## Installation

### Spacemacs
```elisp
;; In dotspacemacs/user-config:
(use-package kill-buffers)

;; In dotspacemacs-additional-packages:
(kill-buffers :location (recipe :fetcher github :repo "Bost/kill-buffers"))
```

## Dependencies
- `term` (for term-send-eof)
- `cl-lib` (for cl-remove-if-not, cl-find)
- `cider-connection` (for cider-quit)

## Key Functions

### Buffer Killing
- `kb-kill-buffers--force` - Kill buffers matching regex (interactive)
- `kb-kill-buffers--forcefully` - Kill buffers matching regex (programmatic)
- `kb-kill-buffers--unwanted` - Kill predefined unwanted buffers
- `kb-kill-buffers--magit` - Kill all Magit buffers
- `kb-kill-buffers--dired` - Kill all Dired buffers

### Smart Buffer Closing
- `kb-close-buffer` - Intelligently close current buffer:
  - CIDER REPL buffers: calls `cider-quit`
  - Terminal buffers: sends EOF
  - Other buffers: standard kill

## Customization

### Unwanted Buffer Lists
Customize which buffers are killed by `kb-kill-buffers--unwanted`:

```elisp
;; Magit buffer modes to kill
(setq kb-magit-unwanted-modes
      '(magit-status-mode magit-log-mode magit-diff-mode ...))

;; All unwanted buffer modes (includes Magit + others)
(setq kb-all-unwanted--modes
      '(dired-mode Man-mode woman-mode ...))

;; Specific buffer names to kill
(setq kb-unwanted-buffers
      '("*Backtrace*" "*Help*" "*Warnings*" ...))
```

## Features

- **Smart killing**: Different strategies for different buffer types
- **Bulk operations**: Kill multiple buffers at once by type or pattern
- **Customizable**: Configure which buffers are considered "unwanted"
- **Window management**: Automatically balances windows after closing buffers
- **Safe**: Handles special buffers (CIDER REPL, terminal) appropriately

## Author
Rostislav Svoboda - Rostislav.Svoboda@gmail.com

## License
GPLv3+, Copyright (C) 2020 - 2025 Rostislav Svoboda
