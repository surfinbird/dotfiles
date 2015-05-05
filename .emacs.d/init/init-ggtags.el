;; GNU Global Tags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

(setq ggtags-completing-read-function nil)

(provide 'init-ggtags)
