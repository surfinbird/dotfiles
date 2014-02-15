;; Use pep8 and pylint when running compile for python code
(require `tramp)
(autoload 'python-pep8 "python-pep8")
(autoload 'pep8 "python-pep8")

(autoload 'python-pylint "python-pylint")
(autoload 'pylint "python-pylint")

(elpy-enable)
;(setq elpy-rpc-backend "jedi")

(provide 'init-python)
