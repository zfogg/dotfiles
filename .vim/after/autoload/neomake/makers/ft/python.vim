" after/autoload/neomake/makers/ft/python
scriptencoding utf-8


function! neomake#makers#ft#python#EnabledMakers() abort
    "return ['flake8', 'frosted', 'mypy', 'pep257', 'pep8', 'py3kwarn', 'pycodestyle', 'pydocstyle', 'pyflakes', 'pylama', 'pylint', 'python', 'vulture']
    "return ['python', 'frosted', 'flake8', 'pylint']
    return ['python', 'frosted', 'flake8', 'mypy', 'pyflakes']
endfunction

