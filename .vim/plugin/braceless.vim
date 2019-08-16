" braceless
scriptencoding utf-8


aug RcPlugin__braceless
    au!
    au FileType haml,yaml
                \,coffee
                \,python,python3
                \,jade,plug
        \ BracelessEnable +indent +fold +highlight
aug END
