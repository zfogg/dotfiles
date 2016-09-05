" ftdetect/cpp.vim


au BufRead,BufNewFile
            \ *.cc,*.c++,*.cpp,*.cxx,.C
            \,*.hh,*.h++,*.hpp,*.hxx,.H
    \ setfiletype cpp
