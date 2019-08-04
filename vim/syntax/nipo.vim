if exists('b:current_syntax')
    finish
endif

syntax keyword nipoKeyword lexer parser where token rules start whitespace pos
highlight default link nipoKeyword Keyword

syntax match nipoArrow "->"
highlight default link nipoArrow Operator

syntax match nipoEq "="
highlight default link nipoEq Operator

syntax match nipoBar "|"
highlight default link nipoBar Operator

syntax match nipoQMark "?"
highlight default link nipoQMark Operator

syntax match nipoStar "*"
highlight default link nipoStar Operator

syntax match nipoPlus "+"
highlight default link nipoPlus Operator

syntax region nipoToken start=/'/ end=/'/
highlight default link nipoToken Character

syntax region nipoPosix start=/\[\[:/ end=/:\]\]/
highlight default link nipoPosix Character

syntax region nipoAction start=/{%/ end=/%}/
highlight default link nipoAction PreProc

syntax region nipoComment start=/#/ end=/\n/
highlight default link nipoComment Comment

let b:current_syntax = 'nipo'

