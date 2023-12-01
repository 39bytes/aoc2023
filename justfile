alias r := run
alias l := load

compile day:
  ghc day{{day}}/day{{day}}.hs

run day: (compile day)
  cd day{{day}} && ./day{{day}} < input

test day: (compile day)
  cd day{{day}} && ./day{{day}} < test

load day:
  cd day{{day}} && ghci day{{day}}.hs

