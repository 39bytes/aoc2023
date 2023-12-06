alias r := run
alias rp := run-python
alias l := load
alias t := test

compile day:
  ghc day{{day}}/day{{day}}.hs

run day: (compile day)
  cd day{{day}} && ./day{{day}} < input

run-python day:
  cd day{{day}} && python day{{day}}.py < input

test day: (compile day)
  cd day{{day}} && ./day{{day}} < test

load day:
  cd day{{day}} && ghci day{{day}}.hs

init day:
  ./init.sh day{{day}}

