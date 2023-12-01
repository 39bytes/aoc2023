run day:
  cd day{{day}} && ghc day{{day}}.hs && ./day{{day}} < input

load day:
  cd day{{day}} && ghci day{{day}}.hs
