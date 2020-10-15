# ninety-nine

## H-99: Ninety-Nine Haskell Problems
A list of the problems can be found here: 
https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

Or here: https://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/

## This project
I started this as a one file experiment, mostly to start using the language, without getting too distracted with other things.

It eventually was turned into a gist, where a very rudimentary versioning, sort of kept things sane for a little while. A very little while.

The gist can ge found here:
https://gist.github.com/gustavofranke/f265de2a9339ddcaa3e489a8b05eafe1

## Some commands
The project was setup with the following commands:
```
stack new ninety-nine simple
cd ninety-nine/
stack setup
```

The cycle for building and testing is:
```
stack build
stack test
```

## Some generated files and reports
linter file generation: `hlint --default > .hlint.yaml` (done only once)
linter report generation: `hlint . --report`
file is left at: `ninety-nine/report.html`

documentation report generation: `stack haddock`
file is left at: `ninety-nine/.stack-work/install/<cpu-os>/<some-hash>/8.8.4/doc/index.html`

code coverage report generation: `stack clean && stack test --coverage && stack hpc report .`
file is left at: `ninety-nine/.stack-work/install/<cpu-os>/<some-hash>/8.8.4/hpc/combined/custom/hpc_index.html`
