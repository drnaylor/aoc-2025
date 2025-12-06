# Advent of Code 2025

The 2025 edition of Advent of Code, written in Scala 3.7 this year! Uses sbt 1.11 as the build tool.

**Puzzle inputs are not included**.[^1] Create the `src/main/resources` directory and add your inputs with the filename `day[nn].txt`, replacing `[nn]` with the two digit day number (so, the input for day 1 is `day01.txt`).

Tests are written in ScalaTest with ScalaCheck property based testing where appropriate.

By default, the code will only run the last day. To run all days, add the argument "all" to the command line. To run specific days, add the various day numbers to the commandline. Be aware, I've not added error handling for the command args parser so it'll just blow up if you get it wrong.

[^1]: See https://adventofcode.com/2024/about, specifically "Can I copy/redistribute part of Advent of Code?", which says:

    "If you're posting a code repository somewhere, please don't include parts of Advent of Code like the puzzle text or your inputs."