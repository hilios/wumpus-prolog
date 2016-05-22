Hunt the Wumpus *in Prolog*
---------------

Artificial intelligence agent for the Hunt the Wumpus game implemented in Prolog.

#### Running:

Run in shell throug `swipl`:

```sh
$ ./wumpus.sh src/naive.pl
```

Using SWI Prolog interface:

```shell
$ swipl
?- [src/world], [src/main], [src/naive].
true.
?- run.
```

#### Random World generation

To generate a random world at each run user the alternate predicate:

```shell
$ swipl
?- [src/main], [src/naive].
true.
?- run(random).
```
