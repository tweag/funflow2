# Funflow 1 rewrite completion

- [x] Programmatic API in Haskell
- (!) Multiple types of computation:
  - [x] Pure functions
  - [x] IO actions
  - [x] External computations
  - (!) User-defined effects _[should be allowed extension?]_
- [x] Safe workflow composition
- [x] Caching steps using CAS
- [ ] Failure handling
  - [ ] Handling failures inside workflows
  - [ ] Resuming workflows from the last successful point once some external error has been corrected
- [ ] Parallel execution: execute sections of workflows in parallel where this is possible
- [ ] Task distribution: external steps can be serialised and run remotely
  - [ ] An in-memory coordinator, useful for single-process computation
  - [ ] A Redis based coordinator
  - [ ] A SQLite coordinator
- [ ] Distributing complete workflows, rather than just individual steps (funflow-jobs)
