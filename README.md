Hopper offers primitives for distributed task processing in Haskell, simplifying asynchronous workflows and
efficient task scheduling.

# (Re-)generate hopper-thrift protocol

`pinch-gen` is an extra-package in the cabal project. Run

```
$ cabal exec pinch-gen -- --in hopper-thrift/thrift/hopper.thrift --out hopper-thrift/gen-src --hashable-vec-mod Data.Vector.Instances --module-prefix Hopper.Thrift.
```

to (re-)generate the Thrift data types.

# Example

hopper-distributed comes with an example application. The example application binary can run both scheduler
and executor based on the flags you pass to them:

To launch a scheduler on port the (default) port 4000 execute

```
$ cabal run hopper-distributed
```

In separate terminal you can then execute

```
$ cabal run hopper-distributed -- executor
```

to launch an executor. The executor looks for a running scheduler on
localhost:4000. If both are running you should see them working and
logging.

If you want to be adventurous, you can run not one but many executors
by opening many terminals and run the command to launch executors in
them.

Play with terminating executors and scheduler and see what happens.