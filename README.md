eBPF Control-Flow-Graph
=======================

Some starter code that shows how to use [`ebpf-tools`](https://github.com/kfl/ebpf-tools).

Start by cloning or forking this repository, and then work on you own
clone.

All the Haskell code of interest is in
[`app/Main.hs`](./app/Main.hs). The code roughly does two things: it
builds a control-flow-graph (CFG) and it visualise the CFG using
[graphviz](https://graphviz.org/).

You can find some sample eBPF programs in the [`examples`](./examples)
directory.

The set up in this project assumes that you are using `cabal` to build
your project.


Cabal details
-------------

Using [`ebpf-tools`](https://github.com/kfl/ebpf-tools) is currently a
bit gnarly, because it isn't released to Hackage yet. Thus, you must
have a `cabal.project` file with the following content:

```cabal
-- Using the unreleased v. 0.2.0.0 of ebpf-tools from github
source-repository-package
    type: git
    location: https://github.com/kfl/ebpf-tools.git
    tag: 3024cfaddd9160bb746a2b20d982c511791c8644
```

As demonstrated in this project.


Visualise the CFG
-----------------

To make a `dot` file of the CFG for an eBPF assembler file, say
`examples/add.asm`, run the command:

```
cabal run ebpf-cfg -- examples/add.asm add.dot
```

To make a PDF out of the `dot` file run the command (requires
[graphviz](https://graphviz.org/)):

```
dot -Tpdf add.dot -o add.pdf
```


What if I'm using `stack`?
--------------------------

If you are using `stack` then you'll want to add a `stack.yaml` file
with you favourite `stack` configuration, and then also the following
for `ebpf-tools`:

```yaml
extra-deps:
  - github: kfl/ebpf-tools
    commit: 3024cfaddd9160bb746a2b20d982c511791c8644
```

(You might also need to delete the `cabal.project` file. I don't know,
I no longer use `stack`. If you have good `stack` advice that should
be included here, then please make a PR.)
