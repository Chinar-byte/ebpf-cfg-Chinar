eBPF Control-Flow-Graph
=======================

Some starter code that shows how to use [`ebpf-tools`](https://github.com/kfl/ebpf-tools).

Using [`ebpf-tools`](https://github.com/kfl/ebpf-tools) is currently a
bit gnarly, because it isn't released to Hackage yet. Thus, you must
have a `cabal.project` file with the following content:

```cabal
-- Using the unreleased v. 0.1.0.0 of ebpf-tools from github
source-repository-package
    type: git
    location: https://github.com/kfl/ebpf-tools.git
    tag: d87166854082e68890b93a531a109838a8241958

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

