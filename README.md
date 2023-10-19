eBPF Control-Flow-Graph
=======================

Some starter code that shows how to use [`ebpf-tools`](https://github.com/kfl/ebpf-tools).

Using [`ebpf-tools`](https://github.com/kfl/ebpf-tools) is currently a
bit gnarly, because it isn't released to Hackage yet. Thus, you must
have a `cabal.project` file with the following content:

```cabal
packages: .

-- Using the unreleased v. 0.1.0.0 of ebpf-tools from github
source-repository-package
    type: git
    location: https://github.com/kfl/ebpf-tools.git
    tag: d87166854082e68890b93a531a109838a8241958

```

As demonstrated in this project.


How to run
----------

