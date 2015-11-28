PROJECT = webdist

DEPS = cowboy
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.1.x

SHELL_OPTS = -epmd_module webdist_epmd -proto_dist webdist -sname webdist_test

include erlang.mk
