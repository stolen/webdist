PROJECT = webdist
ERLC_OPTS = +debug_info +warn_export_vars +warn_shadow_vars +warn_obsolete_guard +bin_opt_info +warn_export_all

DEPS = cowboy
dep_cowboy = git https://github.com/ninenines/cowboy.git 1.1.x

SHELL_OPTS = -proto_dist webdist -sname webdist_test #-epmd_module webdist_epmd 

include erlang.mk
