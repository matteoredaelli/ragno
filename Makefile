PROJECT = ragno
PROJECT_DESCRIPTION = New project
PROJECT_VERSION = 0.1.0

LOCAL_DEPS += inets ssl
BUILD_DEPS += relx
ERLC_OPTS = +debug_info

DEPS += jsone
DEPS += worker_pool
DEPS += couchbeam

#DEPS += jiffy
DEPS += jsx

DEPS += emqtt

dep_emqtt = git https://github.com/emqx/emqtt
dep_jsone = hex 1.7.0

dep_worker_pool = hex 6.0.1



include erlang.mk
