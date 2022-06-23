PROJECT = ragno
PROJECT_DESCRIPTION = Ragno 
PROJECT_VERSION = 0.1.0
LOCAL_DEPS += inets ssl
BUILD_DEPS += relx
ERLC_OPTS = +debug_info

#DEPS += worker_pool jiffy
#dep_worker_pool = hex 6.0.1

include erlang.mk
