WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
MIGRATIONS:=$(shell find $(WD)/migrations -type f | sort)

run-migrations: APP_ENV?=development
run-migrations:
	@export APP_ENV=$(APP_ENV) ;\
	for m in $(MIGRATIONS); do\
		echo '### Running' $$m ;\
		$$m ;\
	done

run-web: APP_ENV?=development
run-web: PORT?=5000
run-web:
	@export APP_ENV=$(APP_ENV) ;\
	trap 'kill %1; kill %2' SIGINT ;\
	clackup --server :wookie \
		--port $(PORT) \
		$(WD)/app.lisp & \
	npm run dev

build-node:
	@$(error 'not implemented yet')

build-docker:
	@$(error 'not implemented yet')
