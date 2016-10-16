### -=<(Kalesville)>=-

WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SELF:=$(firstword $(MAKEFILE_LIST))
SHELL:=/bin/bash -e -o pipefail

### WEB
web-dev: APP_ENV?=development
web-dev: PORT?=5000
web-dev:
	@export APP_ENV=$(APP_ENV) ;\
	trap 'kill %1; kill %2' SIGINT ;\
	clackup --server :wookie \
		--port $(PORT) \
		$(WD)/app.lisp & \
	npm run dev
### WEB

### MIGRATE
MIGRATIONS:=$(shell find $(WD)/migrations -type f -name '*.ros' | sort)

run-migrations: APP_ENV?=development
run-migrations:
	@export APP_ENV=$(APP_ENV) ;\
	for m in $(MIGRATIONS); do\
		echo '### running' $$m ;\
		$$m ;\
	done
### MIGRATE

### BUILD
build-node:
	@$(error 'not implemented yet')

build-docker:
	@$(error 'not implemented yet')
### BUILD

### TESTS
test: TEST-DB?=$(WD)/test-db.sqlite
test:
	@if [ -e $(TEST-DB) ]; then\
		(rm $(TEST-DB) && echo 'removed $(TEST-DB)') \
	fi
	$(MAKE) -f $(SELF) run-migrations APP_ENV=test
### TESTS
