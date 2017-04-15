### -=<(Kalesville)>=-

WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SHELL:=/bin/bash -eo pipefail

### docker compose
COMPOSE-FILES:=$(shell echo '-f devops/'{postgres,nginx,kalesville}/docker-compose.yml)
DOCKER-COMPOSE:=docker-compose $(COMPOSE-FILES)
OPTS:=
build-web:
	$(DOCKER-COMPOSE) build $(OPTS) web

build-nginx:
	$(DOCKER-COMPOSE) build $(OPTS) nginx

build-web-dev:
	$(DOCKER-COMPOSE) build $(OPTS) web-dev

build-nginx-dev:
	$(DOCKER-COMPOSE) build $(OPTS) nginx-dev

up:
	$(DOCKER-COMPOSE) up -d $(OPTS) nginx

up-dev:
	$(DOCKER-COMPOSE) up -d $(OPTS) nginx-dev

down:
	$(DOCKER-COMPOSE) down

restart:
	$(DOCKER-COMPOSE) restart

logs:
	$(DOCKER-COMPOSE) logs -f
### docker compose

### db
migrate-dev:
	for f in migrations/*.sql; do \
		echo '### running' $$f ;\
		docker exec -i -ePGUSER=mysql -ePGPASSWORD=mysql -ePGDATABASE=kalesville-web \
		kalesville-pg-dev psql -v ON_ERROR_STOP=1 < $$f ;\
		echo '### done' $$f ;\
	done
### db

### release
release-patch:
	@git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment patch version by Makefile' \
		$$(git describe | ./scripts/increment-version patch) ;\
	git push --tags

release-minor:
	@git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment minor version by Makefile' \
		$$(git describe | ./scripts/increment-version minor) ;\
	git push --tags

release-major:
	@git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment major version by Makefile' \
		$$(git describe | ./scripts/increment-version major) ;\
	git push --tags
### release
