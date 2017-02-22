### -=<(Kalesville)>=-

WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SHELL:=/bin/bash -eo pipefail

### docker compose
COMPOSE-FILES:=$(shell echo '-f devops/'{postgres,nginx,kalesville}/docker-compose.yml)
DOCKER-COMPOSE:=docker-compose $(COMPOSE-FILES)
OPTS:=
build-web:
	$(DOCKER-COMPOSE) build $(OPTS) web

build-web-dev:
	$(DOCKER-COMPOSE) build $(OPTS) web-dev

build-nginx-dev:
	$(DOCKER-COMPOSE) build $(OPTS) nginx-dev

up:
	$(DOCKER-COMPOSE) up -d $(OPTS) web

up-dev:
	$(DOCKER-COMPOSE) up -d $(OPTS) web-dev

down:
	$(DOCKER-COMPOSE) down

restart:
	$(DOCKER-COMPOSE) restart
### docker compose
