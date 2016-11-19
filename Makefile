### -=<(Kalesville)>=-

WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SELF:=$(firstword $(MAKEFILE_LIST))
SHELL:=/bin/bash -e -o pipefail

### web
web-dev: APP_ENV?=development
web-dev: PORT?=5000
web-dev:
	@export APP_ENV=$(APP_ENV) ;\
	trap 'kill %1; kill %2' SIGINT ;\
	clackup --server :wookie \
		--port $(PORT) \
		$(WD)/app.lisp & \
	npm run dev
### web

### migrate
MIGRATIONS:=$(shell find $(WD)/scripts/migrations -type f -name '*.ros' | sort)

run-migrations: APP_ENV?=development
run-migrations:
	@export APP_ENV=$(APP_ENV) ;\
	for m in $(MIGRATIONS); do\
		echo '### running' $$m ;\
		$$m ;\
	done
### migrate

### build
build-node:
	@$(error 'not implemented yet')
### build

### tests
test: TEST-DB?=$(WD)/test-db.sqlite
test:
	@if [ -e $(TEST-DB) ]; then\
		(rm $(TEST-DB) && echo 'removed $(TEST-DB)') \
	fi
	$(MAKE) -f $(SELF) run-migrations APP_ENV=test
### tests

### mysql
mysql-up: # $(MYSQL-DATA)
	cd $(WD)/devops/mysql/ \
		&& docker-compose up -d kalesville-mysql \
		&& docker logs -f mysql_kalesville-mysql_1

mysql-stop:
	cd $(WD)/devops/mysql/ && docker-compose stop

mysql-down:
	cd $(WD)/devops/mysql/ && docker-compose down

mysql-shell:
	docker run -it --rm \
		--link mysql_kalesville-mysql_1:mysql \
		--net mysql_default \
		mysql:5.7 \
		mysql -umysql -pmysql -Dkalesville-web -hmysql
### mysql

### docker compose
COMPOSE-FILES:=$(shell echo '-f devops/'{mysql,kalesville}/docker-compose.yml)
DOCKER-COMPOSE:=docker-compose $(COMPOSE-FILES)
OPTS:=
build-web:
	$(DOCKER-COMPOSE) build $(OPTS) web

build-web-dev:
	$(DOCKER-COMPOSE) build $(OPTS) --tag 'lloydkirk/kalesville:dev' web-dev

up:
	$(DOCKER-COMPOSE) up -d $(OPTS) web

up-dev:
	$(DOCKER-COMPOSE) up -d $(OPTS) web-dev

down:
	$(DOCKER-COMPOSE) down

restart:
	$(DOCKER-COMPOSE) restart
### docker compose
