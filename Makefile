### -=<(Kalesville)>=-
DEFAULT_GOAL: go-build
.PHONY: go-build clean run-dev

WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SHELL:=/bin/bash -eo pipefail

### run dev stack
run-dev: go-build down up wait migrate
	$(MAKE) logs O=-f

kalesville:=$(WD)/kalesville

go-build: $(kalesville)
$(kalesville):
	CGO_ENABLED=0 go build

clean:
	rm kalesville

### docker compose
DOCKER-COMPOSE:=docker-compose -f $(WD)/docker/compose/dev/docker-compose.yml
# (O)ption (S)ervice (C)ommand
O:=
S:=
C:=
build:
	$(DOCKER-COMPOSE) build $(O) $(S)

up:
	$(DOCKER-COMPOSE) up -d $(O) $(S)

pull:
	$(DOCKER-COMPOSE) pull $(O) $(S)

down:
	$(DOCKER-COMPOSE) down $(O) $(S)

restart:
	$(DOCKER-COMPOSE) restart $(O) $(S)

logs:
	$(DOCKER-COMPOSE) logs $(O) $(S)

exec:
	$(DOCKER-COMPOSE) exec $(O) $(S) $(C)

run:
	$(DOCKER-COMPOSE) run $(O) $(S) $(C)
### docker compose

### db
wait:
	sleep 9

migrate:
	@for f in migrations/*.sql; do \
		echo '### running' $$f ;\
		$(DOCKER-COMPOSE) exec db psql -U postgres -d kalesville-web -f $$f ;\
		echo '### done' $$f ;\
	done
### db

### release
RELEASE-INCREMENTS:=major minor patch

define release_template =
release-$(1):
	@ \
	set -x \
	git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	NEW_VERSION=$$(git describe | ./scripts/increment-version $(1)) ;\
	git checkout dev ;\
	sed -i -re"s/(.+image: .+:)[0-9]+\.[0-9]+\.[0-9]+/\1$$NEW_VERSION/g" \
		devops/{kalesville,nginx}/docker-compose.yml ;\
	git commit -m'Increment $(1) version of docker-compose.yml files by Makefile [ci skip]' \
		devops/{kalesville,nginx}/docker-compose.yml ;\
	git push ;\
	git checkout master ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment $(1) version by Makefile' $$NEW_VERSION ;\
	git push --tags ;\
	git checkout dev
endef

$(foreach increment,$(RELEASE-INCREMENTS),$(eval $(call release_template,$(increment))))
### release
