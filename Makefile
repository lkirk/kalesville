### -=<(Kalesville)>=-
WD:=$(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
SHELL:=/bin/bash -eo pipefail

DEFAULT_GOAL: go-build

go-build:
	CGO_ENABLED=0 go build

clean:
	rm kalesville

### docker compose
DOCKER-COMPOSE:=docker-compose -f $(WD)/docker/compose/dev/docker-compose.yml
O:=
S:=
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
### docker compose

### db
wait:
	sleep 5

migrate:
	@for f in migrations/*.sql; do \
		echo '### running' $$f ;\
		$(DOCKER-COMPOSE) exec db psql -U postgres -d kalesville-web -f $$f ;\
		echo '### done' $$f ;\
	done
### db

### release
release-patch:
	@ \
	set -x \
	git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	NEW_VERSION=$$(git describe | ./scripts/increment-version patch) ;\
	git checkout dev ;\
	sed -i -re"s/(.+image: .+:)[0-9]+\.[0-9]+\.[0-9]+/\1$$NEW_VERSION/g" devops/{kalesville,nginx}/docker-compose.yml ;\
	git commit -m'Increment patch version of docker-compose.yml files by Makefile [ci skip]' devops/{kalesville,nginx}/docker-compose.yml ;\
	git push ;\
	git checkout master ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment patch version by Makefile' $$NEW_VERSION ;\
	git push --tags ;\
	git checkout dev

release-minor:
	@ \
	set -x \
	git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	NEW_VERSION=$$(git describe | ./scripts/increment-version minor) ;\
	git checkout dev ;\
	sed -i -re"s/(.+image: .+:)[0-9]+\.[0-9]+\.[0-9]+/\1$$NEW_VERSION/g" devops/{kalesville,nginx}/docker-compose.yml ;\
	git commit -m'Increment minor version of docker-compose.yml files by Makefile [ci skip]' devops/{kalesville,nginx}/docker-compose.yml ;\
	git push ;\
	git checkout master ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment minor version by Makefile' $$NEW_VERSION ;\
	git push --tags ;\
	git checkout dev

release-major:
	@ \
	set -x \
	git checkout dev ;\
	git pull ;\
	git checkout master ;\
	git pull ;\
	NEW_VERSION=$$(git describe | ./scripts/increment-version major) ;\
	git checkout dev ;\
	sed -i -re"s/(.+image: .+:)[0-9]+\.[0-9]+\.[0-9]+/\1$$NEW_VERSION/g" devops/{kalesville,nginx}/docker-compose.yml ;\
	git commit -m'Increment major version of docker-compose.yml files by Makefile [ci skip]' devops/{kalesville,nginx}/docker-compose.yml ;\
	git push ;\
	git checkout master ;\
	git merge --no-ff -m'Merge dev into master by Makefile' dev ;\
	git tag -a -m'Increment major version by Makefile' $$NEW_VERSION ;\
	git push --tags ;\
	git checkout dev
### release
