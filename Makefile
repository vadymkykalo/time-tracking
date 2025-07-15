.PHONY: compile clean test docker-build docker-up docker-down docker-test

compile:
	rebar3 compile

clean:
	rebar3 clean

test:
	rebar3 eunit

docker-build:
	docker-compose build

docker-up:
	docker-compose up -d

docker-down:
	docker-compose down

docker-test:
	docker-compose -f docker-compose.test.yml up --build

shell:
	rebar3 shell

release:
	rebar3 release

all: compile test
