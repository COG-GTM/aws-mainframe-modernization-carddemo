JAVA_HOME ?= /usr/lib/jvm/java-21-openjdk-amd64
MVN := JAVA_HOME=$(JAVA_HOME) mvn -B
export JAVA_HOME

.PHONY: build test it db2-up db2-wait db2-sql up run down clean-db2

build:            ## host build of the jar (used by the Dockerfile)
	$(MVN) -DskipTests package

test:             ## unit + MockMvc tests
	$(MVN) verify

db2-up:           ## IBM Db2 Community in Docker with CardDemo DDL + seed data
	docker compose up -d db2
	$(MAKE) db2-wait

db2-wait:
	@echo "waiting for Db2 setup + CardDemo schema (first start takes a few minutes)..."
	@until [ "$$(docker inspect -f '{{.State.Health.Status}}' carddemo-db2)" = "healthy" ]; do sleep 10; done
	@echo "Db2 ready"

db2-sql:          ## make db2-sql Q="SELECT * FROM CARDDEMO.TRANSACTION_TYPE"
	docker exec carddemo-db2 su - db2inst1 -c "db2 connect to CARDDEMO >/dev/null && db2 \"$(Q)\""

it: db2-up        ## integration tests against the real Db2 container
	$(MVN) -Pdb2-it verify

run: db2-up build ## run the service on the host against Db2 (Swagger: http://localhost:8080/swagger-ui.html)
	$(JAVA_HOME)/bin/java -jar target/transaction-type-service-0.1.0.jar

up: build         ## db2 + service containers
	docker compose up -d --build
	$(MAKE) db2-wait

down:
	docker compose down

clean-db2:        ## drop the Db2 volume (schema is re-created on next start)
	docker compose down -v
