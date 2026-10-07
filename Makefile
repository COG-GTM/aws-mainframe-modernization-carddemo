# Entry points for the CardDemo modernization (see modernization/README.md).
JAVA_HOME ?= /usr/lib/jvm/java-21-openjdk-amd64
export JAVA_HOME
COMPOSE := docker compose -f modernization/docker-compose.yml
CARDDEMO_HTTP_PORT ?= 8080

.PHONY: help baseline baseline-fast baseline-check batch-equivalence verify up down health

help:
	@echo "baseline        compile all batch COBOL with GnuCOBOL and run the 26 baseline jobs (WAITSTEP waits 36 s)"
	@echo "baseline-fast   same, skipping the WAITSTEP sleep (outputs identical)"
	@echo "baseline-check  baseline-fast + assert 'jobs=26 compile failures=0' + no drift vs docs/validation/baseline (CI gate)"
	@echo "batch-equivalence  run READACCT/READCARD/READXREF/READCUST and POSTTRAN via the batch CLI (file + table DDs) and"
	@echo "                   compare with docs/validation/baseline (needs the packaged jar + CARDDEMO_DB_*; CI gate)"
	@echo "verify          mvn -B verify on JDK 21 (unit + Testcontainers ITs; needs Docker)"
	@echo "up / down       docker compose: PostgreSQL 16 + carddemo-app (needs CARDDEMO_DB_PASSWORD or modernization/.env)"
	@echo "health          curl /actuator/health on CARDDEMO_HTTP_PORT (default 8080)"

baseline:
	scripts/baseline/run_baseline.sh

baseline-fast:
	scripts/baseline/run_baseline.sh --fast

baseline-check:
	scripts/baseline/ci_check.sh

batch-equivalence:
	scripts/batch/run_print_jobs.sh file build/batch-equivalence/file
	scripts/batch/run_print_jobs.sh table build/batch-equivalence/table
	scripts/batch/run_posttran.sh file build/batch-equivalence/posttran-file
	scripts/batch/run_posttran.sh table build/batch-equivalence/posttran-table

verify:
	cd modernization && mvn -B verify

up:
	$(COMPOSE) up -d --build --wait

down:
	$(COMPOSE) down

health:
	curl -fsS http://localhost:$(CARDDEMO_HTTP_PORT)/actuator/health; echo
