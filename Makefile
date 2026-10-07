# Entry points for the CardDemo modernization (see modernization/README.md).
JAVA_HOME ?= /usr/lib/jvm/java-21-openjdk-amd64
export JAVA_HOME
COMPOSE := docker compose -f modernization/docker-compose.yml
CARDDEMO_HTTP_PORT ?= 8080

.PHONY: help baseline baseline-fast baseline-check batch-equivalence nightly-cycle golden-set golden-set-check traceability traceability-check verify up down health

help:
	@echo "baseline        compile all batch COBOL with GnuCOBOL and run the 26 baseline jobs (WAITSTEP waits 36 s)"
	@echo "baseline-fast   same, skipping the WAITSTEP sleep (outputs identical)"
	@echo "baseline-check  baseline-fast + assert 'jobs=26 compile failures=0' + no drift vs docs/validation/baseline (CI gate)"
	@echo "batch-equivalence  run READACCT/READCARD/READXREF/READCUST, POSTTRAN, INTCALC, TRANBKP/COMBTRAN/TRANREPT/PRTCATBL"
	@echo "                   and CREASTMT one by one, then the whole nightly-cycle (one launch, chained outputs)"
	@echo "                   via the batch CLI (file + table DDs) and"
	@echo "                   compare with docs/validation/baseline (needs the packaged jar + CARDDEMO_DB_*; CI gate)"
	@echo "nightly-cycle   --job=nightly-cycle (file + table) from freshly loaded sample data, every job vs the baseline;"
	@echo "                job x mode x result matrix in build/batch-equivalence/nightly-cycle-*/REPORT.md (gate g-batch)"
	@echo "golden-set      online scenario (REST) + whole nightly cycle, Java vs GnuCOBOL from the same sample data;"
	@echo "                field-by-field + report compare, allow-list scripts/golden-set/expected-diffs (needs Docker,"
	@echo "                cobc, the packaged jar); reconciliation in docs/validation/golden-set/<date>/ (gate g-golden)"
	@echo "golden-set-check  golden-set into build/golden-set-doc + no drift vs the newest committed"
	@echo "                  docs/validation/golden-set/<date>/ (Toolchain line normalised); CI job golden-set"
	@echo "traceability    regenerate docs/modernization/07-traceability.md + traceability.json (COBOL/JCL/scheduler -> Java)"
	@echo "traceability-check  regenerate into a temp dir; fail on any diff or GAP (CI build job)"
	@echo "verify          mvn -B verify on JDK 21 (unit + Testcontainers ITs; needs Docker)"
	@echo "up / down       docker compose: PostgreSQL 16 + carddemo-app (needs CARDDEMO_DB_PASSWORD + CARDDEMO_JWT_SECRET or modernization/.env)"
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
	scripts/batch/run_intcalc.sh file build/batch-equivalence/intcalc-file
	scripts/batch/run_intcalc.sh table build/batch-equivalence/intcalc-table
	scripts/batch/run_tranrept.sh file build/batch-equivalence/tranrept-file
	scripts/batch/run_tranrept.sh table build/batch-equivalence/tranrept-table
	scripts/batch/run_creastmt.sh file build/batch-equivalence/creastmt-file
	scripts/batch/run_creastmt.sh table build/batch-equivalence/creastmt-table
	$(MAKE) nightly-cycle

nightly-cycle:
	@rc=0; \
	scripts/batch/run_nightly_cycle.sh file build/batch-equivalence/nightly-cycle-file || rc=1; \
	scripts/batch/run_nightly_cycle.sh table build/batch-equivalence/nightly-cycle-table || rc=1; \
	python3 scripts/batch/nightly_cycle_report.py --combine build/batch-equivalence/nightly-cycle-file \
	    build/batch-equivalence/nightly-cycle-table --report build/batch-equivalence/nightly-cycle-matrix/REPORT.md \
	    || rc=1; \
	exit $$rc

golden-set:
	scripts/golden-set/run_golden_set.sh

golden-set-check:
	scripts/golden-set/ci_check.sh

traceability:
	python3 scripts/traceability/build_traceability.py

traceability-check:
	python3 scripts/traceability/build_traceability.py --check

verify:
	cd modernization && mvn -B verify

up:
	$(COMPOSE) up -d --build --wait

down:
	$(COMPOSE) down

health:
	curl -fsS http://localhost:$(CARDDEMO_HTTP_PORT)/actuator/health; echo
